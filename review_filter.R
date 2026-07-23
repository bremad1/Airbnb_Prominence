options(stringsAsFactors = FALSE)

.libPaths(c(file.path(getwd(), ".Rlib"), .libPaths()))

suppressPackageStartupMessages({
  library(dplyr)
  library(rdrobust)
})

args <- commandArgs(trailingOnly = TRUE)
output_file <- if (length(args) >= 1L) {
  args[[1L]]
} else {
  "results/review_threshold_5to40_ltm_first_trim_main_spec_balanced.csv"
}
load("RData/Entire.RData")
run_analysis <- function() {
  overall_start <- Sys.time()
  load("RData/Entire.RData")
  
  # Reuse the balanced data-preparation and RD functions without running its
  # estimation/output section.
  balanced_source <- readLines("balanced.R", warn = FALSE)
  function_end <- grep("^price_trims <-", balanced_source)[[1L]] - 1L
  eval(parse(text = balanced_source[seq_len(function_end)]),
       envir = environment())
  
  # NOTE: no "cell" argument here on purpose. cell is pushed to each worker's
  # global environment via clusterExport() below, and this function - having
  # environment(run_main_spec) <- .GlobalEnv set right after its definition -
  # looks it up there. If cell were captured as a function argument via an
  # anonymous wrapper at the call site instead, parLapplyLB would have to
  # serialize that wrapper's entire enclosing environment (including the
  # full Entire dataset and the growing results_list) to every worker on
  # every single call, which is what made each cell take ~100s.
  run_main_spec <- function(panel_name) {
    d <- cell
    if (panel_name == "A") d <- d %>% filter(ex_super == "t")
    if (panel_name == "B") d <- d %>% filter(ex_super == "f")
    d$date3_ym <- droplevels(d$date3_ym)
    
    run_start <- Sys.time()
    fit <- tryCatch(fit_one(d, 1L), error = function(e) e)
    run_time <- as.numeric(difftime(Sys.time(), run_start, units = "secs"))
    
    if (inherits(fit, "error")) {
      return(data.frame(
        panel = panel_name, raw_n = nrow(d), coef = NA_real_,
        se = NA_real_, p = NA_real_, h_left = NA_real_,
        h_right = NA_real_, obs_h = NA_integer_,
        error = conditionMessage(fit), run_time_sec = run_time
      ))
    }
    
    data.frame(
      panel = panel_name,
      raw_n = nrow(d),
      coef = fit$Estimate[[1L]],
      se = fit$se[[1L]],
      p = fit$pv[[1L]],
      h_left = fit$bws[1, 1],
      h_right = fit$bws[1, 2],
      obs_h = sum(fit$N_h),
      error = NA_character_,
      run_time_sec = run_time
    )
  }
  
  # Keep worker function serialization small; dependencies are exported
  # explicitly to each worker below.
  environment(diagnose_na) <- .GlobalEnv
  environment(fit_one) <- .GlobalEnv
  environment(run_main_spec) <- .GlobalEnv
  
  review_thresholds <- seq(0L, 50L, by = 5L)
  price_trims <- c(0, 0.01)
  change_trims <- c(0, 0.01)
  panels <- c("A", "B", "C")
  
  worker_count <- min(4L, parallel::detectCores(logical = FALSE))
  cl <- parallel::makeCluster(worker_count)
  on.exit(try(parallel::stopCluster(cl), silent = TRUE), add = TRUE)
  
  parallel::clusterCall(cl, function(paths) {
    .libPaths(paths)
    NULL
  }, .libPaths())
  parallel::clusterEvalQ(cl, {
    suppressPackageStartupMessages(library(dplyr))
    suppressPackageStartupMessages(library(rdrobust))
    NULL
  })
  parallel::clusterExport(
    cl,
    c("diagnose_na", "fit_one", "run_main_spec"),
    envir = environment()
  )
  
  results_list <- list()
  for (price_trim in price_trims) {
    message(sprintf(">> price_trim=%.2f 데이터 준비 중", price_trim))
    prep_start <- Sys.time()
    prepared <- prepare_entire(Entire, price_trim)
    prechange <- lapply(
      quarter_specs,
      function(spec) build_quarter_prechange(prepared, spec)
    )
    message(sprintf(
      "   준비 완료 (%.1f초)",
      as.numeric(difftime(Sys.time(), prep_start, units = "secs"))
    ))
    
    for (change_trim in change_trims) {
      cell_full <- lapply(
        prechange,
        apply_filters,
        change_pct = change_trim
      ) %>%
        bind_rows(.id = "quarter") %>%
        mutate(date3_ym = factor(format(as.Date(Date), "%Y-%m")))
      
      for (review_min in review_thresholds) {
        cell <- cell_full %>%
          filter(
            !is.na(first_month_number_of_reviews),
            first_month_number_of_reviews >= review_min
          )
        
        # cell is (re)exported to each worker's global environment here.
        # run_main_spec() looks it up from there, so parLapplyLB below can
        # be called with the bare function - no anonymous wrapper needed.
        parallel::clusterExport(cl, "cell", envir = environment())
        cell_start <- Sys.time()
        cell_results <- parallel::parLapplyLB(cl, panels, run_main_spec)
        cell_elapsed <- as.numeric(
          difftime(Sys.time(), cell_start, units = "secs")
        )
        
        for (i in seq_along(panels)) {
          res <- cell_results[[i]]
          status <- if (is.na(res$error)) {
            sprintf("coef=%.4f, se=%.4f, p=%.4f, N_h=%d",
                    res$coef, res$se, res$p, res$obs_h)
          } else {
            paste0("ERROR: ", res$error)
          }
          message(sprintf(
            "[price_trim=%.2f, change_trim=%.2f, review_min=%d, panel=%s] 완료 (%.1f초) - %s",
            price_trim, change_trim, review_min, panels[i],
            res$run_time_sec, status
          ))
        }
        message(sprintf(
          "   -> cell 전체 소요 (병렬, 3개 패널): %.1f초", cell_elapsed
        ))
        
        results_list[[length(results_list) + 1L]] <-
          bind_rows(cell_results) %>%
          mutate(
            review_min = review_min,
            price_trim = price_trim,
            change_trim = change_trim
          )
      }
    }
  }
  
  results <- bind_rows(results_list) %>%
    select(
      review_min, price_trim, change_trim, panel,
      everything()
    ) %>%
    arrange(
      review_min,
      change_trim,
      price_trim,
      match(panel, panels)
    )
  
  dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)
  write.csv(results, output_file, row.names = FALSE)
  
  overall_elapsed <- as.numeric(
    difftime(Sys.time(), overall_start, units = "secs")
  )
  message(
    "Completed estimates: ", nrow(results),
    "; errors: ", sum(!is.na(results$error))
  )
  message(sprintf(
    "Total run time: %.1f초 (%.2f분)",
    overall_elapsed, overall_elapsed / 60
  ))
  message("CSV output: ", normalizePath(output_file, winslash = "/"))
  invisible(results)
}
run_analysis()
