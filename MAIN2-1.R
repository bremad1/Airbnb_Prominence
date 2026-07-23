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
  "results/review_threshold_5to40_ltm_first_trim_main_spec.csv"
}

run_analysis <- function() {
  load("RData/Entire.RData")
  
  # Reuse the data-preparation and RD functions from MAIN2 without running its
  # estimation/output section.
  main2_source <- readLines("MAIN2.R", warn = FALSE)
  function_end <- grep("^price_trims <-", main2_source)[[1L]] - 1L
  eval(parse(text = main2_source[seq_len(function_end)]),
       envir = environment())
  
  run_main_spec <- function(panel_name, cell) {
    d <- cell
    if (panel_name == "A") d <- d %>% filter(ex_super == "t")
    if (panel_name == "B") d <- d %>% filter(ex_super == "f")
    d$date3_ym <- droplevels(d$date3_ym)
    
    fit <- tryCatch(fit_one(d, 1L), error = function(e) e)
    if (inherits(fit, "error")) {
      return(data.frame(
        panel = panel_name, raw_n = nrow(d), coef = NA_real_,
        se = NA_real_, p = NA_real_, h_left = NA_real_,
        h_right = NA_real_, obs_h = NA_integer_,
        error = conditionMessage(fit)
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
      error = NA_character_
    )
  }
  
  # Keep worker function serialization small; dependencies are exported
  # explicitly to each worker below.
  environment(make_covariates) <- .GlobalEnv
  environment(fit_one) <- .GlobalEnv
  environment(run_main_spec) <- .GlobalEnv
  
  review_thresholds <- seq(5L, 40L, by = 5L)
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
    c("make_covariates", "fit_one", "run_main_spec"),
    envir = environment()
  )
  
  results_list <- list()
  for (review_min in review_thresholds) {
    # Apply both sample restrictions before calculating either trim cutoff.
    base <- Entire %>%
      filter(
        !is.na(first_month_number_of_reviews),
        first_month_number_of_reviews >= review_min,
        !is.na(first_month_ltm),
        first_month_ltm >= 1
      )
    
    for (price_trim in price_trims) {
      prepared <- prepare_entire(base, price_trim)
      prechange <- lapply(
        quarter_specs,
        function(spec) build_quarter_prechange(prepared, spec)
      )
      
      for (change_trim in change_trims) {
        cell <- lapply(
          prechange,
          apply_filters,
          change_pct = change_trim
        ) %>%
          bind_rows(.id = "quarter") %>%
          mutate(date3_ym = factor(format(as.Date(Date), "%Y-%m")))
        
        parallel::clusterExport(cl, "cell", envir = environment())
        cell_results <- parallel::parLapplyLB(
          cl,
          panels,
          function(panel_name) run_main_spec(panel_name, cell)
        )
        
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
  
  message(
    "Completed estimates: ", nrow(results),
    "; errors: ", sum(!is.na(results$error))
  )
  message("CSV output: ", normalizePath(output_file, winslash = "/"))
  invisible(results)
}

run_analysis()
