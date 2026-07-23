load("RData/Entire.RData")

options(stringsAsFactors = FALSE)

.libPaths(c(
  file.path(getwd(), ".Rlib"),
  .libPaths()
))

suppressPackageStartupMessages({
  library(dplyr)
  library(rdrobust)
})


args <- commandArgs(trailingOnly = TRUE)
tex_file <- if (length(args) >= 1L) args[[1L]] else
  "MAIN2_ltm_first_results_balanced.tex"
tex_file <- normalizePath(tex_file, winslash = "/", mustWork = FALSE)
dir.create(dirname(tex_file), recursive = TRUE, showWarnings = FALSE)

quarter_specs <- list(
  Q323 = list(valid = c(5, 6), analysis = 4:9, year = 2023L,
              target = c(8, 9), cross = FALSE),
  Q423 = list(valid = c(8, 9), analysis = 7:12, year = 2023L,
              target = c(11, 12), cross = FALSE),
  Q124 = list(valid = c(11, 12), analysis = c(10, 11, 12, 1, 2, 3),
              year = 2024L, target = c(2, 3), cross = TRUE),
  Q224 = list(valid = c(2, 3), analysis = 1:6, year = 2024L,
              target = c(5, 6), cross = FALSE),
  Q324 = list(valid = c(5, 6), analysis = 4:9, year = 2024L,
              target = c(8, 9), cross = FALSE),
  Q424 = list(valid = c(8, 9), analysis = 7:12, year = 2024L,
              target = c(11, 12), cross = FALSE)
)

strict_trim <- function(x, pct) {
  if (pct == 0) return(!is.na(x))
  q <- quantile(x, c(pct, 1 - pct), na.rm = TRUE, names = FALSE)
  !is.na(x) & x > q[[1L]] & x < q[[2L]]
}

prepare_entire <- function(data, price_pct) {
  x <- data %>%
    mutate(
      .year = as.integer(format(as.Date(Date), "%Y")),
      .month = as.integer(format(as.Date(Date), "%m")),
      .year_month = format(as.Date(Date), "%Y-%m"),
      .last_review_date = as.Date(last_review)
    ) %>%
    # Restrict the analysis population before calculating trim cutoffs.
    filter(!is.na(first_month_ltm), first_month_ltm >= 1)
  
  if (price_pct > 0) {
    x <- x %>%
      group_by(.year_month) %>%
      filter(strict_trim(price, price_pct)) %>%
      ungroup()
  }
  
  x %>%
    arrange(id, Date) %>%
    group_by(id) %>%
    mutate(
      date_diff = 12L * (.year - lag(.year)) + (.month - lag(.month)),
      ex_price1 = if_else(date_diff == 1L, lag(price, 1), NA_real_),
      ex_price2 = if_else(
        date_diff == 2L, lag(price, 1),
        if_else(date_diff == 1L, lag(price, 2), NA_real_)
      ),
      ex_price3 = if_else(
        date_diff == 3L, lag(price, 1),
        if_else(date_diff == 2L, lag(price, 2),
                if_else(date_diff == 1L, lag(price, 3), NA_real_))
      ),
      ex_price4 = if_else(
        date_diff == 4L, lag(price, 1),
        if_else(date_diff == 3L, lag(price, 2),
                if_else(date_diff == 2L, lag(price, 3),
                        if_else(date_diff == 1L, lag(price, 4), NA_real_)))
      ),
      prior_snapshot_date = if_else(
        date_diff == 2L, lag(as.Date(Date), 1),
        if_else(date_diff == 1L, lag(as.Date(Date), 2), as.Date(NA))
      ),
      prior_last_review = if_else(
        date_diff == 2L, lag(.last_review_date, 1),
        if_else(date_diff == 1L, lag(.last_review_date, 2), as.Date(NA))
      )
    ) %>%
    ungroup()
}

build_quarter_prechange <- function(data, spec) {
  no_dup <- data %>%
    group_by(id) %>%
    filter(n_distinct(host_id) == 1L) %>%
    ungroup()
  
  if (!spec$cross) {
    valid_ids <- no_dup %>%
      filter(.year == spec$year, .month %in% spec$valid) %>%
      filter(!is.na(price), host_is_superhost != "",
             !is.na(host_is_superhost)) %>%
      group_by(host_id) %>%
      filter(n_distinct(host_is_superhost) == 1L) %>%
      distinct(id) %>%
      pull(id)
    
    temp <- no_dup %>%
      filter(.year == spec$year, .month %in% spec$analysis) %>%
      filter(host_is_superhost != "", !is.na(host_is_superhost),
             !is.na(price)) %>%
      group_by(id) %>%
      # This is the balanced branch: an id is kept only if it is observed
      # in every month of the analysis window.
      filter(all(spec$analysis %in% .month)) %>%
      ungroup() %>%
      filter(.year == spec$year, .month %in% spec$target)
  } else {
    valid_ids <- no_dup %>%
      filter(.year == spec$year - 1L, .month %in% spec$valid,
             !is.na(price), host_is_superhost != "",
             !is.na(host_is_superhost)) %>%
      group_by(host_id) %>%
      filter(n_distinct(host_is_superhost) == 1L) %>%
      distinct(id) %>%
      pull(id)
    
    temp <- no_dup %>%
      filter(
        (.year == spec$year - 1L & .month %in% spec$analysis[1:3]) |
          (.year == spec$year & .month %in% spec$analysis[4:6])
      ) %>%
      filter(
        host_is_superhost != "",
        !is.na(host_is_superhost),
        !is.na(price)
      ) %>%
      group_by(id) %>%
      # This is the balanced branch: an id is kept only if it is observed
      # in every month of the six-month analysis window (three months in
      # spec$year - 1 and three months in spec$year). The initial filter
      # above already restricts rows to those exact (year, month)
      # combinations, so checking .month membership alone is unambiguous
      # here - the analysis months never repeat across the two years.
      filter(all(spec$analysis %in% .month)) %>%
      ungroup() %>%
      filter(.year == spec$year, .month %in% spec$target)
  }
  
  temp %>%
    filter(id %in% valid_ids) %>%
    group_by(host_id) %>%
    filter(n_distinct(host_is_superhost) == 1L) %>%
    ungroup() %>%
    group_by(id) %>%
    mutate(avg_price = ifelse(
      sum(!is.na(price)) > 1L, mean(price, na.rm = TRUE),
      price[!is.na(price)]
    )) %>%
    slice(1L) %>%
    ungroup() %>%
    mutate(
      ex_avg = ifelse(
        .month %in% c(2, 5, 8, 11),
        rowMeans(cbind(ex_price2, ex_price3), na.rm = TRUE),
        ifelse(
          .month %in% c(1, 4, 7, 10),
          rowMeans(cbind(ex_price1, ex_price2), na.rm = TRUE),
          rowMeans(cbind(ex_price3, ex_price4), na.rm = TRUE)
        )
      ),
      raw_change = (avg_price - ex_avg) / ex_avg,
      price_diff = log(avg_price) - log(ex_avg)
    )
}

apply_filters <- function(data, change_pct) {
  x <- if (change_pct == 0) {
    data %>% filter(is.finite(raw_change))
  } else {
    data %>% filter(strict_trim(raw_change, change_pct))
  }
  
  # first_month_ltm >= 1 was already imposed before all trimming.
  x
}

add_price_quartiles <- function(d) {
  q <- quantile(d$ex_avg, c(.25, .50, .75), na.rm = TRUE, names = FALSE)
  d %>% mutate(
    ex_q1 = as.integer(ex_avg < q[[1L]]),
    ex_q2 = as.integer(ex_avg >= q[[1L]] & ex_avg < q[[2L]]),
    ex_q3 = as.integer(ex_avg >= q[[2L]] & ex_avg < q[[3L]]),
    ex_q4 = as.integer(ex_avg >= q[[3L]])
  )
}

condition_names <- c("FULL", "Q1Q2", "Q2Q3", "Q3Q4")
condition_index <- function(d, condition) {
  switch(condition,
         FULL = rep(TRUE, nrow(d)),
         Q1Q2 = d$ex_q1 == 1 | d$ex_q2 == 1,
         Q2Q3 = d$ex_q2 == 1 | d$ex_q3 == 1,
         Q3Q4 = d$ex_q3 == 1 | d$ex_q4 == 1
  )
}

diagnose_na <- function(d) {
  # Rough diagnostics for the case where rdrobust() returns NA point
  # estimates WITHOUT throwing an R error or warning. This does not change
  # the fit; it only explains the NA after the fact.
  msgs <- character(0)
  
  if (is.null(d$running_scr)) {
    msgs <- c(msgs, "running_scr not found in data (NULL) - check column name")
  }
  if (is.null(d$host_is_superhost2)) {
    msgs <- c(msgs, "host_is_superhost2 not found in data (NULL) - check column name")
  }
  
  if (!is.null(d$running_scr)) {
    x <- d$running_scr - 4.75
    near <- !is.na(x) & abs(x) < 0.5
    if (sum(near) < 10L) {
      msgs <- c(msgs, sprintf(
        "only %d obs within +-0.5 of cutoff - bandwidth may be degenerate",
        sum(near)
      ))
    }
    if (!is.null(d$host_is_superhost2) && sum(near) > 0L) {
      nvar <- length(unique(d$host_is_superhost2[near]))
      if (nvar < 2L) {
        msgs <- c(msgs, sprintf(
          "no variation in host_is_superhost2 near cutoff (n_unique=%d) - fuzzy ratio is 0/0",
          nvar
        ))
      }
    }
  }
  
  if (length(msgs) == 0L) {
    msgs <- "Estimate is NA but no obvious cause detected - inspect this cell manually"
  }
  paste(msgs, collapse = "; ")
}

fit_one <- function(d, specification, first_fit = NULL) {
  rd_args <- list(
    y = d$price_diff,
    x = d$running_scr - 4.75,
    fuzzy = d$host_is_superhost2,
    #cluster = d$host_id,
    kernel = "tri",
    bwselect = "msetwo",
    p = 1,
    masspoints = "off",
    bwrestrict = TRUE
  )
  # The project's .Rlib (loaded first via .libPaths() above) may contain an
  # older rdrobust build that predates the "all" argument. Only pass it
  # when the installed function actually accepts it, so this doesn't error
  # out with "unused argument (all = TRUE)" on older installs.
  if ("all" %in% names(formals(rdrobust))) rd_args$all <- TRUE
  
  if (specification <= 5L) {
    # Time (year-month) fixed effects, built the same way each time:
    # one dummy per date3_ym level, first level dropped as the reference.
    dummy_vars <- as.data.frame(model.matrix(~ date3_ym - 1, data = d))
    if (ncol(dummy_vars) > 1L) {
      dummy_vars <- dummy_vars[, -1, drop = FALSE]
      rd_args$covs <- cbind(dummy_vars)
    }
    # If d$date3_ym has only one level (e.g. a very narrow subset), there is
    # nothing left to control for after dropping the reference level, so
    # rd_args$covs is simply left unset for this cell.
  }
  if (specification == 2L) {
    rd_args$h <- 2 * first_fit$bws[1, 1:2]
  } else if (specification == 3L) {
    rd_args$h <- c(.2, .1)
  } else if (specification == 4L) {
    rd_args$h <- c(.3, .15)
  } else if (specification == 5L) {
    rd_args$h <- c(.4, .2)
  }
  
  # tryCatch(error=...) alone will NOT catch cases where rdrobust() emits a
  # warning and still returns an object with NA point estimates. Collect
  # warnings here so they end up visible in the results instead of being
  # silently discarded.
  warn_msgs <- character(0)
  fit <- withCallingHandlers(
    do.call(rdrobust, rd_args),
    warning = function(w) {
      warn_msgs <<- c(warn_msgs, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  
  if (length(warn_msgs) > 0L) {
    attr(fit, "fit_warning") <- paste(unique(warn_msgs), collapse = " | ")
  } else if (is.na(fit$Estimate[[1L]])) {
    attr(fit, "fit_warning") <- diagnose_na(d)
  }
  fit
}

# NOTE: no "cell" argument here on purpose. cell is pushed to each worker's
# global environment via clusterExport() below (see run_all_regressions()),
# and this function - having environment(run_task) <- .GlobalEnv set right
# after its definition - looks it up there. This avoids the anonymous
# wrapper at the call site (function(i) run_task(tasks[i,,drop=FALSE], cell))
# that used to capture run_all_regressions()'s entire local environment
# (including the full Entire dataset) on every parLapplyLB call.
run_task <- function(task) {
  d <- cell
  if (task$panel == "A") d <- d %>% filter(ex_super == "t")
  if (task$panel == "B") d <- d %>% filter(ex_super == "f")
  d <- d[condition_index(d, task$condition), , drop = FALSE]
  d$date3_ym <- droplevels(d$date3_ym)
  
  first_fit <- NULL
  fits <- vector("list", 6L)
  run_times <- numeric(6L)
  
  for (specification in 1:6) {
    run_start <- Sys.time()
    fit <- if (specification == 1L) {
      tryCatch(fit_one(d, 1L), error = function(e) e)
    } else if (inherits(first_fit, "error")) {
      first_fit
    } else {
      tryCatch(fit_one(d, specification, first_fit), error = function(e) e)
    }
    run_times[[specification]] <-
      as.numeric(difftime(Sys.time(), run_start, units = "secs"))
    fits[[specification]] <- fit
    if (specification == 1L) first_fit <- fit
  }
  
  bind_rows(lapply(seq_along(fits), function(specification) {
    fit <- fits[[specification]]
    if (inherits(fit, "error")) {
      data.frame(
        specification = specification, raw_n = nrow(d),
        coef_conv = NA_real_, se_conv = NA_real_, p_conv = NA_real_,
        h_left = NA_real_, h_right = NA_real_, obs_h = NA_integer_,
        error = conditionMessage(fit), warning = NA_character_,
        run_time_sec = run_times[[specification]]
      )
    } else {
      fw <- attr(fit, "fit_warning")
      data.frame(
        specification = specification, raw_n = nrow(d),
        coef_conv = fit$Estimate[[1L]], se_conv = fit$se[[1L]],
        p_conv = fit$pv[[1L]], h_left = fit$bws[1, 1],
        h_right = fit$bws[1, 2], obs_h = sum(fit$N_h),
        error = NA_character_,
        warning = if (is.null(fw)) NA_character_ else fw,
        run_time_sec = run_times[[specification]]
      )
    }
  })) %>% mutate(panel = task$panel, condition = task$condition)
}

price_trims <- c(0, .01)
change_trims <- c(0, .01)
tasks <- expand.grid(
  panel = c("A", "B", "C"),
  condition = condition_names,
  stringsAsFactors = FALSE
)

run_all_regressions <- function() {
  overall_start <- Sys.time()
  
  detected_cores <- parallel::detectCores(logical = FALSE)
  if (is.na(detected_cores) || detected_cores < 1L) detected_cores <- 1L
  worker_count <- min(4L, detected_cores)
  
  cl <- parallel::makeCluster(worker_count)
  on.exit(parallel::stopCluster(cl), add = TRUE)
  
  parallel::clusterCall(cl, function(paths) {
    .libPaths(paths)
    invisible(NULL)
  }, .libPaths())
  
  parallel::clusterEvalQ(cl, {
    suppressPackageStartupMessages(library(dplyr))
    suppressPackageStartupMessages(library(rdrobust))
    NULL
  })
  
  parallel::clusterExport(
    cl,
    c("condition_index", "diagnose_na", "fit_one", "run_task"),
    envir = environment()
  )
  
  results_list <- list()
  
  for (price_trim in price_trims) {
    message("Preparing monthly data: price trim=", 100 * price_trim, "%")
    prepared <- prepare_entire(Entire, price_trim)
    prechange <- lapply(
      quarter_specs,
      function(spec) build_quarter_prechange(prepared, spec)
    )
    
    for (change_trim in change_trims) {
      message(
        "Running: price trim=", 100 * price_trim,
        "%, change trim=", 100 * change_trim, "%"
      )
      
      # cell is built exactly once per (price_trim, change_trim) combination
      # here - NOT once per task. The 24 tasks (panel x condition) below all
      # share this same cell.
      cell <- lapply(prechange, apply_filters, change_pct = change_trim) %>%
        lapply(add_price_quartiles) %>%
        bind_rows(.id = "quarter") %>%
        mutate(date3_ym = factor(format(as.Date(Date), "%Y-%m")))
      
      parallel::clusterExport(
        cl,
        c("cell", "tasks"),
        envir = environment()
      )
      
      cell_start <- Sys.time()
      # No anonymous wrapper capturing cell here - run_task() looks up cell
      # from the worker's global environment (just exported above).
      cell_results <- parallel::parLapplyLB(
        cl,
        seq_len(nrow(tasks)),
        function(i) run_task(tasks[i, , drop = FALSE])
      )
      cell_elapsed <- as.numeric(
        difftime(Sys.time(), cell_start, units = "secs")
      )
      
      for (i in seq_len(nrow(tasks))) {
        res <- cell_results[[i]]
        for (j in seq_len(nrow(res))) {
          status <- if (is.na(res$error[[j]])) {
            sprintf("coef=%.4f, se=%.4f, p=%.4f, N_h=%d",
                    res$coef_conv[[j]], res$se_conv[[j]],
                    res$p_conv[[j]], res$obs_h[[j]])
          } else {
            paste0("ERROR: ", res$error[[j]])
          }
          message(sprintf(
            "[price_trim=%.2f, change_trim=%.2f, panel=%s, condition=%s, spec=%d] 완료 (%.1f초) - %s",
            price_trim, change_trim, res$panel[[j]], res$condition[[j]],
            res$specification[[j]], res$run_time_sec[[j]], status
          ))
        }
      }
      message(sprintf(
        "   -> (price_trim=%.2f, change_trim=%.2f) 전체 %d개 태스크(x6 spec) 소요 (병렬 %d워커): %.1f초",
        price_trim, change_trim, nrow(tasks), worker_count, cell_elapsed
      ))
      
      cell_result <- bind_rows(cell_results) %>%
        mutate(price_trim = price_trim, change_trim = change_trim)
      
      results_list[[length(results_list) + 1L]] <- cell_result
    }
  }
  
  overall_elapsed <- as.numeric(
    difftime(Sys.time(), overall_start, units = "secs")
  )
  message(sprintf(
    "run_all_regressions 총 소요: %.1f초 (%.2f분)",
    overall_elapsed, overall_elapsed / 60
  ))
  
  bind_rows(results_list) %>%
    arrange(
      price_trim,
      change_trim,
      match(panel, c("A", "B", "C")),
      match(condition, condition_names),
      specification
    )
}

# environment(...) <- .GlobalEnv pins each worker-side function's enclosing
# environment to the top-level global environment (rather than wherever it
# happened to be defined), so parLapplyLB never needs to serialize anything
# beyond what clusterExport() explicitly sent.
environment(condition_index) <- .GlobalEnv
environment(diagnose_na) <- .GlobalEnv
environment(fit_one) <- .GlobalEnv
environment(run_task) <- .GlobalEnv

results <- run_all_regressions()
stopifnot(nrow(results) == 4L * 3L * 4L * 6L)
if (any(!is.na(results$error))) {
  failed <- unique(results$error[!is.na(results$error)])
  stop("Regression error(s): ", paste(failed, collapse = " | "))
}

# NEW: rdrobust can return NA point estimates without raising an R error.
# Surface those cases now instead of letting them show up silently as NA
# in the LaTeX table.
na_cells <- results[is.na(results$coef_conv), , drop = FALSE]
if (nrow(na_cells) > 0L) {
  message(
    "NOTE: ", nrow(na_cells), " of ", nrow(results),
    " cells have NA coef_conv despite no R error."
  )
  print(unique(na_cells[, c("panel", "condition", "specification", "warning")]))
}
saveRDS(results, sub("\\.tex$", "_results.rds", tex_file,
                     ignore.case = TRUE))

stars <- function(p) {
  ifelse(p < .01, "***", ifelse(p < .05, "**", ifelse(p < .10, "*", "")))
}
trim_code <- function(x) if (x == 0) "0" else "1"
newline <- " \\\\"

make_overview <- function(results) {
  d <- results %>% filter(condition == "FULL", specification == 1L)
  lines <- c(
    "\\subsection{Overview}",
    "\\begin{table}[!htbp]", "\\centering",
    "\\caption{FULL results: LTM restriction applied before trimming}",
    "\\label{tab:ltm1:panels_abc_trim_grid_full_bal}", "\\small",
    "\\begin{tabular}{rrlrr}", "\\toprule",
    paste0("Price trim & Change trim & Panel & $N$ & Conventional", newline),
    "\\midrule"
  )
  for (i in seq_len(nrow(d))) {
    lines <- c(lines, paste0(
      round(100 * d$price_trim[[i]]), "\\% & ",
      round(100 * d$change_trim[[i]]), "\\% & ", d$panel[[i]], " & ",
      format(d$raw_n[[i]], big.mark = ","), " & $",
      sprintf("%.3f%s", d$coef_conv[[i]], stars(d$p_conv[[i]])),
      "$ (", sprintf("%.3f", d$se_conv[[i]]), ")", newline
    ))
  }
  c(lines, "\\bottomrule", "\\end{tabular}", "\\end{table}")
}

make_detail_table <- function(d, price_trim, change_trim) {
  pc <- trim_code(price_trim)
  cc <- trim_code(change_trim)
  lines <- c(
    sprintf("\\subsection{(%d\\%%,%d\\%%)}",
            round(100 * price_trim), round(100 * change_trim)),
    "\\begin{table}[!htbp]", "\\centering",
    sprintf(paste0("\\caption{price trim %d\\%%, change trim %d\\%%, ",
                   "\\texttt{first\\_month\\_ltm $\\geq 1$}: Conventional}"),
            round(100 * price_trim), round(100 * change_trim)),
    sprintf("\\label{tab:abc:trim%s%s:ltm1:conventional:bal}", pc, cc),
    "\\small", "\\begin{tabular}{lcccccc}", "\\toprule",
    paste0("Condition & (1) & (2) & (3) & (4) & (5) & (6)", newline),
    "\\midrule"
  )
  
  for (panel_name in c("A", "B", "C")) {
    panel_data <- d[d$panel == panel_name, , drop = FALSE]
    lines <- c(lines,
               sprintf("\\textbf{Panel %s} &&&&&& %s", panel_name, newline),
               "")
    for (condition in condition_names) {
      x <- panel_data[panel_data$condition == condition, , drop = FALSE]
      stopifnot(nrow(x) == 6L)
      coef_cells <- sprintf("%.3f%s", x$coef_conv, stars(x$p_conv))
      se_cells <- sprintf("(%.3f)", x$se_conv)
      lines <- c(
        lines,
        paste0(paste(c(condition, coef_cells), collapse = " & "), newline),
        paste0(paste(c("", se_cells), collapse = " & "), newline)
      )
    }
    full <- panel_data[panel_data$condition == "FULL", , drop = FALSE]
    lines <- c(
      lines, "",
      paste0(paste(c("\\textit{obs}",
                     format(full$raw_n, big.mark = ",")),
                   collapse = " & "), newline),
      paste0(paste(c("\\textit{obs\\_h}",
                     format(full$obs_h, big.mark = ",")),
                   collapse = " & "), newline),
      ""
    )
  }
  
  c(
    lines, "\\bottomrule", "\\end{tabular}",
    "\\begin{minipage}{0.98\\textwidth}\\footnotesize",
    paste0(
      "Notes: The sample is restricted to \\texttt{first\\_month\\_ltm} ",
      "$\\geq 1$ before price and change trimming. Panel A is defined by \\texttt{ex\\_super==\"t\"}; ",
      "Panel B by \\texttt{ex\\_super==\"f\"}; Panel C imposes no ",
      "\\texttt{ex\\_super} restriction. Column (1) uses side-specific ",
      "MSE-optimal bandwidths with time fixed effects; column (2) doubles ",
      "those bandwidths; columns (3)--(5) use bandwidths (0.2,0.1), ",
      "(0.3,0.15), and (0.4,0.2); column (6) omits time fixed effects. ",
      "Standard errors are clustered by listing ID. Both observation rows ",
      "correspond to FULL only: \\textit{obs} is the raw sample size and ",
      "\\textit{obs\\_h} is the number of observations inside the RD bandwidth."
    ),
    "\\end{minipage}", "\\end{table}", "\\clearpage"
  )
}

tex_lines <- c(
  "\\section{\\texttt{first\\_month\\_ltm} $\\geq 1$ before trimming}",
  make_overview(results)
)
for (price_trim in price_trims) {
  for (change_trim in change_trims) {
    d <- results[
      results$price_trim == price_trim &
        results$change_trim == change_trim, , drop = FALSE
    ]
    tex_lines <- c(tex_lines, "", make_detail_table(
      d, price_trim, change_trim
    ))
  }
}

cat(tex_lines, file = tex_file, sep = "\n")
message("Completed estimates: ", nrow(results), "; errors: 0")
message("LaTeX fragment: ", tex_file)
tex_lines