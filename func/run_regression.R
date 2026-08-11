REGRESSION_PROJECT_DIR <- if (file.exists(file.path(
  getwd(), "func", "run_regression.R"
))) {
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
} else if (file.exists(file.path(
  getwd(), "Final2", "func", "run_regression.R"
))) {
  normalizePath(file.path(getwd(), "Final2"), winslash = "/", mustWork = TRUE)
} else {
  stop("Run run_regression() from the project root or Final2 directory.")
}
FINAL_RESULTS_DIR <- file.path(REGRESSION_PROJECT_DIR, "results")

source(file.path(
  REGRESSION_PROJECT_DIR,
  "func",
  "balanced_3month_data.R"
))
REGRESSION_HELPERS <- load_balanced_3month_helpers()
prepare_runrd_sample <- REGRESSION_HELPERS$prepare_runrd_sample
runrd_subpanel_conditions <- REGRESSION_HELPERS$runrd_subpanel_conditions

# ---------------------------------------------------------------------------
# FINAL BALANCED THREE-MONTH REVIEW-THRESHOLD TABLES
# ---------------------------------------------------------------------------
# Sample:
#   - trim (1,0): 1% avg_price trim, no price_diff trim
#   - FULL price subpanel
#   - Panels A (ex_super == "t") and B (ex_super == "f")
#   - first_month_number_of_reviews >= each threshold below
#
# Estimation:
#   - fuzzy local-linear RD, triangular kernel
#   - bwselect = "msetwo", masspoints = "adjust"
#   - specifications 1--5 include quarter fixed effects
#   - specification 6 excludes quarter fixed effects
#   - separate outputs for listing-ID, host-ID, and quarter clustering
# ---------------------------------------------------------------------------

# Change only this vector if a different threshold grid is required.
REVIEW_THRESHOLDS <- seq(0L, 50L, by = 5L)

TRIM_AVG_PRICE <- 0.01
TRIM_PRICE_DIFF <- 0
CLUSTER_TYPES <- c("id", "host_id", "quarter")
RESULTS_RDS <-
  file.path(
    FINAL_RESULTS_DIR,
    "balanced_3month_final_review_threshold_results.rds"
  )
SUBPANEL_RESULTS_RDS <-
  file.path(
    FINAL_RESULTS_DIR,
    "balanced_3month_final_subpanel_results.rds"
  )

cluster_label <- function(cluster_type) {
  switch(
    cluster_type,
    id = "Listing-ID",
    host_id = "Host-ID",
    quarter = "Quarter",
    stop("Unknown cluster type: ", cluster_type)
  )
}

cluster_file_label <- function(cluster_type) {
  switch(
    cluster_type,
    id = "listing",
    host_id = "host",
    quarter = "quarter",
    stop("Unknown cluster type: ", cluster_type)
  )
}

run_one_final_spec <- function(
    data,
    cluster_type,
    h = NULL,
    include_quarter_fe = TRUE
) {
  rd_args <- list(
    y = data$price_diff,
    x = data$running_scr - 4.75,
    fuzzy = data$host_is_superhost2,
    kernel = "tri",
    bwselect = "msetwo",
    p = 1,
    masspoints = "adjust",
    bwrestrict = TRUE
  )
  if ("all" %in% names(formals(rdrobust))) rd_args$all <- TRUE
  if (!is.null(h)) rd_args$h <- h

  # CLUSTERING DEFINITIONS
  if (cluster_type == "id") {
    rd_args$cluster <- data$id
  } else if (cluster_type == "host_id") {
    rd_args$cluster <- data$host_id
  } else if (cluster_type == "quarter") {
    rd_args$cluster <- as.integer(factor(data$quarter))
  } else {
    stop("Unknown cluster type: ", cluster_type)
  }

  # TIME FIXED EFFECTS: five quarter dummies with one omitted reference.
  if (include_quarter_fe) {
    quarter_dummies <- as.data.frame(
      model.matrix(~ quarter - 1, data = data)
    )
    if (ncol(quarter_dummies) > 1L) {
      rd_args$covs <- as.matrix(
        quarter_dummies[, -1L, drop = FALSE]
      )
    }
  }

  do.call(rdrobust, rd_args)
}

empty_result <- function(
    panel,
    specification,
    raw_n,
    error
) {
  data.frame(
    panel = panel,
    specification = specification,
    n_full = raw_n,
    coef_conventional = NA_real_,
    se_conventional = NA_real_,
    p_conventional = NA_real_,
    coef_bc = NA_real_,
    se_robust = NA_real_,
    p_robust = NA_real_,
    n_h = NA_integer_,
    error = error
  )
}

extract_final_result <- function(
    fit,
    panel,
    specification,
    raw_n
) {
  if (inherits(fit, "error")) {
    return(empty_result(
      panel,
      specification,
      raw_n,
      conditionMessage(fit)
    ))
  }

  data.frame(
    panel = panel,
    specification = specification,
    n_full = raw_n,
    coef_conventional = as.numeric(fit$Estimate[[1L]]),
    se_conventional = as.numeric(fit$se[[1L]]),
    p_conventional = as.numeric(fit$pv[[1L]]),
    coef_bc = as.numeric(fit$Estimate[[2L]]),
    se_robust = as.numeric(fit$se[[3L]]),
    p_robust = as.numeric(fit$pv[[3L]]),
    n_h = as.integer(sum(fit$N_h)),
    error = NA_character_
  )
}

run_final_six <- function(data, panel, cluster_type) {
  fits <- vector("list", 6L)

  # (1) MSE-optimal side-specific bandwidths with quarter FE.
  fits[[1L]] <- tryCatch(
    run_one_final_spec(
      data,
      cluster_type,
      include_quarter_fe = TRUE
    ),
    error = function(e) e
  )

  # (2) Twice specification (1)'s estimation bandwidths with quarter FE.
  fits[[2L]] <- if (inherits(fits[[1L]], "error")) {
    simpleError("Spec 1 failed; doubled bandwidth unavailable.")
  } else {
    tryCatch(
      run_one_final_spec(
        data,
        cluster_type,
        h = 2 * fits[[1L]]$bws[1L, ],
        include_quarter_fe = TRUE
      ),
      error = function(e) e
    )
  }

  # (3)--(5) Fixed asymmetric bandwidths with quarter FE.
  fits[[3L]] <- tryCatch(
    run_one_final_spec(
      data,
      cluster_type,
      h = c(0.20, 0.10),
      include_quarter_fe = TRUE
    ),
    error = function(e) e
  )
  fits[[4L]] <- tryCatch(
    run_one_final_spec(
      data,
      cluster_type,
      h = c(0.30, 0.15),
      include_quarter_fe = TRUE
    ),
    error = function(e) e
  )
  fits[[5L]] <- tryCatch(
    run_one_final_spec(
      data,
      cluster_type,
      h = c(0.40, 0.20),
      include_quarter_fe = TRUE
    ),
    error = function(e) e
  )

  # (6) MSE-optimal bandwidths without quarter FE.
  fits[[6L]] <- tryCatch(
    run_one_final_spec(
      data,
      cluster_type,
      include_quarter_fe = FALSE
    ),
    error = function(e) e
  )

  bind_rows(lapply(seq_along(fits), function(i) {
    extract_final_result(
      fits[[i]],
      panel = panel,
      specification = paste0("spec", i),
      raw_n = nrow(data)
    )
  }))
}

significance_stars <- function(p) {
  if (length(p) == 0L || is.na(p)) return("")
  if (p <= 0.01) return("$^{***}$")
  if (p <= 0.05) return("$^{**}$")
  if (p <= 0.10) return("$^{*}$")
  ""
}

format_coefficient <- function(coefficient, p) {
  if (length(coefficient) == 0L || is.na(coefficient)) return("")
  paste0(
    formatC(coefficient, format = "f", digits = 3L),
    significance_stars(p)
  )
}

format_standard_error <- function(se) {
  if (length(se) == 0L || is.na(se)) return("")
  paste0("(", formatC(se, format = "f", digits = 3L), ")")
}

format_integer <- function(x) {
  if (length(x) == 0L || is.na(x)) return("")
  formatC(as.integer(x), format = "d", big.mark = ",")
}

get_result_row <- function(
    results,
    cluster_type,
    panel,
    threshold,
    specification
) {
  results %>%
    filter(
      .data$cluster_type == .env$cluster_type,
      .data$panel == .env$panel,
      .data$review_min == .env$threshold,
      .data$specification == .env$specification
    ) %>%
    slice(1L)
}

make_model_block <- function(
    results,
    cluster_type,
    panel,
    model_label,
    specification,
    use_bc
) {
  coefficient_cells <- standard_error_cells <- n_h_cells <-
    character(length(REVIEW_THRESHOLDS))

  for (i in seq_along(REVIEW_THRESHOLDS)) {
    threshold <- REVIEW_THRESHOLDS[[i]]
    row <- get_result_row(
      results,
      cluster_type,
      panel,
      threshold,
      specification
    )

    if (nrow(row) == 0L || !is.na(row$error[[1L]])) {
      coefficient_cells[[i]] <- ""
      standard_error_cells[[i]] <- ""
      n_h_cells[[i]] <- ""
    } else if (use_bc) {
      coefficient_cells[[i]] <- format_coefficient(
        row$coef_bc[[1L]],
        row$p_robust[[1L]]
      )
      standard_error_cells[[i]] <- format_standard_error(
        row$se_robust[[1L]]
      )
      n_h_cells[[i]] <- format_integer(row$n_h[[1L]])
    } else {
      coefficient_cells[[i]] <- format_coefficient(
        row$coef_conventional[[1L]],
        row$p_conventional[[1L]]
      )
      standard_error_cells[[i]] <- format_standard_error(
        row$se_conventional[[1L]]
      )
      n_h_cells[[i]] <- format_integer(row$n_h[[1L]])
    }
  }

  c(
    paste0(
      model_label, " & ",
      paste(coefficient_cells, collapse = " & "), " \\\\"
    ),
    paste0(
      " & ",
      paste(standard_error_cells, collapse = " & "), " \\\\"
    ),
    paste0(
      "$N_h$ & ",
      paste(n_h_cells, collapse = " & "), " \\\\"
    )
  )
}

make_final_table <- function(results, cluster_type, panel) {
  body <- character(0L)
  for (threshold in REVIEW_THRESHOLDS) {
    rows <- results %>%
      filter(
        .data$cluster_type == .env$cluster_type,
        .data$panel == .env$panel,
        .data$review_min == .env$threshold
      ) %>%
      arrange(match(specification, paste0("spec", 1:6)))

    main <- rows %>%
      filter(specification == "spec1") %>%
      slice(1L)

    if (nrow(main) == 0L || !is.na(main$error[[1L]])) {
      coefficient_cells <- rep("", 7L)
      standard_error_cells <- rep("", 7L)
      n_h_cells <- rep("", 7L)
    } else {
      coefficient_cells <- c(
        format_coefficient(
          main$coef_bc[[1L]],
          main$p_robust[[1L]]
        ),
        vapply(seq_len(6L), function(i) {
          row <- rows %>%
            filter(specification == paste0("spec", i)) %>%
            slice(1L)
          if (nrow(row) == 0L || !is.na(row$error[[1L]])) {
            ""
          } else {
            format_coefficient(
              row$coef_conventional[[1L]],
              row$p_conventional[[1L]]
            )
          }
        }, character(1L))
      )

      standard_error_cells <- c(
        format_standard_error(main$se_robust[[1L]]),
        vapply(seq_len(6L), function(i) {
          row <- rows %>%
            filter(specification == paste0("spec", i)) %>%
            slice(1L)
          if (nrow(row) == 0L || !is.na(row$error[[1L]])) {
            ""
          } else {
            format_standard_error(row$se_conventional[[1L]])
          }
        }, character(1L))
      )

      n_h_cells <- c(
        format_integer(main$n_h[[1L]]),
        vapply(seq_len(6L), function(i) {
          row <- rows %>%
            filter(specification == paste0("spec", i)) %>%
            slice(1L)
          if (nrow(row) == 0L || !is.na(row$error[[1L]])) {
            ""
          } else {
            format_integer(row$n_h[[1L]])
          }
        }, character(1L))
      )
    }

    body <- c(
      body,
      paste0(
        "$\\geq ", threshold, "$ & ",
        paste(coefficient_cells, collapse = " & "), " \\\\"
      ),
      paste0(
        " & ",
        paste(standard_error_cells, collapse = " & "), " \\\\"
      ),
      paste0(
        "$N_h$ & ",
        paste(n_h_cells, collapse = " & "), " \\\\"
      ),
      if (threshold < max(REVIEW_THRESHOLDS)) {
        "\\addlinespace"
      } else {
        ""
      }
    )
  }

  panel_description <- if (panel == "A") {
    "\\texttt{ex\\_super=``t''}"
  } else {
    "\\texttt{ex\\_super=``f''}"
  }

  paste(
    c(
      sprintf(
        "%% FINAL TABLE | TRIM=(1,0) | PANEL=%s | CLUSTER=%s",
        panel, cluster_type
      ),
      "\\begin{table}[!htbp]",
      "\\centering",
      sprintf(
        paste0(
          "\\caption{Balanced three-month RunRD by initial review ",
          "threshold: Panel %s; %s clustering}"
        ),
        panel,
        cluster_label(cluster_type)
      ),
      "\\scriptsize",
      "\\setlength{\\tabcolsep}{5pt}",
      "\\begin{tabular}{l*{7}{c}}",
      "\\toprule",
      " & (1) & (2) & (3) & (4) & (5) & (6) & (7) \\\\",
      "\\midrule",
      body,
      "\\bottomrule",
      "\\end{tabular}",
      "\\begin{minipage}{0.98\\textwidth}",
      "\\footnotesize",
      paste0(
        "\\textit{Notes:} The sample uses trim (1,0), the FULL price ",
        "subpanel, and Panel ", panel, " (", panel_description, "). ",
        "Each $\\geq$ row imposes ",
        "\\texttt{first\\_month\\_number\\_of\\_reviews} greater than ",
        "or equal to the stated value before trimming. Column (1) reports ",
        "specification 1's bias-corrected coefficient and robust standard ",
        "error. Columns (2)--(7) report conventional coefficients and ",
        "conventional standard errors for specifications 1--6. ",
        "Specifications 1--5 include quarter ",
        "fixed effects; specification 6 omits them. ",
        "\\texttt{bwselect=``msetwo''}; ",
        "\\texttt{masspoints=``adjust''}. ",
        "$N_h$ is the number of observations inside the estimation ",
        "bandwidth. ",
        "$^{***}p\\leq0.01$, $^{**}p\\leq0.05$, ",
        "$^{*}p\\leq0.10$."
      ),
      if (cluster_type == "quarter") {
        paste0(
          " Quarter-clustered inference uses only the six calendar-quarter ",
          "clusters and should be interpreted cautiously."
        )
      } else {
        ""
      },
      "\\end{minipage}",
      "\\end{table}",
      "\\clearpage"
    ),
    collapse = "\n"
  )
}

make_subpanel_table <- function(results, cluster_type, panel) {
  subpanels <- c("FULL", "Q1Q2", "Q2Q3", "Q3Q4")
  body <- character(0L)

  for (subpanel in subpanels) {
    rows <- results %>%
      filter(
        .data$cluster_type == .env$cluster_type,
        .data$panel == .env$panel,
        .data$subpanel == .env$subpanel
      ) %>%
      arrange(match(specification, paste0("spec", 1:6)))

    main <- rows %>%
      filter(specification == "spec1") %>%
      slice(1L)

    if (nrow(main) == 0L || !is.na(main$error[[1L]])) {
      coefficient_cells <- rep("", 7L)
      standard_error_cells <- rep("", 7L)
      n_h_cells <- rep("", 7L)
      n_full <- ""
    } else {
      coefficient_cells <- c(
        format_coefficient(
          main$coef_bc[[1L]],
          main$p_robust[[1L]]
        ),
        vapply(seq_len(6L), function(i) {
          row <- rows %>%
            filter(specification == paste0("spec", i)) %>%
            slice(1L)
          if (nrow(row) == 0L || !is.na(row$error[[1L]])) {
            ""
          } else {
            format_coefficient(
              row$coef_conventional[[1L]],
              row$p_conventional[[1L]]
            )
          }
        }, character(1L))
      )

      standard_error_cells <- c(
        format_standard_error(main$se_robust[[1L]]),
        vapply(seq_len(6L), function(i) {
          row <- rows %>%
            filter(specification == paste0("spec", i)) %>%
            slice(1L)
          if (nrow(row) == 0L || !is.na(row$error[[1L]])) {
            ""
          } else {
            format_standard_error(row$se_conventional[[1L]])
          }
        }, character(1L))
      )

      n_h_cells <- c(
        format_integer(main$n_h[[1L]]),
        vapply(seq_len(6L), function(i) {
          row <- rows %>%
            filter(specification == paste0("spec", i)) %>%
            slice(1L)
          if (nrow(row) == 0L || !is.na(row$error[[1L]])) {
            ""
          } else {
            format_integer(row$n_h[[1L]])
          }
        }, character(1L))
      )
      n_full <- format_integer(main$n_full[[1L]])
    }

    body <- c(
      body,
      paste0(
        subpanel, " & ",
        paste(coefficient_cells, collapse = " & "), " \\\\"
      ),
      paste0(
        " & ",
        paste(standard_error_cells, collapse = " & "), " \\\\"
      ),
      paste0(
        "$N_h$ & ",
        paste(n_h_cells, collapse = " & "), " \\\\"
      ),
      if (subpanel != tail(subpanels, 1L)) {
        "\\addlinespace"
      } else {
        ""
      }
    )
  }

  full_row <- results %>%
    filter(
      .data$cluster_type == .env$cluster_type,
      .data$panel == .env$panel,
      .data$subpanel == "FULL",
      .data$specification == "spec1"
    ) %>%
    slice(1L)
  full_sample_n <- if (nrow(full_row) == 0L) {
    ""
  } else {
    format_integer(full_row$n_full[[1L]])
  }
  body <- c(
    body,
    "\\midrule",
    paste0(
      "$N_{full}$ & \\multicolumn{7}{c}{",
      full_sample_n,
      "} \\\\"
    )
  )

  panel_description <- if (panel == "A") {
    "\\texttt{ex\\_super=``t''}"
  } else {
    "\\texttt{ex\\_super=``f''}"
  }

  paste(
    c(
      sprintf(
        "%% FINAL SUBPANEL TABLE | TRIM=(1,0) | PANEL=%s | CLUSTER=%s",
        panel, cluster_type
      ),
      "\\begin{table}[!htbp]",
      "\\centering",
      sprintf(
        paste0(
          "\\caption{Balanced three-month RunRD by price subpanel: ",
          "Panel %s; %s clustering}"
        ),
        panel,
        cluster_label(cluster_type)
      ),
      "\\scriptsize",
      "\\setlength{\\tabcolsep}{5pt}",
      "\\begin{tabular}{l*{7}{c}}",
      "\\toprule",
      " & (1) & (2) & (3) & (4) & (5) & (6) & (7) \\\\",
      "\\midrule",
      body,
      "\\bottomrule",
      "\\end{tabular}",
      "\\begin{minipage}{0.98\\textwidth}",
      "\\footnotesize",
      paste0(
        "\\textit{Notes:} The sample uses trim (1,0), does not impose ",
        "a \\texttt{first\\_month\\_number\\_of\\_reviews} threshold, ",
        "and reports Panel ", panel, " (", panel_description, "). ",
        "Column (1) reports specification 1's bias-corrected coefficient ",
        "and robust standard error. Columns (2)--(7) report conventional ",
        "coefficients and conventional standard errors for specifications ",
        "1--6. Specifications 1--5 include quarter fixed effects; ",
        "specification 6 omits them. ",
        "\\texttt{bwselect=``msetwo''}; ",
        "\\texttt{masspoints=``adjust''}. ",
        "$N_h$ is the number of observations inside the estimation ",
        "bandwidth and $N_{full}$ is the full pre-bandwidth subpanel ",
        "sample size. $^{***}p\\leq0.01$, $^{**}p\\leq0.05$, ",
        "$^{*}p\\leq0.10$."
      ),
      "\\end{minipage}",
      "\\end{table}",
      "\\clearpage"
    ),
    collapse = "\n"
  )
}

run_regression <- function(refresh_data = FALSE) {
dir.create(FINAL_RESULTS_DIR, recursive = TRUE, showWarnings = FALSE)
quarterly_panel <- get_balanced_3month_quarter_panel(
  force = refresh_data
)
all_results <- list()
cached_results <- if (file.exists(RESULTS_RDS)) {
  readRDS(RESULTS_RDS)
} else {
  data.frame()
}

for (review_min in REVIEW_THRESHOLDS) {
  # Apply the review threshold before calculating quarter-specific trims.
  sample_data <- prepare_runrd_sample(
    quarterly_panel,
    avg_price_trim = TRIM_AVG_PRICE,
    price_diff_trim = TRIM_PRICE_DIFF,
    review_min = review_min
  ) %>%
    mutate(quarter = droplevels(quarter))

  for (cluster_type in CLUSTER_TYPES) {
    for (panel in c("A", "B")) {
      panel_data <- if (panel == "A") {
        sample_data %>% filter(ex_super == "t")
      } else {
        sample_data %>% filter(ex_super == "f")
      }
      panel_data <- panel_data %>%
        mutate(quarter = droplevels(quarter))

      cached_rows <- if (nrow(cached_results) > 0L) {
        cached_results %>%
          filter(
            .data$review_min == .env$review_min,
            .data$cluster_type == .env$cluster_type,
            .data$panel == .env$panel
          )
      } else {
        data.frame()
      }

      key <- paste(review_min, cluster_type, panel, sep = "|")
      if (
        nrow(cached_rows) == 6L &&
        all(paste0("spec", 1:6) %in% cached_rows$specification) &&
        all(is.na(cached_rows$error))
      ) {
        cat(sprintf(
          "REUSE review_min=%d cluster=%s panel=%s n_full=%d\n",
          review_min,
          cluster_type,
          panel,
          nrow(panel_data)
        ))
        flush.console()
        all_results[[key]] <- cached_rows
        next
      }

      cat(sprintf(
        "RUN review_min=%d cluster=%s panel=%s n_full=%d\n",
        review_min,
        cluster_type,
        panel,
        nrow(panel_data)
      ))
      flush.console()

      all_results[[key]] <- run_final_six(
        panel_data,
        panel,
        cluster_type
      ) %>%
        mutate(
          review_min = review_min,
          cluster_type = cluster_type,
          .before = 1L
        )
    }
  }
}

results <- bind_rows(all_results)
saveRDS(results, RESULTS_RDS)

# Subpanel tables use no first_month_number_of_reviews threshold.
if (file.exists(SUBPANEL_RESULTS_RDS)) {
  subpanel_results <- readRDS(SUBPANEL_RESULTS_RDS)
}

expected_subpanel_rows <-
  length(CLUSTER_TYPES) * 2L * 4L * 6L
if (
  !exists("subpanel_results") ||
  nrow(subpanel_results) != expected_subpanel_rows ||
  any(!is.na(subpanel_results$error))
) {
  subpanel_sample <- prepare_runrd_sample(
    quarterly_panel,
    avg_price_trim = TRIM_AVG_PRICE,
    price_diff_trim = TRIM_PRICE_DIFF,
    review_min = NULL
  ) %>%
    mutate(quarter = droplevels(quarter))

  subpanel_conditions <- runrd_subpanel_conditions(subpanel_sample)
  subpanel_result_list <- list()

  for (cluster_type in CLUSTER_TYPES) {
    for (panel in c("A", "B")) {
      super_type <- if (panel == "A") "t" else "f"

      for (subpanel in names(subpanel_conditions)) {
        keep <- subpanel_conditions[[subpanel]] &
          subpanel_sample$ex_super == super_type
        subpanel_data <- subpanel_sample[
          which(!is.na(keep) & keep),
          ,
          drop = FALSE
        ]
        subpanel_data$quarter <- droplevels(subpanel_data$quarter)

        cat(sprintf(
          "RUN subpanel=%s cluster=%s panel=%s n_full=%d\n",
          subpanel,
          cluster_type,
          panel,
          nrow(subpanel_data)
        ))
        flush.console()

        key <- paste(cluster_type, panel, subpanel, sep = "|")
        subpanel_result_list[[key]] <- run_final_six(
          subpanel_data,
          panel,
          cluster_type
        ) %>%
          mutate(
            cluster_type = cluster_type,
            subpanel = subpanel,
            .before = 1L
          )
      }
    }
  }

  subpanel_results <- bind_rows(subpanel_result_list)
  saveRDS(subpanel_results, SUBPANEL_RESULTS_RDS)
} else {
  cat(sprintf(
    "REUSE_SUBPANEL_RESULTS rows=%d\n",
    nrow(subpanel_results)
  ))
}

for (cluster_type in CLUSTER_TYPES) {
  output_file <- sprintf(
    "balanced_3month_final_review_%s.tex",
    cluster_file_label(cluster_type)
  )
  output_file <- file.path(FINAL_RESULTS_DIR, output_file)
  tex <- paste(
    make_final_table(results, cluster_type, "A"),
    make_final_table(results, cluster_type, "B"),
    sep = "\n\n"
  )
  writeLines(tex, output_file)
  cat(sprintf("OUTPUT_FILE=%s\n", output_file))

  subpanel_output_file <- sprintf(
    "balanced_3month_final_subpanels_%s.tex",
    cluster_file_label(cluster_type)
  )
  subpanel_output_file <- file.path(
    FINAL_RESULTS_DIR,
    subpanel_output_file
  )
  subpanel_tex <- paste(
    make_subpanel_table(
      subpanel_results,
      cluster_type,
      "A"
    ),
    make_subpanel_table(
      subpanel_results,
      cluster_type,
      "B"
    ),
    sep = "\n\n"
  )
  writeLines(subpanel_tex, subpanel_output_file)
  cat(sprintf("OUTPUT_FILE=%s\n", subpanel_output_file))
}

cat(sprintf("RESULTS_RDS=%s\n", RESULTS_RDS))
cat(sprintf("SUBPANEL_RESULTS_RDS=%s\n", SUBPANEL_RESULTS_RDS))
cat(sprintf("REGRESSION_ROWS=%d\n", nrow(results)))
cat(sprintf(
  "SUBPANEL_REGRESSION_ROWS=%d\n",
  nrow(subpanel_results)
))
cat(sprintf(
  "FAILED_ROWS=%d\n",
  sum(!is.na(results$error)) +
    sum(!is.na(subpanel_results$error))
))

invisible(list(
  results = results,
  subpanel_results = subpanel_results,
  results_rds = RESULTS_RDS,
  subpanel_results_rds = SUBPANEL_RESULTS_RDS
))
}
