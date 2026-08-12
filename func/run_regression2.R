RUN_REGRESSION2_PROJECT_DIR <- if (file.exists(file.path(
  getwd(), "func", "run_regression2.R"
))) {
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
} else if (file.exists(file.path(
  getwd(), "Final2", "func", "run_regression2.R"
))) {
  normalizePath(file.path(getwd(), "Final2"), winslash = "/", mustWork = TRUE)
} else {
  stop("Run run_regression2() from the project root or Final2 directory.")
}

required_run_regression2_functions <- c(
  "get_balanced_3month_quarter_panel",
  "prepare_runrd_sample",
  "runrd_subpanel_conditions",
  "run_final_six",
  "make_subpanel_table",
  "cluster_file_label"
)
missing_run_regression2_functions <- required_run_regression2_functions[
  !vapply(
    required_run_regression2_functions,
    exists,
    logical(1L),
    mode = "function",
    inherits = TRUE
  )
]
if (length(missing_run_regression2_functions) > 0L) {
  source(file.path(
    RUN_REGRESSION2_PROJECT_DIR,
    "func",
    "sub_run_regression.R"
  ))
}

RUN_REGRESSION2_REVIEW_THRESHOLDS <- seq(10L, 40L, by = 5L)
RUN_REGRESSION2_SUBPANELS <- c("FULL", "Q1Q2", "Q2Q3", "Q3Q4")
RUN_REGRESSION2_CLUSTER_TYPES <- "id"
RUN_REGRESSION2_RESULTS_DIR <- file.path(
  RUN_REGRESSION2_PROJECT_DIR,
  "results",
  "regression_results"
)
RUN_REGRESSION2_RESULTS_RDS <- file.path(
  RUN_REGRESSION2_RESULTS_DIR,
  "review_sweep_10_40_results.rds"
)

# Review-threshold regressions for Panels A/B and all four ex-price subgroups.
run_regression2 <- function(refresh_data = FALSE) {
  dir.create(
    RUN_REGRESSION2_RESULTS_DIR,
    recursive = TRUE,
    showWarnings = FALSE
  )

  quarterly_panel <- get_balanced_3month_quarter_panel(
    force = refresh_data
  )
  cached_results <- if (
    !refresh_data && file.exists(RUN_REGRESSION2_RESULTS_RDS)
  ) {
    readRDS(RUN_REGRESSION2_RESULTS_RDS)
  } else {
    data.frame()
  }
  result_list <- list()

  for (review_min in RUN_REGRESSION2_REVIEW_THRESHOLDS) {
    sample_data <- prepare_runrd_sample(
      quarterly_panel,
      avg_price_trim = TRIM_AVG_PRICE,
      price_diff_trim = TRIM_PRICE_DIFF,
      review_min = review_min
    ) %>%
      mutate(quarter = droplevels(quarter))
    subpanel_conditions <- runrd_subpanel_conditions(sample_data)
    subpanel_conditions <-
      subpanel_conditions[RUN_REGRESSION2_SUBPANELS]

    for (cluster_type in RUN_REGRESSION2_CLUSTER_TYPES) {
      for (panel in c("A", "B")) {
        panel_value <- if (panel == "A") "t" else "f"

        for (subpanel in RUN_REGRESSION2_SUBPANELS) {
          keep <- subpanel_conditions[[subpanel]] &
            sample_data$ex_super == panel_value
          subgroup_data <- sample_data[
            which(!is.na(keep) & keep),
            ,
            drop = FALSE
          ]
          subgroup_data$quarter <- droplevels(subgroup_data$quarter)

          cached_rows <- if (nrow(cached_results) > 0L) {
            cached_results %>%
              filter(
                .data$review_min == .env$review_min,
                .data$cluster_type == .env$cluster_type,
                .data$panel == .env$panel,
                .data$subpanel == .env$subpanel
              )
          } else {
            data.frame()
          }

          key <- paste(
            review_min,
            cluster_type,
            panel,
            subpanel,
            sep = "|"
          )
          if (
            nrow(cached_rows) == 6L &&
            all(paste0("spec", 1:6) %in% cached_rows$specification) &&
            all(is.na(cached_rows$error))
          ) {
            cat(sprintf(
              paste0(
                "REUSE review_min=%d subpanel=%s cluster=%s ",
                "panel=%s n_full=%d\n"
              ),
              review_min,
              subpanel,
              cluster_type,
              panel,
              nrow(subgroup_data)
            ))
            result_list[[key]] <- cached_rows
            next
          }

          cat(sprintf(
            paste0(
              "RUN review_min=%d subpanel=%s cluster=%s ",
              "panel=%s n_full=%d\n"
            ),
            review_min,
            subpanel,
            cluster_type,
            panel,
            nrow(subgroup_data)
          ))
          flush.console()

          result_list[[key]] <- run_final_six(
            subgroup_data,
            panel,
            cluster_type
          ) %>%
            mutate(
              review_min = review_min,
              cluster_type = cluster_type,
              subpanel = subpanel,
              .before = 1L
            )
        }
      }
    }
  }

  results <- bind_rows(result_list)
  saveRDS(results, RUN_REGRESSION2_RESULTS_RDS)

  output_files <- setNames(
    character(length(RUN_REGRESSION2_REVIEW_THRESHOLDS)),
    paste0("review", RUN_REGRESSION2_REVIEW_THRESHOLDS)
  )
  for (review_min in RUN_REGRESSION2_REVIEW_THRESHOLDS) {
    threshold_results <- results %>%
      filter(.data$review_min == .env$review_min)
    cluster_type <- RUN_REGRESSION2_CLUSTER_TYPES[[1L]]
    output_file <- file.path(
      RUN_REGRESSION2_RESULTS_DIR,
      sprintf(
        "balanced_3month_review%d_subpanels_%s.tex",
        review_min,
        cluster_file_label(cluster_type)
      )
    )
    tex <- paste(
      make_subpanel_table(
        threshold_results,
        cluster_type,
        "A",
        review_min = review_min
      ),
      make_subpanel_table(
        threshold_results,
        cluster_type,
        "B",
        review_min = review_min
      ),
      sep = "\n\n"
    )
    writeLines(tex, output_file)
    output_files[[paste0("review", review_min)]] <- output_file
    cat(sprintf("OUTPUT_FILE=%s\n", output_file))
  }

  cat(sprintf("RESULTS_RDS=%s\n", RUN_REGRESSION2_RESULTS_RDS))
  cat(sprintf("REGRESSION_ROWS=%d\n", nrow(results)))
  cat(sprintf("FAILED_ROWS=%d\n", sum(!is.na(results$error))))

  invisible(list(
    results = results,
    results_rds = RUN_REGRESSION2_RESULTS_RDS,
    output_files = output_files
  ))
}
