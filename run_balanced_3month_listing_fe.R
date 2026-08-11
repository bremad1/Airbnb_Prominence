LISTING_FE_SCRIPT <- "run_balanced_3month_listing_fe.R"
LISTING_FE_FINAL2_DIR <- if (file.exists(file.path(
  getwd(), "Final2", LISTING_FE_SCRIPT
))) {
  normalizePath(file.path(getwd(), "Final2"), winslash = "/", mustWork = TRUE)
} else {
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}
LISTING_FE_PROJECT_ROOT <- normalizePath(
  file.path(LISTING_FE_FINAL2_DIR, ".."),
  winslash = "/",
  mustWork = TRUE
)

# Reuse the finalized sample, six-specification, extraction, and LaTeX helpers.
# Its guarded execution block does not run when sourced here.
old_wd <- getwd()
setwd(LISTING_FE_FINAL2_DIR)
tryCatch(
  source(file.path(
    LISTING_FE_FINAL2_DIR,
    "run_balanced_3month_final_review_tables.R"
  )),
  finally = setwd(old_wd)
)

LISTING_FE_RESULTS_DIR <- file.path(
  LISTING_FE_FINAL2_DIR,
  "results",
  "balanced_3month_listing_fe"
)
LISTING_FE_RESULTS_RDS <- file.path(
  LISTING_FE_RESULTS_DIR,
  "listing_fe_review_threshold_results.rds"
)

# Override only the estimator used by run_final_six(). Listing indicators are
# included in every specification. Specifications 1--5 additionally include
# quarter indicators, matching the original script's time-FE choices.
run_one_final_spec <- function(
    data,
    cluster_type = "id",
    h = NULL,
    include_quarter_fe = TRUE
) {
  if (cluster_type != "id") {
    stop("The listing-FE run uses listing-ID clustering only.")
  }

  listing_dummies <- model.matrix(
    ~ factor(id) - 1,
    data = data
  )
  if (ncol(listing_dummies) > 1L) {
    listing_dummies <- listing_dummies[, -1L, drop = FALSE]
  }

  covariates <- listing_dummies
  if (include_quarter_fe) {
    quarter_dummies <- model.matrix(~ quarter - 1, data = data)
    if (ncol(quarter_dummies) > 1L) {
      quarter_dummies <- quarter_dummies[, -1L, drop = FALSE]
      covariates <- cbind(covariates, quarter_dummies)
    }
  }

  rd_args <- list(
    y = data$price_diff,
    x = data$running_scr - 4.75,
    fuzzy = data$host_is_superhost2,
    covs = covariates,
    cluster = data$id,
    kernel = "tri",
    bwselect = "msetwo",
    p = 1,
    masspoints = "adjust",
    bwrestrict = TRUE,
    covs_drop = TRUE
  )
  if ("all" %in% names(formals(rdrobust))) rd_args$all <- TRUE
  if (!is.null(h)) rd_args$h <- h

  do.call(rdrobust, rd_args)
}

make_listing_fe_table <- function(results, panel) {
  tex <- make_final_table(results, "id", panel)
  tex <- gsub(
    paste0(
      "Balanced three-month RunRD by initial review threshold: ",
      "Panel ([AB]); Listing-ID clustering"
    ),
    paste0(
      "Balanced three-month RunRD with listing fixed effects by initial ",
      "review threshold: Panel \\1; Listing-ID clustering"
    ),
    tex
  )
  tex <- gsub(
    paste0(
      "Specifications 1--5 include quarter fixed effects; ",
      "specification 6 omits them\\."
    ),
    paste0(
      "All specifications include listing fixed effects. ",
      "Specifications 1--5 additionally include quarter fixed effects; ",
      "specification 6 omits quarter fixed effects."
    ),
    tex
  )
  tex
}

run_balanced_3month_listing_fe <- function() {
  dir.create(
    LISTING_FE_RESULTS_DIR,
    recursive = TRUE,
    showWarnings = FALSE
  )

  quarterly_panel <- build_quarter_panel(Entire)
  cached_results <- if (file.exists(LISTING_FE_RESULTS_RDS)) {
    readRDS(LISTING_FE_RESULTS_RDS)
  } else {
    data.frame()
  }
  result_list <- list()

  for (review_min in REVIEW_THRESHOLDS) {
    sample_data <- prepare_runrd_sample(
      quarterly_panel,
      avg_price_trim = TRIM_AVG_PRICE,
      price_diff_trim = TRIM_PRICE_DIFF,
      review_min = review_min
    ) %>%
      mutate(quarter = droplevels(quarter))

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
            .data$panel == .env$panel
          )
      } else {
        data.frame()
      }
      key <- paste(review_min, panel, sep = "|")

      if (
        nrow(cached_rows) == 6L &&
          all(paste0("spec", 1:6) %in% cached_rows$specification) &&
          all(is.na(cached_rows$error))
      ) {
        cat(sprintf(
          "REUSE LISTING_FE review_min=%d panel=%s n_full=%d\n",
          review_min,
          panel,
          nrow(panel_data)
        ))
        result_list[[key]] <- cached_rows
        next
      }

      cat(sprintf(
        "RUN LISTING_FE review_min=%d panel=%s n_full=%d listings=%d\n",
        review_min,
        panel,
        nrow(panel_data),
        dplyr::n_distinct(panel_data$id)
      ))
      flush.console()

      result_list[[key]] <- run_final_six(
        panel_data,
        panel,
        "id"
      ) %>%
        mutate(
          review_min = review_min,
          cluster_type = "id",
          .before = 1L
        )

      remaining_cache <- if (nrow(cached_results) > 0L) {
        cached_results %>%
          filter(!(
            .data$review_min == .env$review_min &
              .data$panel == .env$panel
          ))
      } else {
        data.frame()
      }
      cached_results <- bind_rows(
        remaining_cache,
        result_list[[key]]
      ) %>%
        distinct(
          review_min,
          panel,
          specification,
          .keep_all = TRUE
        )
      saveRDS(
        cached_results,
        LISTING_FE_RESULTS_RDS
      )
    }
  }

  results <- bind_rows(result_list) %>%
    arrange(review_min, panel, specification)
  saveRDS(results, LISTING_FE_RESULTS_RDS)

  panel_a_file <- file.path(
    LISTING_FE_RESULTS_DIR,
    "listing_fe_panel_a.tex"
  )
  panel_b_file <- file.path(
    LISTING_FE_RESULTS_DIR,
    "listing_fe_panel_b.tex"
  )
  writeLines(make_listing_fe_table(results, "A"), panel_a_file)
  writeLines(make_listing_fe_table(results, "B"), panel_b_file)

  cat(sprintf("PANEL_A_TABLE=%s\n", panel_a_file))
  cat(sprintf("PANEL_B_TABLE=%s\n", panel_b_file))
  cat(sprintf("RESULTS_RDS=%s\n", LISTING_FE_RESULTS_RDS))
  cat(sprintf(
    "FAILED_ROWS=%d\n",
    sum(!is.na(results$error))
  ))

  invisible(list(
    results = results,
    panel_a_file = panel_a_file,
    panel_b_file = panel_b_file
  ))
}

if (sys.nframe() == 0L) {
  run_balanced_3month_listing_fe()
}
