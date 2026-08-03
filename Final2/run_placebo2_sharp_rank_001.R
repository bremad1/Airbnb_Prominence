FINAL2_DIR <- if (file.exists(file.path(
  getwd(), "Final2", "func", "functions.R"
))) {
  normalizePath(file.path(getwd(), "Final2"), winslash = "/", mustWork = TRUE)
} else {
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

source(file.path(FINAL2_DIR, "func", "functions.R"))

GRID_001 <- seq(4.35, 4.95, by = 0.001)
GRID_001 <- sort(unique(round(c(GRID_001, 4.75), 8L)))
OUT_DIR_001 <- file.path(
  FINAL2_DIR,
  "results",
  "placebo2_sharp_own_rank_001"
)
dir.create(OUT_DIR_001, recursive = TRUE, showWarnings = FALSE)

get_review30_sample <- function(panel) {
  tag <- if (panel == "A") "a30" else "b30"
  baseline_file <- file.path(
    FINAL2_DIR,
    "results",
    "balanced_3month_fuzzy_sharp_placebo",
    paste0(tag, "_results.rds")
  )
  if (file.exists(baseline_file)) {
    saved <- readRDS(baseline_file)
    if (
      identical(saved$panel, panel) &&
        identical(as.numeric(saved$review_min), 30)
    ) {
      cat(sprintf("REUSE_SAMPLE panel=%s rows=%d\n", panel, nrow(saved$analysis_sample)))
      return(saved$analysis_sample)
    }
  }

  base_sample <- PLACEBO2_ENV$build_placebo_base_sample()
  PLACEBO2_ENV$prepare_placebo_sample(base_sample, panel, 30)
}

run_one_sharp_own <- function(data, cutoff) {
  common_args <- PLACEBO2_ENV$make_rd_args(data, cutoff)
  fit <- tryCatch(
    suppressWarnings(do.call(
      rdrobust::rdrobust,
      c(common_args, list(bwselect = "msetwo"))
    )),
    error = function(e) e
  )
  if (inherits(fit, "error")) {
    return(PLACEBO2_ENV$empty_method_row(
      PLACEBO2_ENV$METHOD_LEVELS[[5L]],
      cutoff,
      conditionMessage(fit)
    ))
  }
  PLACEBO2_ENV$extract_bias_corrected(
    fit,
    PLACEBO2_ENV$METHOD_LEVELS[[5L]],
    cutoff
  )
}

run_panel_grid_001 <- function(panel) {
  data <- get_review30_sample(panel)
  checkpoint <- file.path(
    OUT_DIR_001,
    paste0(tolower(panel), "30_sharp_own_001_checkpoint.rds")
  )
  completed <- if (file.exists(checkpoint)) readRDS(checkpoint) else data.frame()
  done <- if (nrow(completed) > 0L) completed$cutoff else numeric()
  todo <- GRID_001[!GRID_001 %in% done]

  rows <- if (nrow(completed) > 0L) list(completed) else list()
  for (i in seq_along(todo)) {
    cutoff <- todo[[i]]
    rows[[length(rows) + 1L]] <- run_one_sharp_own(data, cutoff)
    if (i %% 10L == 0L || i == length(todo)) {
      current <- dplyr::bind_rows(rows) %>%
        dplyr::distinct(cutoff, .keep_all = TRUE) %>%
        dplyr::arrange(cutoff)
      saveRDS(current, checkpoint)
      cat(sprintf(
        "PROGRESS panel=%s completed=%d/%d cutoff=%.3f\n",
        panel,
        nrow(current),
        length(GRID_001),
        cutoff
      ))
      flush.console()
    }
  }

  raw <- readRDS(checkpoint)
  ranked <- PLACEBO2_ENV$add_ranking_fields(raw)
  output_file <- file.path(
    OUT_DIR_001,
    paste0(tolower(panel), "30_sharp_own_001_results.rds")
  )
  saveRDS(
    list(
      panel = panel,
      review_min = 30,
      candidate_cutoffs = GRID_001,
      analysis_sample = data,
      results = ranked
    ),
    output_file
  )

  true <- ranked[ranked$is_true_cutoff, , drop = FALSE]
  cat(sprintf(
    paste0(
      "FINAL panel=%s coefficient=%.8f rank=%d/%d percentile=%.4f ",
      "n_left=%d n_right=%d eligible=%s\n"
    ),
    panel,
    true$coefficient,
    true$rank_method,
    true$rank_denominator,
    true$rank_method / true$rank_denominator,
    true$effective_n_left,
    true$effective_n_right,
    true$eligible_for_rank
  ))
  invisible(true)
}

summary_rows <- dplyr::bind_rows(
  run_panel_grid_001("A"),
  run_panel_grid_001("B")
)
saveRDS(
  summary_rows,
  file.path(OUT_DIR_001, "sharp_own_001_true_cutoff_summary.rds")
)
