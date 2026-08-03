# Baseline placebo specification with a minimum effective sample requirement.
# This file leaves placebo.R unchanged.
PLACEBO2_FINAL2_DIR <- if (file.exists(file.path(
  getwd(), "Final2", "func", "placebo2.R"
))) {
  normalizePath(file.path(getwd(), "Final2"), winslash = "/", mustWork = TRUE)
} else if (file.exists(file.path(getwd(), "func", "placebo2.R"))) {
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
} else {
  normalizePath(file.path(getwd(), ".."), winslash = "/", mustWork = TRUE)
}

# Load a private copy of all baseline placebo functions into this environment.
sys.source(
  file.path(PLACEBO2_FINAL2_DIR, "func", "placebo.R"),
  envir = environment()
)

PLACEBO2_MIN_N_PER_SIDE <- 50L
PLACEBO2_OUTPUT_ROOT <- file.path(
  PLACEBO2_FINAL2_DIR,
  "results",
  "balanced_3month_fuzzy_sharp_placebo2"
)

placebo2_empty_method_row <- empty_method_row
placebo2_extract_bias_corrected <- extract_bias_corrected
placebo2_extract_fuzzy_first_stage <- extract_fuzzy_first_stage
placebo2_extract_fuzzy_second_stage <- extract_fuzzy_second_stage
placebo2_write_rank_tex_baseline <- write_rank_tex

placebo2_append_side_n <- function(row, fit = NULL) {
  if (is.null(fit) || inherits(fit, "error")) {
    row$effective_n_left <- NA_integer_
    row$effective_n_right <- NA_integer_
  } else {
    row$effective_n_left <- as.integer(fit$N_h[[1L]])
    row$effective_n_right <- as.integer(fit$N_h[[2L]])
  }
  row
}

empty_method_row <- function(method, cutoff, error_message) {
  placebo2_append_side_n(
    placebo2_empty_method_row(method, cutoff, error_message)
  )
}

extract_bias_corrected <- function(fit, method, cutoff) {
  placebo2_append_side_n(
    placebo2_extract_bias_corrected(fit, method, cutoff),
    fit
  )
}

extract_fuzzy_first_stage <- function(fit, cutoff) {
  placebo2_append_side_n(
    placebo2_extract_fuzzy_first_stage(fit, cutoff),
    fit
  )
}

extract_fuzzy_second_stage <- function(fit, cutoff) {
  placebo2_append_side_n(
    placebo2_extract_fuzzy_second_stage(fit, cutoff),
    fit
  )
}

add_ranking_fields <- function(results) {
  results %>%
    mutate(
      method = factor(method, levels = METHOD_LEVELS),
      is_true_cutoff = abs(cutoff - TRUE_CUTOFF) < 1e-10,
      is_placebo = !is_true_cutoff,
      bandwidth_left_endpoint = cutoff - bandwidth_left,
      bandwidth_right_endpoint = cutoff + bandwidth_right,
      bandwidth_contains_true_cutoff =
        is_placebo &
        is.finite(bandwidth_left_endpoint) &
        is.finite(bandwidth_right_endpoint) &
        bandwidth_left_endpoint <= TRUE_CUTOFF &
        bandwidth_right_endpoint >= TRUE_CUTOFF,
      enough_side_n =
        is.finite(effective_n_left) &
        is.finite(effective_n_right) &
        effective_n_left >= PLACEBO2_MIN_N_PER_SIDE &
        effective_n_right >= PLACEBO2_MIN_N_PER_SIDE,
      eligible_for_rank =
        enough_side_n &
        (is_true_cutoff | !bandwidth_contains_true_cutoff),
      is_sharp_method = method %in% METHOD_LEVELS[4:length(METHOD_LEVELS)],
      coefficient_for_rank = if_else(
        eligible_for_rank,
        coefficient,
        NA_real_
      ),
      upper_ci_for_rank = if_else(
        eligible_for_rank & is_sharp_method,
        ci_high,
        NA_real_
      ),
      ranking_value = if_else(
        method == METHOD_LEVELS[[2L]],
        -coefficient_for_rank,
        coefficient_for_rank
      )
    ) %>%
    group_by(method) %>%
    mutate(
      rank_method = if_else(
        eligible_for_rank,
        min_rank(ranking_value),
        NA_integer_
      ),
      rank_denominator = sum(
        eligible_for_rank & is.finite(coefficient)
      ),
      rank_upper_ci = if_else(
        eligible_for_rank &
          is_sharp_method &
          is.finite(ci_high),
        min_rank(upper_ci_for_rank),
        NA_integer_
      ),
      rank_upper_ci_denominator = sum(
        eligible_for_rank &
          is_sharp_method &
          is.finite(ci_high)
      )
    ) %>%
    ungroup() %>%
    arrange(method, cutoff)
}

make_method_plot <- function(
    results,
    methods,
    title,
    subtitle,
    facet_ncol = 1L
) {
  all_rows <- results %>% filter(method %in% methods)
  eligible_rows <- all_rows %>%
    filter(eligible_for_rank, is.finite(coefficient))
  ci_rows <- eligible_rows %>%
    filter(has_ci, is.finite(ci_low), is.finite(ci_high))
  excluded_rows <- all_rows %>%
    filter(
      bandwidth_contains_true_cutoff | !enough_side_n,
      is.finite(coefficient)
    )
  true_rows <- all_rows %>%
    filter(is_true_cutoff, is.finite(coefficient)) %>%
    mutate(
      plot_label = sprintf(
        "4.75: %.3f; rank %d/%d",
        coefficient,
        rank_method,
        rank_denominator
      )
    )

  ggplot(all_rows, aes(x = cutoff, y = coefficient)) +
    geom_blank() +
    geom_hline(
      yintercept = 0,
      linetype = "dashed",
      color = "grey35",
      linewidth = 0.45
    ) +
    geom_errorbar(
      data = ci_rows,
      aes(ymin = ci_low, ymax = ci_high),
      width = 0.003,
      color = "#92C5DE",
      linewidth = 0.35
    ) +
    geom_point(
      data = eligible_rows,
      color = "#2166AC",
      size = 1
    ) +
    geom_point(
      data = excluded_rows,
      color = "#D6604D",
      shape = 4,
      stroke = 0.9,
      size = 2.1
    ) +
    geom_vline(
      xintercept = TRUE_CUTOFF,
      linetype = "dotted",
      color = "black",
      linewidth = 0.6
    ) +
    geom_point(
      data = true_rows,
      color = "#D6604D",
      size = 2.5
    ) +
    geom_text(
      data = true_rows,
      aes(label = plot_label),
      hjust = 1.03,
      vjust = -0.8,
      size = 2.8
    ) +
    facet_wrap(
      ~ method,
      ncol = facet_ncol,
      scales = "free_y"
    ) +
    labs(
      title = title,
      subtitle = paste0(
        subtitle,
        " Red X also marks N_h < ",
        PLACEBO2_MIN_N_PER_SIDE,
        " on either side."
      ),
      x = "Candidate cutoff",
      y = "Bias-corrected estimate"
    ) +
    theme_minimal(base_size = 10)
}

write_rank_tex <- function(results, panel, review_min, output_file) {
  placebo2_write_rank_tex_baseline(
    results,
    panel,
    review_min,
    output_file
  )
  tex <- readLines(output_file, warn = FALSE)
  old_note <- "bandwidth window contains 4.75 are excluded from the ranking. "
  new_note <- paste0(
    old_note,
    "A cutoff is also excluded when either side of the selected bandwidth ",
    "contains fewer than ", PLACEBO2_MIN_N_PER_SIDE, " observations. "
  )
  tex <- sub(old_note, new_note, tex, fixed = TRUE)
  writeLines(tex, output_file, useBytes = TRUE)
}

placebo2 <- function(
    output_root = PLACEBO2_OUTPUT_ROOT,
    candidate_cutoffs = CANDIDATE_CUTOFFS,
    show_plots = TRUE,
    save_outputs = TRUE,
    refresh_data = FALSE
) {
  run_balanced_fuzzy_sharp_placebo(
    output_root = output_root,
    candidate_cutoffs = candidate_cutoffs,
    show_plots = show_plots,
    save_outputs = save_outputs,
    refresh_data = refresh_data
  )
}

if (sys.nframe() == 0L) {
  placebo2()
}
