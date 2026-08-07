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

# Pre-specified balanced placebo design used by placebo2():
#   - paired cutoffs at equal distances on either side of 4.75
#   - 0.005 grid spacing and a common maximum distance of 0.10
#   - own-msetwo first-stage and sharp RD estimates only (no fixed h)
#   - placebo input samples stop at 4.75, so neither h nor b can use
#     observations from the other side of the true discontinuity
#   - a distance pair enters the ranking only when both estimates have at
#     least PLACEBO2_MIN_N_PER_SIDE effective observations on each RD side
PLACEBO2_GRID_STEP <- 0.005
PLACEBO2_MAX_DISTANCE <- 0.10
PLACEBO2_DISTANCES <- seq(
  PLACEBO2_GRID_STEP,
  PLACEBO2_MAX_DISTANCE,
  by = PLACEBO2_GRID_STEP
)
PLACEBO2_CANDIDATE_CUTOFFS <- sort(unique(round(c(
  TRUE_CUTOFF - PLACEBO2_DISTANCES,
  TRUE_CUTOFF,
  TRUE_CUTOFF + PLACEBO2_DISTANCES
), 8L)))
PLACEBO2_MIN_N_PER_SIDE <- 50L
PLACEBO2_OUTPUT_ROOT <- file.path(
  PLACEBO2_FINAL2_DIR,
  "results",
  "balanced_3month_fuzzy_sharp_placebo2_paired005_masspoints_off"
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
      cutoff_side = case_when(
        cutoff < TRUE_CUTOFF ~ -1L,
        cutoff > TRUE_CUTOFF ~ 1L,
        TRUE ~ 0L
      ),
      distance_from_true = round(abs(cutoff - TRUE_CUTOFF), 8L),
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
      base_eligible_for_rank =
        enough_side_n & is.finite(coefficient) & is.na(error),
      is_sharp_method = method %in% METHOD_LEVELS[4:length(METHOD_LEVELS)]
    ) %>%
    group_by(method, distance_from_true) %>%
    mutate(
      pair_complete = if_else(
        is_true_cutoff,
        TRUE,
        any(base_eligible_for_rank & cutoff_side == -1L) &
          any(base_eligible_for_rank & cutoff_side == 1L)
      ),
      eligible_for_rank = base_eligible_for_rank & pair_complete
    ) %>%
    ungroup() %>%
    mutate(
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
      is_placebo & !eligible_for_rank,
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
        " Red X marks an incomplete distance pair or N_h < ",
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
  old_note <- paste0(
    "Placebo cutoffs whose bandwidth window contains 4.75 are excluded ",
    "from the ranking. "
  )
  new_note <- paste0(
    "Placebo samples are truncated at 4.75 before bandwidth selection. ",
    "Equal-distance cutoffs on both sides enter the ranking as a pair only ",
    "when both estimates contain at least ", PLACEBO2_MIN_N_PER_SIDE,
    " effective observations on each RD side. "
  )
  tex <- sub(old_note, new_note, tex, fixed = TRUE)
  writeLines(tex, output_file, useBytes = TRUE)
}

placebo2_sample_at_cutoff <- function(data, cutoff) {
  if (cutoff < TRUE_CUTOFF) {
    return(data %>% filter(running_scr < TRUE_CUTOFF))
  }
  if (cutoff > TRUE_CUTOFF) {
    return(data %>% filter(running_scr > TRUE_CUTOFF))
  }
  data
}

placebo2_make_rd_args <- function(data, cutoff) {
  args <- make_rd_args(data, cutoff)
  args$masspoints <- "off"
  args
}

placebo2_run_first_stage_at_cutoff <- function(data, cutoff) {
  fit_data <- placebo2_sample_at_cutoff(data, cutoff)
  first_stage_args <- placebo2_make_rd_args(fit_data, cutoff)
  first_stage_args$y <- fit_data$.fuzzy_treatment

  fit <- tryCatch(
    suppressWarnings(do.call(
      rdrobust::rdrobust,
      c(first_stage_args, list(bwselect = "msetwo"))
    )),
    error = function(e) e
  )

  if (inherits(fit, "error")) {
    return(empty_method_row(
      METHOD_LEVELS[[2L]],
      cutoff,
      conditionMessage(fit)
    ))
  }

  extract_bias_corrected(fit, METHOD_LEVELS[[2L]], cutoff)
}

placebo2_run_sharp_at_cutoff <- function(data, cutoff) {
  fit_data <- placebo2_sample_at_cutoff(data, cutoff)
  fit <- tryCatch(
    suppressWarnings(do.call(
      rdrobust::rdrobust,
      c(placebo2_make_rd_args(fit_data, cutoff), list(bwselect = "msetwo"))
    )),
    error = function(e) e
  )

  if (inherits(fit, "error")) {
    return(empty_method_row(
      METHOD_LEVELS[[5L]],
      cutoff,
      conditionMessage(fit)
    ))
  }

  extract_bias_corrected(fit, METHOD_LEVELS[[5L]], cutoff)
}

placebo2_run_panel <- function(
    base_sample,
    panel = c("A", "B"),
    review_min = 30,
    output_dir = PLACEBO2_OUTPUT_ROOT,
    candidate_cutoffs = PLACEBO2_CANDIDATE_CUTOFFS,
    show_plots = TRUE,
    save_outputs = TRUE,
    analysis_sample = NULL
) {
  panel <- match.arg(panel)
  file_prefix <- sprintf(
    "%s%02d",
    tolower(panel),
    as.integer(review_min)
  )
  if (save_outputs) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }

  if (is.null(analysis_sample)) {
    analysis_sample <- prepare_placebo_sample(
      base_sample,
      panel,
      review_min
    )
  }
  candidate_cutoffs <- sort(unique(round(
    c(candidate_cutoffs, TRUE_CUTOFF),
    8L
  )))
  candidate_cutoffs <- candidate_cutoffs[
    candidate_cutoffs > min(analysis_sample$running_scr) &
      candidate_cutoffs < max(analysis_sample$running_scr)
  ]

  cat(sprintf(
    paste0(
      "\n=== placebo2 Panel %s; reviews >= %d ===\n",
      "Candidate cutoffs: %d; rdrobust calls: %d\n"
    ),
    panel,
    review_min,
    length(candidate_cutoffs),
    2L * length(candidate_cutoffs)
  ))

  first_stage_rows <- vector("list", length(candidate_cutoffs))
  for (i in seq_along(candidate_cutoffs)) {
    cutoff <- candidate_cutoffs[[i]]
    if (i == 1L || i %% 10L == 0L || i == length(candidate_cutoffs)) {
      cat(sprintf(
        "[Panel %s first stage] cutoff=%.3f (%d/%d)\n",
        panel,
        cutoff,
        i,
        length(candidate_cutoffs)
      ))
    }
    first_stage_rows[[i]] <- placebo2_run_first_stage_at_cutoff(
      analysis_sample,
      cutoff
    )
  }

  sharp_rows <- vector("list", length(candidate_cutoffs))
  for (i in seq_along(candidate_cutoffs)) {
    cutoff <- candidate_cutoffs[[i]]
    if (i == 1L || i %% 10L == 0L || i == length(candidate_cutoffs)) {
      cat(sprintf(
        "[Panel %s sharp own msetwo] cutoff=%.3f (%d/%d)\n",
        panel,
        cutoff,
        i,
        length(candidate_cutoffs)
      ))
    }
    sharp_rows[[i]] <- placebo2_run_sharp_at_cutoff(
      analysis_sample,
      cutoff
    )
  }

  results <- add_ranking_fields(bind_rows(
    first_stage_rows,
    sharp_rows
  ))
  sample_label <- sprintf("First-Month Review Count >= %d", review_min)
  first_stage_plot <- make_method_plot(
    results,
    METHOD_LEVELS[[2L]],
    sprintf("Panel %s: fuzzy first-stage placebo estimates", panel),
    paste0(sample_label, "; direct treatment-jump RD with own msetwo bandwidths.")
  )
  sharp_plot <- make_method_plot(
    results,
    METHOD_LEVELS[[5L]],
    sprintf("Panel %s: sharp RD placebo estimates", panel),
    paste0(
      sample_label,
      "; own-msetwo Bias-Corrected estimates on cutoff-truncated samples."
    )
  )

  if (show_plots) {
    print(first_stage_plot)
    print(sharp_plot)
  }
  if (save_outputs) {
    ggsave(
      file.path(output_dir, paste0(file_prefix, "_1.eps")),
      first_stage_plot,
      device = grDevices::cairo_ps,
      width = 10,
      height = 5.5,
      onefile = FALSE,
      fallback_resolution = 600
    )
    ggsave(
      file.path(output_dir, paste0(file_prefix, "_2.eps")),
      sharp_plot,
      device = grDevices::cairo_ps,
      width = 10,
      height = 9,
      onefile = FALSE,
      fallback_resolution = 600
    )
    write_rank_tex(
      results,
      panel,
      review_min,
      file.path(output_dir, paste0(file_prefix, "_rank.tex"))
    )
    saveRDS(
      list(
        panel = panel,
        review_min = review_min,
        analysis_sample = analysis_sample,
        results = results
      ),
      file.path(output_dir, paste0(file_prefix, "_results.rds"))
    )
  }

  true_results <- results %>% filter(is_true_cutoff)
  cat(sprintf(
    "\n=== Panel %s; reviews >= %d; results at 4.75 ===\n",
    panel,
    review_min
  ))
  print(as.data.frame(true_results), row.names = FALSE)

  failed_results <- results %>%
    filter(!is.na(error)) %>%
    count(method, name = "n_failed")
  cat("\n=== Failed fits ===\n")
  if (nrow(failed_results) == 0L) {
    cat("None\n")
  } else {
    print(as.data.frame(failed_results), row.names = FALSE)
  }

  invisible(list(
    panel = panel,
    review_min = review_min,
    analysis_sample = analysis_sample,
    results = results,
    true_results = true_results,
    first_stage_plot = first_stage_plot,
    sharp_plot = sharp_plot
  ))
}

placebo2 <- function(
    output_root = PLACEBO2_OUTPUT_ROOT,
    candidate_cutoffs = PLACEBO2_CANDIDATE_CUTOFFS,
    show_plots = TRUE,
    save_outputs = TRUE,
    refresh_data = FALSE
) {
  base_sample <- build_placebo_base_sample(
    refresh_data = refresh_data
  )

  output <- list(
    panel_a_review_ge_30 = placebo2_run_panel(
      base_sample = base_sample,
      panel = "A",
      review_min = 30,
      output_dir = output_root,
      candidate_cutoffs = candidate_cutoffs,
      show_plots = show_plots,
      save_outputs = save_outputs
    ),
    panel_b_review_ge_30 = placebo2_run_panel(
      base_sample = base_sample,
      panel = "B",
      review_min = 30,
      output_dir = output_root,
      candidate_cutoffs = candidate_cutoffs,
      show_plots = show_plots,
      save_outputs = save_outputs
    )
  )

  invisible(output)
}

if (sys.nframe() == 0L) {
  placebo2()
}
