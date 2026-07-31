# Experimental placebo specification.
#
# This file leaves placebo.R unchanged. It sources the finalized baseline and
# overrides only the MSE-bandwidth fitting and support-ranking behavior.
ADAPTIVE_FINAL2_DIR <- if (file.exists(file.path(
  getwd(), "Final2", "func", "placebo_adaptive_bandwidth.R"
))) {
  normalizePath(file.path(getwd(), "Final2"), winslash = "/", mustWork = TRUE)
} else if (file.exists(file.path(
  getwd(), "func", "placebo_adaptive_bandwidth.R"
))) {
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
} else {
  normalizePath(file.path(getwd(), ".."), winslash = "/", mustWork = TRUE)
}

# Load a private copy of the baseline placebo definitions into the environment
# in which this experimental file is evaluated.
sys.source(
  file.path(ADAPTIVE_FINAL2_DIR, "func", "placebo.R"),
  envir = environment()
)

MIN_EFFECTIVE_N_PER_SIDE <- 100L
# Set this to a finite value (for example, 0.20) to impose a locality cap.
MAX_ADAPTIVE_BANDWIDTH <- Inf
ADAPTIVE_OUTPUT_ROOT <- file.path(
  ADAPTIVE_FINAL2_DIR,
  "results",
  "balanced_3month_fuzzy_sharp_placebo_adaptive"
)

baseline_empty_method_row <- empty_method_row
baseline_extract_bias_corrected <- extract_bias_corrected
baseline_extract_fuzzy_first_stage <- extract_fuzzy_first_stage
baseline_extract_fuzzy_second_stage <- extract_fuzzy_second_stage
baseline_write_rank_tex <- write_rank_tex

adaptive_fit_value <- function(fit, name, default) {
  value <- fit[[name]]
  if (is.null(value) || length(value) == 0L) default else value[[1L]]
}

append_fit_audit <- function(row, fit = NULL) {
  if (is.null(fit) || inherits(fit, "error")) {
    row$effective_n_left <- NA_integer_
    row$effective_n_right <- NA_integer_
    row$bandwidth_adapted <- FALSE
    row$adaptation_feasible <- FALSE
    row$adaptation_error <- NA_character_
    row$initial_bandwidth_left <- NA_real_
    row$initial_bandwidth_right <- NA_real_
    row$initial_effective_n_left <- NA_integer_
    row$initial_effective_n_right <- NA_integer_
    row$required_bandwidth_left <- NA_real_
    row$required_bandwidth_right <- NA_real_
    return(row)
  }

  row$effective_n_left <- as.integer(fit$N_h[[1L]])
  row$effective_n_right <- as.integer(fit$N_h[[2L]])
  row$bandwidth_adapted <- as.logical(adaptive_fit_value(
    fit, "bandwidth_adapted", FALSE
  ))
  row$adaptation_feasible <- as.logical(adaptive_fit_value(
    fit, "adaptation_feasible", TRUE
  ))
  row$adaptation_error <- as.character(adaptive_fit_value(
    fit, "adaptation_error", NA_character_
  ))
  row$initial_bandwidth_left <- as.numeric(adaptive_fit_value(
    fit, "initial_bandwidth_left", fit$bws["h", "left"]
  ))
  row$initial_bandwidth_right <- as.numeric(adaptive_fit_value(
    fit, "initial_bandwidth_right", fit$bws["h", "right"]
  ))
  row$initial_effective_n_left <- as.integer(adaptive_fit_value(
    fit, "initial_effective_n_left", fit$N_h[[1L]]
  ))
  row$initial_effective_n_right <- as.integer(adaptive_fit_value(
    fit, "initial_effective_n_right", fit$N_h[[2L]]
  ))
  row$required_bandwidth_left <- as.numeric(adaptive_fit_value(
    fit, "required_bandwidth_left", NA_real_
  ))
  row$required_bandwidth_right <- as.numeric(adaptive_fit_value(
    fit, "required_bandwidth_right", NA_real_
  ))
  row
}

empty_method_row <- function(method, cutoff, error_message) {
  append_fit_audit(
    baseline_empty_method_row(method, cutoff, error_message)
  )
}

extract_bias_corrected <- function(fit, method, cutoff) {
  append_fit_audit(
    baseline_extract_bias_corrected(fit, method, cutoff),
    fit
  )
}

extract_fuzzy_first_stage <- function(fit, cutoff) {
  append_fit_audit(
    baseline_extract_fuzzy_first_stage(fit, cutoff),
    fit
  )
}

extract_fuzzy_second_stage <- function(fit, cutoff) {
  append_fit_audit(
    baseline_extract_fuzzy_second_stage(fit, cutoff),
    fit
  )
}

minimum_bandwidth_for_side_n <- function(
    x,
    cutoff,
    side = c("left", "right"),
    min_n = MIN_EFFECTIVE_N_PER_SIDE
) {
  side <- match.arg(side)
  distances <- if (side == "left") {
    cutoff - x[is.finite(x) & x < cutoff]
  } else {
    x[is.finite(x) & x >= cutoff] - cutoff
  }
  distances <- sort(distances[is.finite(distances)])
  if (length(distances) < min_n) return(NA_real_)

  as.numeric(distances[[min_n]]) +
    sqrt(.Machine$double.eps) * max(1, abs(cutoff))
}

run_adaptive_msetwo <- function(
    data,
    cutoff,
    common_args,
    extra_args = list(),
    min_n = MIN_EFFECTIVE_N_PER_SIDE,
    max_h = MAX_ADAPTIVE_BANDWIDTH
) {
  initial_fit <- do.call(
    rdrobust::rdrobust,
    c(common_args, extra_args, list(bwselect = "msetwo"))
  )
  initial_h <- as.numeric(initial_fit$bws["h", ])
  initial_b <- as.numeric(initial_fit$bws["b", ])
  initial_n <- as.integer(initial_fit$N_h)

  required_h <- c(
    minimum_bandwidth_for_side_n(
      data$running_scr, cutoff, "left", min_n
    ),
    minimum_bandwidth_for_side_n(
      data$running_scr, cutoff, "right", min_n
    )
  )
  needs_expansion <- initial_n < min_n
  expansion_feasible <- !needs_expansion |
    (is.finite(required_h) & required_h <= max_h)

  fit <- initial_fit
  adapted <- FALSE
  adaptation_error <- NA_character_

  if (any(needs_expansion) && all(expansion_feasible)) {
    adjusted_h <- initial_h
    adjusted_h[needs_expansion] <- pmax(
      initial_h[needs_expansion],
      required_h[needs_expansion]
    )
    # Preserve the MSE-selected h/b ratio separately on each side.
    adjusted_b <- initial_b * adjusted_h / initial_h
    adjusted_fit <- tryCatch(
      do.call(
        rdrobust::rdrobust,
        c(
          common_args,
          extra_args,
          list(h = adjusted_h, b = adjusted_b)
        )
      ),
      error = function(e) e
    )
    if (inherits(adjusted_fit, "error")) {
      adaptation_error <- conditionMessage(adjusted_fit)
    } else {
      fit <- adjusted_fit
      adapted <- any(adjusted_h > initial_h)
    }
  }

  fit$bandwidth_adapted <- adapted
  fit$adaptation_feasible <- all(expansion_feasible)
  fit$adaptation_error <- adaptation_error
  fit$initial_bandwidth_left <- initial_h[[1L]]
  fit$initial_bandwidth_right <- initial_h[[2L]]
  fit$initial_effective_n_left <- initial_n[[1L]]
  fit$initial_effective_n_right <- initial_n[[2L]]
  fit$required_bandwidth_left <- required_h[[1L]]
  fit$required_bandwidth_right <- required_h[[2L]]
  fit
}

copy_adaptive_metadata <- function(target_fit, source_fit) {
  fields <- c(
    "bandwidth_adapted",
    "adaptation_feasible",
    "adaptation_error",
    "initial_bandwidth_left",
    "initial_bandwidth_right",
    "initial_effective_n_left",
    "initial_effective_n_right",
    "required_bandwidth_left",
    "required_bandwidth_right"
  )
  for (field in fields) target_fit[[field]] <- source_fit[[field]]
  target_fit
}

run_core_estimators_at_cutoff <- function(data, cutoff) {
  common_args <- make_rd_args(data, cutoff)

  fuzzy_fit <- tryCatch(
    suppressWarnings(run_adaptive_msetwo(
      data = data,
      cutoff = cutoff,
      common_args = common_args,
      extra_args = list(fuzzy = data$.fuzzy_treatment)
    )),
    error = function(e) e
  )

  if (inherits(fuzzy_fit, "error")) {
    fuzzy_row <- empty_method_row(
      METHOD_LEVELS[[1L]], cutoff, conditionMessage(fuzzy_fit)
    )
    first_stage_row <- empty_method_row(
      METHOD_LEVELS[[2L]],
      cutoff,
      paste("Fuzzy fit failed:", conditionMessage(fuzzy_fit))
    )
    second_stage_row <- empty_method_row(
      METHOD_LEVELS[[3L]],
      cutoff,
      paste("Fuzzy fit failed:", conditionMessage(fuzzy_fit))
    )
    fuzzy_bw_sharp_row <- empty_method_row(
      METHOD_LEVELS[[4L]],
      cutoff,
      paste("Fuzzy fit failed:", conditionMessage(fuzzy_fit))
    )
  } else {
    fuzzy_row <- extract_bias_corrected(
      fuzzy_fit, METHOD_LEVELS[[1L]], cutoff
    )
    first_stage_row <- extract_fuzzy_first_stage(fuzzy_fit, cutoff)
    second_stage_row <- extract_fuzzy_second_stage(fuzzy_fit, cutoff)

    fuzzy_bw_sharp_fit <- tryCatch(
      suppressWarnings(do.call(
        rdrobust::rdrobust,
        c(
          common_args,
          list(
            h = as.numeric(fuzzy_fit$bws["h", ]),
            b = as.numeric(fuzzy_fit$bws["b", ])
          )
        )
      )),
      error = function(e) e
    )
    if (inherits(fuzzy_bw_sharp_fit, "error")) {
      fuzzy_bw_sharp_row <- empty_method_row(
        METHOD_LEVELS[[4L]],
        cutoff,
        conditionMessage(fuzzy_bw_sharp_fit)
      )
    } else {
      fuzzy_bw_sharp_fit <- copy_adaptive_metadata(
        fuzzy_bw_sharp_fit,
        fuzzy_fit
      )
      fuzzy_bw_sharp_row <- extract_bias_corrected(
        fuzzy_bw_sharp_fit, METHOD_LEVELS[[4L]], cutoff
      )
    }
  }

  own_bw_sharp_fit <- tryCatch(
    suppressWarnings(run_adaptive_msetwo(
      data = data,
      cutoff = cutoff,
      common_args = common_args
    )),
    error = function(e) e
  )
  if (inherits(own_bw_sharp_fit, "error")) {
    own_bw_sharp_row <- empty_method_row(
      METHOD_LEVELS[[5L]], cutoff, conditionMessage(own_bw_sharp_fit)
    )
  } else {
    own_bw_sharp_row <- extract_bias_corrected(
      own_bw_sharp_fit, METHOD_LEVELS[[5L]], cutoff
    )
  }

  bind_rows(
    fuzzy_row,
    first_stage_row,
    second_stage_row,
    fuzzy_bw_sharp_row,
    own_bw_sharp_row
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
        effective_n_left >= MIN_EFFECTIVE_N_PER_SIDE &
        effective_n_right >= MIN_EFFECTIVE_N_PER_SIDE,
      eligible_for_rank =
        enough_side_n &
        (is_true_cutoff | !bandwidth_contains_true_cutoff),
      is_sharp_method = method %in% METHOD_LEVELS[4:length(METHOD_LEVELS)],
      coefficient_for_rank = if_else(
        eligible_for_rank, coefficient, NA_real_
      ),
      upper_ci_for_rank = if_else(
        eligible_for_rank & is_sharp_method, ci_high, NA_real_
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
    filter(bandwidth_contains_true_cutoff, is.finite(coefficient))
  low_support_rows <- all_rows %>%
    filter(
      !enough_side_n,
      !bandwidth_contains_true_cutoff,
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
    geom_point(
      data = low_support_rows,
      color = "#E08214",
      shape = 17,
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
      subtitle = subtitle,
      x = "Candidate cutoff",
      y = "Bias-corrected estimate"
    ) +
    theme_minimal(base_size = 10)
}

write_rank_tex <- function(results, panel, review_min, output_file) {
  baseline_write_rank_tex(results, panel, review_min, output_file)
  tex <- readLines(output_file, warn = FALSE)
  old_note <- paste0(
    "bandwidth window contains 4.75 are excluded from the ranking. "
  )
  new_note <- paste0(
    old_note,
    "MSE-selected bandwidths are expanded by side when needed to contain ",
    MIN_EFFECTIVE_N_PER_SIDE,
    " observations. A cutoff is excluded unless the final bandwidth ",
    "contains at least ",
    MIN_EFFECTIVE_N_PER_SIDE,
    " observations on each side. "
  )
  tex <- sub(old_note, new_note, tex, fixed = TRUE)
  writeLines(tex, output_file, useBytes = TRUE)
}

placebo_adaptive <- function(
    output_root = ADAPTIVE_OUTPUT_ROOT,
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
  placebo_adaptive()
}
