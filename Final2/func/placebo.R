options(stringsAsFactors = FALSE)

PLACEBO_FINAL_DIR <- if (exists(
  "FINAL2_FUNCTIONS_DIR",
  inherits = TRUE
)) {
  FINAL2_FUNCTIONS_DIR
} else if (file.exists(file.path(
  getwd(), "Final2", "func", "placebo.R"
))) {
  normalizePath(file.path(getwd(), "Final2"), winslash = "/", mustWork = TRUE)
} else if (file.exists(file.path(getwd(), "func", "placebo.R"))) {
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
} else {
  normalizePath(file.path(getwd(), ".."), winslash = "/", mustWork = TRUE)
}
PROJECT_DIR <- dirname(PLACEBO_FINAL_DIR)
.libPaths(c(file.path(PROJECT_DIR, ".Rlib"), .libPaths()))

required_packages <- c("dplyr", "rdrobust", "ggplot2")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1L), quietly = TRUE)
]
if (length(missing_packages) > 0L) {
  stop("Missing packages: ", paste(missing_packages, collapse = ", "))
}

suppressPackageStartupMessages({
  library(dplyr)
  library(rdrobust)
  library(ggplot2)
})

# Load only the shared data helpers. The finalized RunRD/table script remains
# independent and is not sourced by this diagnostic.
source(file.path(
  PLACEBO_FINAL_DIR,
  "func",
  "balanced_3month_data.R"
))
BALANCED_3MONTH_HELPERS <- load_balanced_3month_helpers()
trim_quarter_sample <- BALANCED_3MONTH_HELPERS$trim_quarter_sample
target_quarters <- BALANCED_3MONTH_HELPERS$target_quarters

TRUE_CUTOFF <- 4.75
CANDIDATE_CUTOFFS <- seq(4.55, 4.95, by = 0.001)
OUTPUT_ROOT <- file.path(
  PLACEBO_FINAL_DIR,
  "results",
  "balanced_3month_fuzzy_sharp_placebo"
)

FIXED_BANDWIDTH_SPECS <- data.frame(
  method = c(
    "4-1. Sharp RD fixed h=(0.05, 0.05)",
    "4-2. Sharp RD fixed h=(0.10, 0.10)",
    "4-3. Sharp RD fixed h=(0.15, 0.15)",
    "4-4. Sharp RD fixed h=(0.10, 0.05)",
    "4-5. Sharp RD fixed h=(0.20, 0.10)",
    "4-6. Sharp RD fixed h=(0.03, 0.03)"
  ),
  h_left = c(0.05, 0.10, 0.15, 0.10, 0.20, 0.03),
  h_right = c(0.05, 0.10, 0.15, 0.05, 0.10, 0.03)
)

METHOD_LEVELS <- c(
  "1. Fuzzy RD Wald estimate",
  "2-1. Fuzzy internal first-stage treatment jump",
  "2-2. Fuzzy internal second-stage outcome jump",
  "3-1. Sharp RD at fuzzy-selected bandwidths",
  "3-2. Sharp RD at own bandwidths",
  FIXED_BANDWIDTH_SPECS$method
)

normalize_binary <- function(x) {
  if (is.logical(x)) return(as.numeric(x))
  if (is.numeric(x) || is.integer(x)) return(as.numeric(x))

  value <- tolower(trimws(as.character(x)))
  out <- rep(NA_real_, length(value))
  out[value %in% c("1", "t", "true", "yes", "y")] <- 1
  out[value %in% c("0", "f", "false", "no", "n")] <- 0
  out
}

empty_method_row <- function(method, cutoff, error_message) {
  data.frame(
    method = method,
    cutoff = cutoff,
    coefficient = NA_real_,
    se = NA_real_,
    ci_low = NA_real_,
    ci_high = NA_real_,
    p_value = NA_real_,
    bandwidth_left = NA_real_,
    bandwidth_right = NA_real_,
    bias_bandwidth_left = NA_real_,
    bias_bandwidth_right = NA_real_,
    effective_n = NA_integer_,
    has_ci = FALSE,
    error = error_message
  )
}

extract_bias_corrected <- function(fit, method, cutoff) {
  data.frame(
    method = method,
    cutoff = cutoff,
    coefficient = as.numeric(fit$Estimate[[2L]]),
    se = as.numeric(fit$se[[3L]]),
    ci_low = as.numeric(fit$ci[3L, 1L]),
    ci_high = as.numeric(fit$ci[3L, 2L]),
    p_value = as.numeric(fit$pv[[3L]]),
    bandwidth_left = as.numeric(fit$bws["h", "left"]),
    bandwidth_right = as.numeric(fit$bws["h", "right"]),
    bias_bandwidth_left = as.numeric(fit$bws["b", "left"]),
    bias_bandwidth_right = as.numeric(fit$bws["b", "right"]),
    effective_n = as.integer(sum(fit$N_h)),
    has_ci = TRUE,
    error = NA_character_
  )
}

extract_fuzzy_first_stage <- function(fit, cutoff) {
  data.frame(
    method = METHOD_LEVELS[[2L]],
    cutoff = cutoff,
    coefficient = as.numeric(fit$tau_T[[2L]]),
    se = as.numeric(fit$se_T[[3L]]),
    ci_low = as.numeric(fit$ci_T[3L, 1L]),
    ci_high = as.numeric(fit$ci_T[3L, 2L]),
    p_value = as.numeric(fit$pv_T[[3L]]),
    bandwidth_left = as.numeric(fit$bws["h", "left"]),
    bandwidth_right = as.numeric(fit$bws["h", "right"]),
    bias_bandwidth_left = as.numeric(fit$bws["b", "left"]),
    bias_bandwidth_right = as.numeric(fit$bws["b", "right"]),
    effective_n = as.integer(sum(fit$N_h)),
    has_ci = TRUE,
    error = NA_character_
  )
}

extract_fuzzy_second_stage <- function(fit, cutoff) {
  # rdrobust exposes the bias-corrected left and right outcome limits, but
  # not a standalone robust SE for their stored difference.
  outcome_jump <- as.numeric(fit$tau_bc[[2L]] - fit$tau_bc[[1L]])

  data.frame(
    method = METHOD_LEVELS[[3L]],
    cutoff = cutoff,
    coefficient = outcome_jump,
    se = NA_real_,
    ci_low = NA_real_,
    ci_high = NA_real_,
    p_value = NA_real_,
    bandwidth_left = as.numeric(fit$bws["h", "left"]),
    bandwidth_right = as.numeric(fit$bws["h", "right"]),
    bias_bandwidth_left = as.numeric(fit$bws["b", "left"]),
    bias_bandwidth_right = as.numeric(fit$bws["b", "right"]),
    effective_n = as.integer(sum(fit$N_h)),
    has_ci = FALSE,
    error = NA_character_
  )
}

make_rd_args <- function(data, cutoff) {
  quarter_dummies <- as.data.frame(
    model.matrix(~ quarter - 1, data = data)
  )
  covariates <- as.matrix(
    quarter_dummies[, -1L, drop = FALSE]
  )

  list(
    y = data$price_diff,
    x = data$running_scr,
    c = cutoff,
    covs = covariates,
    cluster = data$id,
    kernel = "tri",
    p = 1,
    masspoints = "adjust",
    bwrestrict = TRUE
  )
}

run_core_estimators_at_cutoff <- function(data, cutoff) {
  common_args <- make_rd_args(data, cutoff)

  # 1. Fuzzy local-Wald RD with its own msetwo bandwidths.
  fuzzy_fit <- tryCatch(
    suppressWarnings(do.call(
      rdrobust::rdrobust,
      c(
        common_args,
        list(
          fuzzy = data$.fuzzy_treatment,
          bwselect = "msetwo"
        )
      )
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

    # 3-1. Sharp outcome RD using h and b selected by the fuzzy fit.
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
      fuzzy_bw_sharp_row <- extract_bias_corrected(
        fuzzy_bw_sharp_fit, METHOD_LEVELS[[4L]], cutoff
      )
    }
  }

  # 3-2. Sharp outcome RD with its own msetwo bandwidths.
  own_bw_sharp_fit <- tryCatch(
    suppressWarnings(do.call(
      rdrobust::rdrobust,
      c(common_args, list(bwselect = "msetwo"))
    )),
    error = function(e) e
  )

  if (inherits(own_bw_sharp_fit, "error")) {
    own_bw_sharp_row <- empty_method_row(
      METHOD_LEVELS[[5L]],
      cutoff,
      conditionMessage(own_bw_sharp_fit)
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

run_fixed_estimators_at_cutoff <- function(data, cutoff) {
  common_args <- make_rd_args(data, cutoff)
  rows <- vector("list", nrow(FIXED_BANDWIDTH_SPECS))

  for (j in seq_len(nrow(FIXED_BANDWIDTH_SPECS))) {
    spec <- FIXED_BANDWIDTH_SPECS[j, ]
    fixed_h <- c(spec$h_left, spec$h_right)

    # Both estimation bandwidth h and bias bandwidth b are fixed.
    fit <- tryCatch(
      suppressWarnings(do.call(
        rdrobust::rdrobust,
        c(common_args, list(h = fixed_h, b = fixed_h))
      )),
      error = function(e) e
    )

    if (inherits(fit, "error")) {
      rows[[j]] <- empty_method_row(
        spec$method, cutoff, conditionMessage(fit)
      )
    } else {
      rows[[j]] <- extract_bias_corrected(
        fit, spec$method, cutoff
      )
    }
  }

  bind_rows(rows)
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
      eligible_for_rank =
        is_true_cutoff | !bandwidth_contains_true_cutoff,
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
      # The first-stage is ranked from largest to smallest. Every other
      # method is ranked from smallest to largest.
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
  all_rows <- results %>%
    filter(method %in% methods)
  eligible_rows <- all_rows %>%
    filter(eligible_for_rank, is.finite(coefficient))
  ci_rows <- eligible_rows %>%
    filter(has_ci, is.finite(ci_low), is.finite(ci_high))
  excluded_rows <- all_rows %>%
    filter(bandwidth_contains_true_cutoff, is.finite(coefficient))
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

  # geom_blank keeps facet labels available even if every estimate in one
  # method failed; this avoids combine_vars() errors on zero-row layers.
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
      subtitle = subtitle,
      x = "Candidate cutoff",
      y = "Bias-corrected estimate"
    ) +
    theme_minimal(base_size = 10)
}

build_placebo_base_sample <- function(refresh_data = FALSE) {
  # Only restrictions that preceded trim in the existing Panel A script.
  get_balanced_3month_derived_data(
    key = "placebo_base_sample_v1",
    force = refresh_data,
    build = function(quarterly_panel) {
      quarterly_panel %>%
        filter(
          !is.na(first_month_ltm),
          first_month_ltm >= 1,
          is.finite(price_diff)
        )
    }
  )
}

prepare_placebo_sample <- function(base_sample, panel, review_min) {
  # Preserve the existing Panel A baseline exactly: trim first, then apply
  # the nonmissing/nonnegative review-count validity condition.
  if (review_min == 0) {
    threshold_sample <- base_sample %>%
      trim_quarter_sample(
        avg_price_pct = 0.01,
        price_diff_pct = 0
      ) %>%
      filter(
        !is.na(first_month_number_of_reviews),
        first_month_number_of_reviews >= 0
      )
  } else {
    # New review-threshold samples apply >= 30 before recalculating trims.
    threshold_sample <- base_sample %>%
      filter(
        !is.na(first_month_number_of_reviews),
        first_month_number_of_reviews >= review_min
      ) %>%
      trim_quarter_sample(
        avg_price_pct = 0.01,
        price_diff_pct = 0
      )
  }

  panel_value <- if (panel == "A") "t" else "f"
  threshold_sample %>%
    filter(
      !is.na(ex_super),
      is.finite(running_scr),
      is.finite(price_diff),
      !is.na(id)
    ) %>%
    mutate(
      quarter = factor(quarter, levels = target_quarters),
      .fuzzy_treatment = normalize_binary(host_is_superhost2)
    ) %>%
    filter(
      is.finite(.fuzzy_treatment),
      ex_super == panel_value
    ) %>%
    mutate(quarter = droplevels(quarter))
}

write_rank_tex <- function(results, panel, review_min, output_file) {
  panel_lower <- tolower(panel)
  sample_label <- if (review_min == 0) {
    "No Additional Review-Count Restriction"
  } else {
    sprintf("First-Month Review Count $\\geq %d$", review_min)
  }

  rank_rows <- results %>%
    filter(is_true_cutoff) %>%
    mutate(
      estimate_text = if_else(
        is.finite(coefficient),
        sprintf("%.3f", coefficient),
        "NA"
      ),
      rank_text = if_else(
        is.finite(rank_method),
        sprintf("%d of %d", rank_method, rank_denominator),
        "NA"
      ),
      upper_ci_rank_text = if_else(
        is.finite(rank_upper_ci),
        sprintf(
          "%d of %d",
          rank_upper_ci,
          rank_upper_ci_denominator
        ),
        "--"
      ),
      tex_row = paste(
        as.character(method),
        estimate_text,
        rank_text,
        upper_ci_rank_text,
        sep = " & "
      )
    )

  tex_lines <- c(
    "\\begin{table}[htbp]",
    "\\centering",
    sprintf(
      paste0(
        "\\caption{Panel %s Placebo-Test Rankings at the Superhost ",
        "Cutoff: %s}"
      ),
      panel,
      sample_label
    ),
    sprintf(
      "\\label{tab:panel_%s_review_%d_placebo_ranks}",
      panel_lower,
      review_min
    ),
    "\\begin{tabular}{lrrr}",
    "\\toprule",
    paste0(
      "Method & Estimate at 4.75 & Coefficient rank & ",
      "Upper 95\\% CI rank \\\\"
    ),
    "\\midrule",
    paste0(rank_rows$tex_row, " \\\\"),
    "\\bottomrule",
    "\\end{tabular}",
    "\\begin{minipage}{0.95\\textwidth}",
    "\\footnotesize",
    paste0(
      "\\textit{Notes:} Estimates are bias-corrected. For the first-stage ",
      "treatment jump, rank 1 is the largest coefficient; for every other ",
      "method, rank 1 is the smallest coefficient. Placebo cutoffs whose ",
      "bandwidth window contains 4.75 are excluded from the ranking. ",
      "For sharp RD methods, the final column additionally ranks the upper ",
      "endpoint of the 95\\% confidence interval from lowest to highest, ",
      "so rank 1 has the lowest upper endpoint. ",
      "The denominator is the number of nonmissing eligible estimates."
    ),
    "\\end{minipage}",
    "\\end{table}"
  )

  writeLines(tex_lines, output_file, useBytes = TRUE)
}

run_panel_placebo <- function(
    base_sample,
    panel = c("A", "B"),
    review_min = 0,
    output_dir = NULL,
    candidate_cutoffs = CANDIDATE_CUTOFFS,
    show_plots = TRUE,
  save_outputs = TRUE
) {
  panel <- match.arg(panel)
  panel_lower <- tolower(panel)
  file_prefix <- sprintf(
    "%s%02d",
    panel_lower,
    as.integer(review_min)
  )
  if (is.null(output_dir)) {
    output_dir <- OUTPUT_ROOT
  }
  if (save_outputs) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }

  sample_label <- if (review_min == 0) {
    "No Additional Review-Count Restriction"
  } else {
    sprintf("First-Month Review Count >= %d", review_min)
  }
  analysis_sample <- prepare_placebo_sample(
    base_sample,
    panel,
    review_min
  )

  candidate_cutoffs <- sort(unique(round(
    c(candidate_cutoffs, TRUE_CUTOFF),
    digits = 8L
  )))
  candidate_cutoffs <- candidate_cutoffs[
    candidate_cutoffs > min(analysis_sample$running_scr) &
      candidate_cutoffs < max(analysis_sample$running_scr)
  ]

  cat(sprintf(
    paste0(
      "Panel %s; reviews >= %d; rows: %d\n",
      "Candidate cutoffs: %d\n",
      "Expected rdrobust calls: up to %d\n"
    ),
    panel,
    review_min,
    nrow(analysis_sample),
    length(candidate_cutoffs),
    9L * length(candidate_cutoffs)
  ))

  result_list <- vector("list", length(candidate_cutoffs))
  for (i in seq_along(candidate_cutoffs)) {
    cutoff <- candidate_cutoffs[[i]]
    cat(sprintf(
      "[Panel %s; reviews >= %d] cutoff=%.2f (%d/%d)\n",
      panel,
      review_min,
      cutoff,
      i,
      length(candidate_cutoffs)
    ))
    result_list[[i]] <- bind_rows(
      run_core_estimators_at_cutoff(analysis_sample, cutoff),
      run_fixed_estimators_at_cutoff(analysis_sample, cutoff)
    )
  }

  results <- add_ranking_fields(bind_rows(result_list))

  fuzzy_values <- results %>%
    filter(
      method == METHOD_LEVELS[[1L]],
      is.finite(coefficient)
    ) %>%
    pull(coefficient)
  fuzzy_abs_90 <- if (length(fuzzy_values) > 0L) {
    as.numeric(quantile(
      abs(fuzzy_values),
      probs = 0.90,
      na.rm = TRUE,
      names = FALSE
    ))
  } else {
    NA_real_
  }

  fuzzy_plot <- make_method_plot(
    results,
    METHOD_LEVELS[[1L]],
    sprintf("Panel %s: fuzzy RD placebo estimates", panel),
    paste0(
      sample_label,
      "; bias-corrected fuzzy-Wald estimates with robust 95% CIs."
    )
  )
  if (is.finite(fuzzy_abs_90) && fuzzy_abs_90 > 0) {
    fuzzy_plot <- fuzzy_plot +
      coord_cartesian(ylim = c(-fuzzy_abs_90, fuzzy_abs_90))
  }

  fuzzy_stages_plot <- make_method_plot(
    results,
    METHOD_LEVELS[2:3],
    sprintf("Panel %s: fuzzy first- and second-stage jumps", panel),
    paste0(
      sample_label,
      "; first stage has a robust 95% CI; second stage has no CI."
    )
  )

  sharp_plot <- make_method_plot(
    results,
    METHOD_LEVELS[4:5],
    sprintf("Panel %s: sharp RD placebo estimates", panel),
    paste0(
      sample_label,
      "; fuzzy-selected versus independently selected bandwidths."
    )
  )

  fixed_plot <- make_method_plot(
    results,
    FIXED_BANDWIDTH_SPECS$method,
    sprintf(
      "Panel %s: fixed-bandwidth sharp RD placebo estimates",
      panel
    ),
    paste0(
      sample_label,
      "; red X marks a placebo window containing 4.75."
    ),
    facet_ncol = 2L
  )

  if (show_plots) {
    print(fuzzy_plot)
    print(fuzzy_stages_plot)
    print(sharp_plot)
    print(fixed_plot)
  }

  if (save_outputs) {
    ggsave(
      file.path(output_dir, paste0(file_prefix, "_1.eps")),
      fuzzy_plot,
      device = grDevices::cairo_ps,
      width = 10,
      height = 5.5,
      onefile = FALSE,
      fallback_resolution = 600
    )
    ggsave(
      file.path(output_dir, paste0(file_prefix, "_2.eps")),
      fuzzy_stages_plot,
      device = grDevices::cairo_ps,
      width = 10,
      height = 8,
      onefile = FALSE,
      fallback_resolution = 600
    )
    ggsave(
      file.path(output_dir, paste0(file_prefix, "_3.eps")),
      sharp_plot,
      device = grDevices::cairo_ps,
      width = 10,
      height = 8,
      onefile = FALSE,
      fallback_resolution = 600
    )
    ggsave(
      file.path(
        output_dir,
        paste0(file_prefix, "_4.eps")
      ),
      fixed_plot,
      device = grDevices::cairo_ps,
      width = 12,
      height = 12,
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

  true_results <- results %>%
    filter(is_true_cutoff) %>%
    select(
      method,
      coefficient,
      se,
      ci_low,
      ci_high,
      p_value,
      bandwidth_left,
      bandwidth_right,
      bias_bandwidth_left,
      bias_bandwidth_right,
      effective_n,
      rank_method,
      rank_denominator,
      rank_upper_ci,
      rank_upper_ci_denominator
    )

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
    fuzzy_plot = fuzzy_plot,
    fuzzy_stages_plot = fuzzy_stages_plot,
    sharp_plot = sharp_plot,
    fixed_plot = fixed_plot
  ))
}

run_balanced_fuzzy_sharp_placebo <- function(
    output_root = OUTPUT_ROOT,
    candidate_cutoffs = CANDIDATE_CUTOFFS,
    show_plots = TRUE,
    save_outputs = TRUE,
    refresh_data = FALSE
) {
  base_sample <- build_placebo_base_sample(
    refresh_data = refresh_data
  )
  requested_samples <- data.frame(
    result_name = c(
      "panel_a_no_review_filter",
      "panel_a_review_ge_30",
      "panel_b_review_ge_30"
    ),
    panel = c("A", "A", "B"),
    review_min = c(0, 30, 30)
  )

  output <- list()
  for (i in seq_len(nrow(requested_samples))) {
    spec <- requested_samples[i, ]
    output[[spec$result_name]] <- run_panel_placebo(
      base_sample = base_sample,
      panel = spec$panel,
      review_min = spec$review_min,
      output_dir = output_root,
      candidate_cutoffs = candidate_cutoffs,
      show_plots = show_plots,
      save_outputs = save_outputs
    )
  }

  invisible(output)
}

# Backward-compatible entry point for the original Panel A baseline only.
run_panel_a_fuzzy_sharp_placebo <- function(
    output_dir = OUTPUT_ROOT,
    candidate_cutoffs = CANDIDATE_CUTOFFS,
    show_plots = TRUE,
    save_outputs = TRUE,
    refresh_data = FALSE
) {
  run_panel_placebo(
    base_sample = build_placebo_base_sample(
      refresh_data = refresh_data
    ),
    panel = "A",
    review_min = 0,
    output_dir = output_dir,
    candidate_cutoffs = candidate_cutoffs,
    show_plots = show_plots,
    save_outputs = save_outputs
  )
}

# Simple user-facing entry point. The balanced three-month data are built only
# when quarterly_panel is absent from the global environment.
placebo <- function(
    output_root = OUTPUT_ROOT,
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
  args <- commandArgs(trailingOnly = TRUE)
  output_root <- if (length(args) >= 1L) args[[1L]] else OUTPUT_ROOT
  placebo(output_root = output_root)
}
