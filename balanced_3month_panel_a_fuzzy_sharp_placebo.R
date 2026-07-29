options(stringsAsFactors = FALSE)

.libPaths(c(file.path(getwd(), ".Rlib"), .libPaths()))

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

source("balanced-3month.R")

TRUE_CUTOFF <- 4.75
CANDIDATE_CUTOFFS <- seq(4.55, 4.95, by = 0.01)
OUTPUT_DIR <-
  "results/balanced_3month_panel_a_fuzzy_sharp_placebo"

METHOD_LEVELS <- c(
  "1. Fuzzy RD Wald tau (msetwo)",
  "2. Sharp RD at fuzzy msetwo bandwidth",
  "3. Sharp RD at own msetwo bandwidth"
)

FIXED_BANDWIDTHS <- data.frame(
  bandwidth_spec = c(
    "Symmetric (0.03, 0.03)",
    "Symmetric (0.05, 0.05)",
    "Symmetric (0.10, 0.10)",
    "Asymmetric (0.03, 0.06)",
    "Asymmetric (0.05, 0.10)",
    "Asymmetric (0.10, 0.15)"
  ),
  bandwidth_left = c(0.03, 0.05, 0.10, 0.03, 0.05, 0.10),
  bandwidth_right = c(0.03, 0.05, 0.10, 0.06, 0.10, 0.15),
  stringsAsFactors = FALSE
)

FIXED_METHOD_LEVELS <- c(
  "1. Fuzzy RD Wald tau (fixed bandwidth)",
  "2. Sharp RD ITT (same fixed bandwidth)"
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
    error = error_message
  )
}

extract_conventional <- function(fit, method, cutoff) {
  coefficient <- as.numeric(fit$Estimate[[1L]])
  se <- as.numeric(fit$se[[1L]])
  ci_low <- as.numeric(fit$ci[1L, 1L])
  ci_high <- as.numeric(fit$ci[1L, 2L])
  p_value <- as.numeric(fit$pv[[1L]])

  if (!all(is.finite(c(
    coefficient, se, ci_low, ci_high, p_value
  )))) {
    out <- empty_method_row(
      method,
      cutoff,
      paste0(
        "Non-finite clustered inference: coefficient=",
        coefficient,
        ", se=", se,
        ", ci=[", ci_low, ", ", ci_high, "]"
      )
    )
    out$bandwidth_left <- as.numeric(fit$bws["h", "left"])
    out$bandwidth_right <- as.numeric(fit$bws["h", "right"])
    out$bias_bandwidth_left <- as.numeric(fit$bws["b", "left"])
    out$bias_bandwidth_right <- as.numeric(fit$bws["b", "right"])
    out$effective_n <- as.integer(sum(fit$N_h))
    return(out)
  }

  data.frame(
    method = method,
    cutoff = cutoff,
    coefficient = coefficient,
    se = se,
    ci_low = ci_low,
    ci_high = ci_high,
    p_value = p_value,
    bandwidth_left = as.numeric(fit$bws["h", "left"]),
    bandwidth_right = as.numeric(fit$bws["h", "right"]),
    bias_bandwidth_left = as.numeric(fit$bws["b", "left"]),
    bias_bandwidth_right = as.numeric(fit$bws["b", "right"]),
    effective_n = as.integer(sum(fit$N_h)),
    error = NA_character_
  )
}

run_three_estimators_at_cutoff <- function(data, cutoff) {
  quarter_dummies <- as.data.frame(
    model.matrix(~ quarter - 1, data = data)
  )
  covariates <- as.matrix(
    quarter_dummies[, -1L, drop = FALSE]
  )

  common_args <- list(
    y = data$price_diff,
    x = data$running_scr,
    c = cutoff,
    covs = covariates,
    cluster = data$id,
    kernel = "tri",
    p = 1,
    masspoints = "off",
    bwrestrict = TRUE
  )

  # 1. Original fuzzy RD. This row reports the Wald ratio tau itself.
  fuzzy_args <- c(
    common_args,
    list(
      fuzzy = data$.fuzzy_treatment,
      bwselect = "msetwo"
    )
  )
  fuzzy_fit <- tryCatch(
    suppressWarnings(do.call(rdrobust::rdrobust, fuzzy_args)),
    error = function(e) e
  )

  if (inherits(fuzzy_fit, "error")) {
    fuzzy_row <- empty_method_row(
      METHOD_LEVELS[[1L]],
      cutoff,
      conditionMessage(fuzzy_fit)
    )
    fuzzy_bw_sharp_row <- empty_method_row(
      METHOD_LEVELS[[2L]],
      cutoff,
      paste("Fuzzy fit failed:", conditionMessage(fuzzy_fit))
    )
  } else {
    fuzzy_row <- extract_conventional(
      fuzzy_fit,
      METHOD_LEVELS[[1L]],
      cutoff
    )

    # 2. Sharp outcome RD, but hold h and b at the values selected by the
    # fuzzy fit. This is the fuzzy specification's reduced-form numerator.
    fuzzy_bw_sharp_args <- c(
      common_args,
      list(
        h = as.numeric(fuzzy_fit$bws["h", ]),
        b = as.numeric(fuzzy_fit$bws["b", ])
      )
    )
    fuzzy_bw_sharp_fit <- tryCatch(
      suppressWarnings(
        do.call(rdrobust::rdrobust, fuzzy_bw_sharp_args)
      ),
      error = function(e) e
    )

    if (inherits(fuzzy_bw_sharp_fit, "error")) {
      fuzzy_bw_sharp_row <- empty_method_row(
        METHOD_LEVELS[[2L]],
        cutoff,
        conditionMessage(fuzzy_bw_sharp_fit)
      )
    } else {
      fuzzy_bw_sharp_row <- extract_conventional(
        fuzzy_bw_sharp_fit,
        METHOD_LEVELS[[2L]],
        cutoff
      )

      numerator_from_fuzzy <- as.numeric(
        fuzzy_fit$tau_cl[[2L]] - fuzzy_fit$tau_cl[[1L]]
      )
      fuzzy_bw_sharp_row$numerator_from_fuzzy <-
        numerator_from_fuzzy
      fuzzy_bw_sharp_row$numerator_check_difference <-
        fuzzy_bw_sharp_row$coefficient - numerator_from_fuzzy
    }
  }

  # 3. Sharp outcome RD with its own MSE-optimal bandwidth selection.
  own_bw_sharp_args <- c(
    common_args,
    list(bwselect = "msetwo")
  )
  own_bw_sharp_fit <- tryCatch(
    suppressWarnings(
      do.call(rdrobust::rdrobust, own_bw_sharp_args)
    ),
    error = function(e) e
  )

  if (inherits(own_bw_sharp_fit, "error")) {
    own_bw_sharp_row <- empty_method_row(
      METHOD_LEVELS[[3L]],
      cutoff,
      conditionMessage(own_bw_sharp_fit)
    )
  } else {
    own_bw_sharp_row <- extract_conventional(
      own_bw_sharp_fit,
      METHOD_LEVELS[[3L]],
      cutoff
    )
  }

  bind_rows(
    fuzzy_row,
    fuzzy_bw_sharp_row,
    own_bw_sharp_row
  )
}

run_fixed_estimators_at_cutoff <- function(
    data,
    cutoff,
    bandwidth_left,
    bandwidth_right,
    bandwidth_spec
) {
  quarter_dummies <- as.data.frame(
    model.matrix(~ quarter - 1, data = data)
  )
  covariates <- as.matrix(
    quarter_dummies[, -1L, drop = FALSE]
  )
  fixed_h <- c(bandwidth_left, bandwidth_right)

  common_args <- list(
    y = data$price_diff,
    x = data$running_scr,
    c = cutoff,
    covs = covariates,
    cluster = data$id,
    h = fixed_h,
    b = fixed_h,
    kernel = "tri",
    p = 1,
    masspoints = "off",
    bwrestrict = TRUE
  )

  fuzzy_fit <- tryCatch(
    suppressWarnings(do.call(
      rdrobust::rdrobust,
      c(
        common_args,
        list(fuzzy = data$.fuzzy_treatment)
      )
    )),
    error = function(e) e
  )

  if (inherits(fuzzy_fit, "error")) {
    fuzzy_row <- empty_method_row(
      FIXED_METHOD_LEVELS[[1L]],
      cutoff,
      conditionMessage(fuzzy_fit)
    )
    sharp_row <- empty_method_row(
      FIXED_METHOD_LEVELS[[2L]],
      cutoff,
      paste("Fuzzy fit failed:", conditionMessage(fuzzy_fit))
    )
  } else {
    fuzzy_row <- extract_conventional(
      fuzzy_fit,
      FIXED_METHOD_LEVELS[[1L]],
      cutoff
    )

    sharp_fit <- tryCatch(
      suppressWarnings(do.call(
        rdrobust::rdrobust,
        common_args
      )),
      error = function(e) e
    )

    if (inherits(sharp_fit, "error")) {
      sharp_row <- empty_method_row(
        FIXED_METHOD_LEVELS[[2L]],
        cutoff,
        conditionMessage(sharp_fit)
      )
    } else {
      sharp_row <- extract_conventional(
        sharp_fit,
        FIXED_METHOD_LEVELS[[2L]],
        cutoff
      )

      numerator_from_fuzzy <- as.numeric(
        fuzzy_fit$tau_cl[[2L]] - fuzzy_fit$tau_cl[[1L]]
      )
      sharp_row$numerator_from_fuzzy <-
        numerator_from_fuzzy
      sharp_row$numerator_check_difference <-
        sharp_row$coefficient - numerator_from_fuzzy
    }
  }

  bind_rows(fuzzy_row, sharp_row) %>%
    mutate(
      bandwidth_spec = bandwidth_spec,
      requested_bandwidth_left = bandwidth_left,
      requested_bandwidth_right = bandwidth_right
    )
}

run_panel_a_fuzzy_sharp_placebo <- function(
    output_dir = OUTPUT_DIR,
    candidate_cutoffs = CANDIDATE_CUTOFFS,
    fixed_bandwidths = FIXED_BANDWIDTHS,
    show_plot = TRUE,
    save_figure = TRUE
) {
  if (save_figure) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }

  # Panel A only: ex_super == "t".
  analysis_sample <- build_quarter_panel(Entire) %>%
    filter(
      !is.na(first_month_ltm),
      first_month_ltm >= 1,
      is.finite(price_diff)
    ) %>%
    trim_quarter_sample(
      avg_price_pct = 0,
      price_diff_pct = 0
    ) %>%
    filter(
      !is.na(first_month_number_of_reviews),
      first_month_number_of_reviews >= 0,
      ex_super == "t",
      is.finite(running_scr),
      is.finite(price_diff),
      !is.na(id)
    ) %>%
    mutate(
      quarter = factor(quarter, levels = target_quarters),
      .fuzzy_treatment = normalize_binary(host_is_superhost2)
    ) %>%
    filter(is.finite(.fuzzy_treatment))

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
      "Panel A rows: %d\nCandidate cutoffs: %d\n",
      "Expected rdrobust calls: up to %d\n"
    ),
    nrow(analysis_sample),
    length(candidate_cutoffs),
    3L * length(candidate_cutoffs)
  ))

  result_list <- vector("list", length(candidate_cutoffs))
  for (i in seq_along(candidate_cutoffs)) {
    cutoff <- candidate_cutoffs[[i]]
    cat(sprintf(
      "[Panel A three-method placebo] cutoff=%.2f (%d/%d)\n",
      cutoff,
      i,
      length(candidate_cutoffs)
    ))
    result_list[[i]] <- run_three_estimators_at_cutoff(
      analysis_sample,
      cutoff
    )
  }

  results <- bind_rows(result_list) %>%
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
      coefficient_for_rank = if_else(
        eligible_for_rank,
        coefficient,
        NA_real_
      )
    ) %>%
    group_by(method) %>%
    mutate(
      # Ascending coefficient rank: rank 1 is the most negative estimate.
      rank_negative = if_else(
        eligible_for_rank,
        min_rank(coefficient_for_rank),
        NA_integer_
      ),
      rank_denominator = sum(
        eligible_for_rank & is.finite(coefficient)
      )
    ) %>%
    ungroup() %>%
    arrange(method, cutoff)

  true_results <- results %>%
    filter(is_true_cutoff) %>%
    mutate(
      plot_label = sprintf(
        "4.75: %.3f; rank %d/%d",
        coefficient,
        rank_negative,
        rank_denominator
      )
    )

  failed_results <- results %>%
    filter(!is.na(error)) %>%
    count(method, name = "n_failed")

  plot_data <- results %>%
    filter(
      eligible_for_rank,
      is.finite(coefficient),
      is.finite(ci_low),
      is.finite(ci_high)
    )

  comparison_plot <- ggplot(
    plot_data,
    aes(x = cutoff, y = coefficient)
  ) +
    geom_hline(
      yintercept = 0,
      linetype = "dashed",
      color = "grey35",
      linewidth = 0.45
    ) +
    geom_errorbar(
      aes(ymin = ci_low, ymax = ci_high),
      width = 0.003,
      color = "#92C5DE",
      linewidth = 0.35
    ) +
    geom_line(color = "#2166AC", linewidth = 0.45) +
    geom_point(color = "#2166AC", size = 1.0) +
    geom_point(
      data = results %>% filter(
        bandwidth_contains_true_cutoff,
        is.finite(coefficient)
      ),
      aes(x = cutoff, y = coefficient),
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
    geom_vline(
      xintercept = 4.80,
      linetype = "dashed",
      color = "#7B3294",
      linewidth = 0.55
    ) +
    geom_point(
      data = true_results,
      aes(x = cutoff, y = coefficient),
      color = "#D6604D",
      size = 2.5
    ) +
    geom_text(
      data = true_results,
      aes(
        x = cutoff,
        y = coefficient,
        label = plot_label
      ),
      hjust = 1.03,
      vjust = -0.8,
      size = 2.8
    ) +
    facet_wrap(
      ~ method,
      ncol = 1,
      scales = "free_y"
    ) +
    labs(
      title = "Panel A placebo comparison: fuzzy and sharp RD",
      subtitle = paste0(
        "Quarter covariates and listing-ID clustering in every fit; ",
        "all bandwidths originate from msetwo selection; ",
        "bars are pointwise 95% CIs."
      ),
      x = "Candidate cutoff",
      y = "Estimated discontinuity"
    ) +
    theme_minimal(base_size = 10)

  if (show_plot) print(comparison_plot)

  if (save_figure) {
    ggsave(
      file.path(
        output_dir,
        "panel_a_fuzzy_tau_vs_sharp_placebo.eps"
      ),
      comparison_plot,
      device = grDevices::cairo_ps,
      width = 10,
      height = 12,
      onefile = FALSE,
      fallback_resolution = 600
    )
  }

  # -----------------------------------------------------------------------
  # Fixed-bandwidth comparison, written to a separate EPS.
  # -----------------------------------------------------------------------
  fixed_jobs <- merge(
    fixed_bandwidths,
    data.frame(cutoff = candidate_cutoffs),
    all = TRUE
  ) %>%
    arrange(bandwidth_spec, cutoff)

  cat(sprintf(
    paste0(
      "\nFixed-bandwidth specifications: %d\n",
      "Expected additional rdrobust calls: up to %d\n"
    ),
    nrow(fixed_jobs),
    2L * nrow(fixed_jobs)
  ))

  fixed_result_list <- vector("list", nrow(fixed_jobs))
  for (i in seq_len(nrow(fixed_jobs))) {
    job <- fixed_jobs[i, ]
    cat(sprintf(
      "[Panel A fixed-BW placebo] spec=%s cutoff=%.2f (%d/%d)\n",
      job$bandwidth_spec,
      job$cutoff,
      i,
      nrow(fixed_jobs)
    ))
    fixed_result_list[[i]] <- run_fixed_estimators_at_cutoff(
      data = analysis_sample,
      cutoff = job$cutoff,
      bandwidth_left = job$bandwidth_left,
      bandwidth_right = job$bandwidth_right,
      bandwidth_spec = job$bandwidth_spec
    )
  }

  fixed_results <- bind_rows(fixed_result_list) %>%
    mutate(
      method = factor(method, levels = FIXED_METHOD_LEVELS),
      bandwidth_spec = factor(
        bandwidth_spec,
        levels = fixed_bandwidths$bandwidth_spec
      ),
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
      coefficient_for_rank = if_else(
        eligible_for_rank,
        coefficient,
        NA_real_
      )
    ) %>%
    group_by(method, bandwidth_spec) %>%
    mutate(
      rank_negative = if_else(
        eligible_for_rank,
        min_rank(coefficient_for_rank),
        NA_integer_
      ),
      rank_denominator = sum(
        eligible_for_rank & is.finite(coefficient)
      )
    ) %>%
    ungroup() %>%
    arrange(method, bandwidth_spec, cutoff)

  fixed_true_results <- fixed_results %>%
    filter(is_true_cutoff) %>%
    mutate(
      plot_label = sprintf(
        "%.3f; %d/%d",
        coefficient,
        rank_negative,
        rank_denominator
      )
    )

  fixed_plot_data <- fixed_results %>%
    filter(
      eligible_for_rank,
      is.finite(coefficient),
      is.finite(ci_low),
      is.finite(ci_high)
    )

  fixed_plot <- ggplot(
    fixed_plot_data,
    aes(x = cutoff, y = coefficient)
  ) +
    geom_hline(
      yintercept = 0,
      linetype = "dashed",
      color = "grey35",
      linewidth = 0.4
    ) +
    geom_errorbar(
      aes(ymin = ci_low, ymax = ci_high),
      width = 0.003,
      color = "#92C5DE",
      linewidth = 0.3
    ) +
    geom_line(color = "#2166AC", linewidth = 0.4) +
    geom_point(color = "#2166AC", size = 0.8) +
    geom_point(
      data = fixed_results %>% filter(
        bandwidth_contains_true_cutoff,
        is.finite(coefficient)
      ),
      aes(x = cutoff, y = coefficient),
      color = "#D6604D",
      shape = 4,
      stroke = 0.8,
      size = 1.8
    ) +
    geom_vline(
      xintercept = TRUE_CUTOFF,
      linetype = "dotted",
      color = "black",
      linewidth = 0.55
    ) +
    geom_vline(
      xintercept = 4.80,
      linetype = "dashed",
      color = "#7B3294",
      linewidth = 0.5
    ) +
    geom_point(
      data = fixed_true_results,
      aes(x = cutoff, y = coefficient),
      color = "#D6604D",
      size = 2
    ) +
    geom_text(
      data = fixed_true_results,
      aes(
        x = cutoff,
        y = coefficient,
        label = plot_label
      ),
      hjust = 1.03,
      vjust = -0.8,
      size = 2.1
    ) +
    facet_grid(
      rows = vars(method),
      cols = vars(bandwidth_spec),
      scales = "free_y"
    ) +
    labs(
      title = "Panel A placebo comparison at fixed bandwidths",
      subtitle = paste0(
        "Quarter covariates and listing-ID clustering in every fit; ",
        "labels show the 4.75 estimate and negative-order rank."
      ),
      x = "Candidate cutoff",
      y = "Estimated discontinuity"
    ) +
    theme_minimal(base_size = 8) +
    theme(
      strip.text.x = element_text(size = 7),
      strip.text.y = element_text(size = 7)
    )

  if (show_plot) print(fixed_plot)

  if (save_figure) {
    ggsave(
      file.path(
        output_dir,
        "panel_a_fuzzy_tau_vs_sharp_fixed_bandwidths.eps"
      ),
      fixed_plot,
      device = grDevices::cairo_ps,
      width = 20,
      height = 8,
      onefile = FALSE,
      fallback_resolution = 600
    )
  }

  cat("\n=== Panel A results at 4.75 ===\n")
  print(as.data.frame(
    true_results %>%
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
        rank_negative,
        rank_denominator
      )
  ), row.names = FALSE)
  cat("\n=== Non-finite or failed fits ===\n")
  if (nrow(failed_results) == 0L) {
    cat("None\n")
  } else {
    print(as.data.frame(failed_results), row.names = FALSE)
  }
  cat("\n=== Panel A fixed-bandwidth results at 4.75 ===\n")
  print(as.data.frame(
    fixed_true_results %>%
      select(
        method,
        bandwidth_spec,
        coefficient,
        se,
        ci_low,
        ci_high,
        p_value,
        rank_negative,
        rank_denominator
      )
  ), row.names = FALSE)

  invisible(list(
    analysis_sample = analysis_sample,
    results = results,
    true_results = true_results,
    failed_results = failed_results,
    plot = comparison_plot,
    optimal_plot = comparison_plot,
    fixed_results = fixed_results,
    fixed_true_results = fixed_true_results,
    fixed_plot = fixed_plot
  ))
}

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  output_dir <- if (length(args) >= 1L) args[[1L]] else OUTPUT_DIR
  run_panel_a_fuzzy_sharp_placebo(output_dir = output_dir)
}
