options(stringsAsFactors = FALSE)

.libPaths(c(file.path(getwd(), ".Rlib"), .libPaths()))

required_packages <- c("dplyr", "rddensity", "ggplot2")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1L), quietly = TRUE)
]
if (length(missing_packages) > 0L) {
  stop("Missing packages: ", paste(missing_packages, collapse = ", "))
}

suppressPackageStartupMessages({
  library(dplyr)
  library(rddensity)
  library(ggplot2)
})

MCCRARY_FINAL_DIR <- if (exists(
  "FINAL2_FUNCTIONS_DIR",
  inherits = TRUE
)) {
  FINAL2_FUNCTIONS_DIR
} else if (file.exists(file.path(
  getwd(), "Final2", "func", "mccrary.R"
))) {
  normalizePath(file.path(getwd(), "Final2"), winslash = "/", mustWork = TRUE)
} else if (file.exists(file.path(getwd(), "func", "mccrary.R"))) {
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
} else {
  normalizePath(file.path(getwd(), ".."), winslash = "/", mustWork = TRUE)
}

# Load only the shared data helpers. The finalized RunRD/table script remains
# independent and is not sourced by this diagnostic.
source(file.path(
  MCCRARY_FINAL_DIR,
  "func",
  "balanced_3month_data.R"
))
BALANCED_3MONTH_HELPERS <- load_balanced_3month_helpers()
trim_quarter_sample <- BALANCED_3MONTH_HELPERS$trim_quarter_sample

TRUE_CUTOFF <- 4.75
CANDIDATE_CUTOFFS <- seq(4.35, 4.95, by = 0.01)
OUTPUT_DIR <- file.path(
  MCCRARY_FINAL_DIR,
  "results",
  "McCrary"
)

extract_scalar <- function(x, name) {
  value <- x[[name]]
  if (is.null(value) || length(value) == 0L) return(NA_real_)
  as.numeric(value[[1L]])
}

run_mccrary_at <- function(x, cutoff) {
  fit <- tryCatch(
    rddensity::rddensity(
      X = x,
      c = cutoff,
      p = 2,
      fitselect = "unrestricted",
      kernel = "triangular",
      vce = "jackknife",
      massPoints = TRUE,
      bwselect = "comb",
      all = TRUE,
      bino = FALSE
    ),
    error = function(e) e
  )

  if (inherits(fit, "error")) {
    return(data.frame(
      cutoff = cutoff,
      density_left = NA_real_,
      density_right = NA_real_,
      density_jump = NA_real_,
      density_jump_se = NA_real_,
      density_jump_ci_low = NA_real_,
      density_jump_ci_high = NA_real_,
      relative_jump = NA_real_,
      t_jk = NA_real_,
      p_jk = NA_real_,
      bandwidth_left = NA_real_,
      bandwidth_right = NA_real_,
      n_left = sum(x < cutoff),
      n_right = sum(x >= cutoff),
      error = conditionMessage(fit)
    ))
  }

  density_left <- extract_scalar(fit$hat, "left")
  density_right <- extract_scalar(fit$hat, "right")
  density_jump <- extract_scalar(fit$hat, "diff")
  density_jump_se <- extract_scalar(fit$sd_jk, "diff")
  t_jk <- extract_scalar(fit$test, "t_jk")
  # Compute the two-sided p-value explicitly from the jackknife T statistic.
  # This also makes it impossible to confuse a T statistic with a p-value.
  p_jk <- if (is.finite(t_jk)) {
    2 * pnorm(-abs(t_jk))
  } else {
    NA_real_
  }

  data.frame(
    cutoff = cutoff,
    density_left = density_left,
    density_right = density_right,
    density_jump = density_jump,
    density_jump_se = density_jump_se,
    density_jump_ci_low =
      density_jump - qnorm(0.975) * density_jump_se,
    density_jump_ci_high =
      density_jump + qnorm(0.975) * density_jump_se,
    relative_jump = if (is.finite(density_left) && density_left != 0) {
      density_jump / density_left
    } else {
      NA_real_
    },
    t_jk = t_jk,
    p_jk = p_jk,
    bandwidth_left = extract_scalar(fit$h, "left"),
    bandwidth_right = extract_scalar(fit$h, "right"),
    n_left = sum(x < cutoff),
    n_right = sum(x >= cutoff),
    error = NA_character_
  )
}

run_true_cutoff_density <- function(
    x,
    output_dir = NULL,
    cutoff = TRUE_CUTOFF,
    show_plot = TRUE,
    save_figure = TRUE
) {
  fit <- rddensity::rddensity(
    X = x,
    c = cutoff,
    p = 2,
    fitselect = "unrestricted",
    kernel = "triangular",
    vce = "jackknife",
    massPoints = TRUE,
    bwselect = "comb",
    all = TRUE,
    bino = TRUE
  )

  plot_range <- c(
    max(min(x, na.rm = TRUE), cutoff - 0.50),
    min(max(x, na.rm = TRUE), cutoff + 0.20)
  )

  density_plot <- rddensity::rdplotdensity(
    fit,
    X = x,
    plotRange = plot_range,
    plotN = c(30, 30),
    plotGrid = "es",
    alpha = 0.05,
    type = "both",
    CItype = "region",
    CIuniform = FALSE,
    hist = TRUE,
    title = "McCrary density test at the true cutoff (4.75)",
    xlabel = "Running score",
    ylabel = "Density"
  )

  if (!is.null(density_plot$Estplot)) {
    density_plot$Estplot <- density_plot$Estplot +
      geom_vline(
        xintercept = 4.75,
        linetype = "dashed",
        color = "#7B3294",
        linewidth = 0.6
      )
  }

  if (!is.null(density_plot$Estplot) && save_figure) {
    if (is.null(output_dir)) {
      stop("output_dir must be supplied when save_figure = TRUE.")
    }
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    ggsave(
      file.path(output_dir, "density.eps"),
      density_plot$Estplot,
      device = grDevices::cairo_ps,
      width = 8,
      height = 5.5,
      onefile = FALSE,
      fallback_resolution = 600
    )
    ggsave(
      file.path(output_dir, "density.pdf"),
      density_plot$Estplot,
      device = grDevices::cairo_pdf,
      width = 8,
      height = 5.5,
      onefile = FALSE
    )

  }

  # rdplotdensity() stores its ggplot object in $Estplot. Explicit print()
  # draws it in the RStudio Plots pane or the active R graphics device.
  if (!is.null(density_plot$Estplot) && show_plot) {
    print(density_plot$Estplot)
  }

  list(
    fit = fit,
    plot = density_plot,
    x = x,
    cutoff = cutoff
  )
}

run_mccrary_rank <- function(
    output_dir = OUTPUT_DIR,
    candidate_cutoffs = CANDIDATE_CUTOFFS,
    show_true_cutoff_plot = TRUE,
    save_figures = TRUE,
    refresh_data = FALSE
) {
  if (save_figures) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }

  # This matches the review_min = 0, (avg_price trim, price_diff trim) =
  # (0, 0) cell in balanced-3month.R.
  quarter_listing <- get_balanced_3month_derived_data(
    key = "mccrary_quarter_listing_v1",
    force = refresh_data,
    build = function(quarterly_panel) {
      quarterly_panel %>%
        filter(
          !is.na(first_month_ltm),
          first_month_ltm >= 1,
          is.finite(price_diff)
        ) %>%
        trim_quarter_sample(
          avg_price_pct = 0.01,
          price_diff_pct = 0
        ) %>%
        filter(
          !is.na(first_month_number_of_reviews),
          first_month_number_of_reviews >= 0,
          !is.na(host_id),
          is.finite(running_scr)
        )
    }
  )

  # running_scr is a host-quarter variable. Check that it is actually
  # constant across a host's listings before retaining one observation per
  # (quarter, host_id). This avoids mechanically overweighting multi-listing
  # hosts in the density test.
  host_score_conflicts <- quarter_listing %>%
    group_by(quarter, host_id) %>%
    summarise(
      n_listings = n_distinct(id),
      n_running_scores = n_distinct(running_scr),
      min_running_scr = min(running_scr),
      max_running_scr = max(running_scr),
      .groups = "drop"
    ) %>%
    filter(n_running_scores != 1L)

  if (nrow(host_score_conflicts) > 0L) {
    stop(
      "running_scr is not unique within some (quarter, host_id) cells. ",
      "Inspect the returned host-quarter construction."
    )
  }

  host_quarter <- quarter_listing %>%
    arrange(quarter, host_id, id) %>%
    group_by(quarter, host_id) %>%
    summarise(
      running_scr = first(running_scr),
      representative_id = first(id),
      n_listings = n_distinct(id),
      .groups = "drop"
    )

  stopifnot(!anyDuplicated(host_quarter[c("quarter", "host_id")]))

  x <- host_quarter$running_scr
  candidate_cutoffs <- sort(unique(round(
    c(candidate_cutoffs, TRUE_CUTOFF),
    digits = 8L
  )))
  candidate_cutoffs <- candidate_cutoffs[
    candidate_cutoffs > min(x) & candidate_cutoffs < max(x)
  ]

  scan <- bind_rows(lapply(
    candidate_cutoffs,
    function(cutoff) {
      cat(sprintf("[McCrary] cutoff=%.2f\n", cutoff))
      run_mccrary_at(x, cutoff)
    }
  )) %>%
    mutate(
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
      reject_10pct = is.finite(p_jk) & p_jk < 0.10,
      reject_5pct = is.finite(p_jk) & p_jk < 0.05,
      reject_1pct = is.finite(p_jk) & p_jk < 0.01,
      abs_t_jk = abs(t_jk),
      abs_density_jump = abs(density_jump)
    ) %>%
    mutate(
      rank_abs_t = if_else(
        eligible_for_rank,
        min_rank(desc(if_else(
          eligible_for_rank,
          abs_t_jk,
          NA_real_
        ))),
        NA_integer_
      ),
      percentile_abs_t = 100 * (
        sum(eligible_for_rank & is.finite(abs_t_jk)) -
          rank_abs_t + 1
      ) / sum(eligible_for_rank & is.finite(abs_t_jk)),
      rank_abs_density_jump = if_else(
        eligible_for_rank,
        min_rank(desc(if_else(
          eligible_for_rank,
          abs_density_jump,
          NA_real_
        ))),
        NA_integer_
      ),
      percentile_abs_density_jump =
        100 * (
          sum(
            eligible_for_rank &
              is.finite(abs_density_jump)
          ) -
            rank_abs_density_jump + 1
        ) / sum(
          eligible_for_rank &
            is.finite(abs_density_jump)
        )
    ) %>%
    arrange(cutoff)

  if (any(scan$p_jk < 0 | scan$p_jk > 1, na.rm = TRUE)) {
    stop("Internal error: a computed p_jk value is outside [0, 1].")
  }

  bandwidth_audit <- scan %>%
    select(
      cutoff,
      bandwidth_left,
      bandwidth_right,
      bandwidth_left_endpoint,
      bandwidth_right_endpoint,
      bandwidth_contains_true_cutoff
    )

  true_result <- scan %>%
    filter(is_true_cutoff)

  if (nrow(true_result) != 1L) {
    stop("The cutoff scan must contain exactly one true-cutoff row.")
  }

  # After the cutoff loop, rerun rddensity only at 4.75 on exactly the same
  # pooled, unique (quarter, host_id) running-score sample.
  true_cutoff_density <- run_true_cutoff_density(
    x = x,
    output_dir = output_dir,
    cutoff = TRUE_CUTOFF,
    show_plot = show_true_cutoff_plot,
    save_figure = save_figures
  )

  true_rank_label <- sprintf(
    "4.75: |T| rank %d/%d",
    true_result$rank_abs_t,
    sum(scan$eligible_for_rank & is.finite(scan$t_jk))
  )
  true_jump_rank_label <- sprintf(
    "4.75: |jump| rank %d/%d",
    true_result$rank_abs_density_jump,
    sum(
      scan$eligible_for_rank &
        is.finite(scan$density_jump)
    )
  )

  jump_plot <- ggplot(
    scan %>% filter(
      eligible_for_rank,
      is.finite(density_jump),
      is.finite(density_jump_ci_low),
      is.finite(density_jump_ci_high)
    ),
    aes(x = cutoff, y = density_jump)
  ) +
    geom_hline(yintercept = 0, color = "grey35", linewidth = 0.5) +
    geom_ribbon(
      aes(
        ymin = density_jump_ci_low,
        ymax = density_jump_ci_high
      ),
      fill = "#92C5DE",
      alpha = 0.35
    ) +
    geom_line(color = "#2166AC", linewidth = 0.55) +
    geom_point(color = "#2166AC", size = 1.3) +
    geom_point(
      data = scan %>% filter(
        bandwidth_contains_true_cutoff,
        is.finite(density_jump)
      ),
      aes(x = cutoff, y = density_jump),
      color = "#D6604D",
      shape = 4,
      stroke = 1,
      size = 2.4
    ) +
    geom_vline(
      xintercept = TRUE_CUTOFF,
      linetype = "dotted",
      linewidth = 0.7
    ) +
    geom_point(
      data = true_result,
      aes(x = cutoff, y = density_jump),
      color = "#D6604D",
      size = 3
    ) +
    annotate(
      "label",
      x = TRUE_CUTOFF,
      y = true_result$density_jump,
      label = true_jump_rank_label,
      hjust = if (TRUE_CUTOFF > mean(range(scan$cutoff))) 1.05 else -0.05,
      vjust = -0.7,
      size = 3.3
    ) +
    labs(
      title = "Estimated density jump across candidate cutoffs",
      subtitle = "Shaded region is the pointwise 95% confidence interval",
      x = "Candidate cutoff",
      y = "Estimated density jump"
    ) +
    theme_minimal(base_size = 11)

  rejection_summary <- scan %>%
    filter(is_placebo, eligible_for_rank) %>%
    summarise(
      cutoff_group = "placebos_excluding_bandwidths_crossing_4.75",
      n_tests = sum(is.finite(p_jk)),
      rejection_rate_10pct = mean(reject_10pct[is.finite(p_jk)]),
      rejection_rate_5pct = mean(reject_5pct[is.finite(p_jk)]),
      rejection_rate_1pct = mean(reject_1pct[is.finite(p_jk)])
    )

  sample_audit <- host_quarter %>%
    group_by(quarter) %>%
    summarise(
      host_quarters = n(),
      unique_hosts = n_distinct(host_id),
      unique_running_scores = n_distinct(running_scr),
      multi_listing_host_quarters = sum(n_listings > 1L),
      .groups = "drop"
    )

  if (save_figures) {
    rank_t_denominator <- sum(
      scan$eligible_for_rank & is.finite(scan$t_jk)
    )
    rank_jump_denominator <- sum(
      scan$eligible_for_rank & is.finite(scan$density_jump)
    )
    excluded_placebos <- sum(
      scan$bandwidth_contains_true_cutoff,
      na.rm = TRUE
    )

    true_cutoff_tex <- c(
      "\\begin{table}[htbp]",
      "\\centering",
      paste0(
        "\\caption{Density Discontinuity Test at the ",
        "True Superhost Cutoff}"
      ),
      "\\label{tab:mccrary_475}",
      "\\begin{tabular}{lr}",
      "\\toprule",
      "Statistic & Value \\\\",
      "\\midrule",
      sprintf(
        "Estimated density jump & $%.3f$ \\\\",
        true_result$density_jump
      ),
      sprintf(
        "Jackknife standard error & $%.3f$ \\\\",
        true_result$density_jump_se
      ),
      sprintf(
        "Jackknife $T$ statistic & $%.3f$ \\\\",
        true_result$t_jk
      ),
      sprintf(
        "Two-sided $p$-value & $%.3f$ \\\\",
        true_result$p_jk
      ),
      sprintf(
        "Rank by absolute $T$ statistic & %d of %d \\\\",
        true_result$rank_abs_t,
        rank_t_denominator
      ),
      sprintf(
        paste0(
          "Rank by absolute density jump & ",
          "%d of %d \\\\"
        ),
        true_result$rank_abs_density_jump,
        rank_jump_denominator
      ),
      "\\bottomrule",
      "\\end{tabular}",
      "\\begin{minipage}{0.90\\textwidth}",
      "\\footnotesize",
      paste0(
        "\\textit{Notes:} Rankings compare the cutoff of ",
        sprintf("%.2f", TRUE_CUTOFF),
        " with eligible candidate cutoffs ranging from ",
        sprintf("%.2f", min(scan$cutoff)),
        " to ",
        sprintf("%.2f", max(scan$cutoff)),
        " in increments of 0.01. Rank 1 corresponds to the largest ",
        "absolute statistic. ",
        excluded_placebos,
        " placebo cutoff",
        if (excluded_placebos == 1L) "" else "s",
        " whose selected bandwidth contains ",
        sprintf("%.2f", TRUE_CUTOFF),
        if (excluded_placebos == 1L) " is" else " are",
        " excluded from the rankings."
      ),
      "\\end{minipage}",
      "\\end{table}"
    )

    writeLines(
      true_cutoff_tex,
      file.path(output_dir, "McCrary.tex")
    )

    ggsave(
      file.path(output_dir, "jump.eps"),
      jump_plot,
      device = grDevices::cairo_ps,
      width = 9,
      height = 5.5,
      onefile = FALSE,
      fallback_resolution = 600
    )
    ggsave(
      file.path(output_dir, "jump.pdf"),
      jump_plot,
      device = grDevices::cairo_pdf,
      width = 9,
      height = 5.5,
      onefile = FALSE
    )
  }

  cat("\n=== McCrary test at cutoff 4.75 ===\n")
  cat(sprintf("density jump       : %.6f\n", true_result$density_jump))
  cat(sprintf("jackknife SE       : %.6f\n", true_result$density_jump_se))
  cat(sprintf("jackknife T        : %.6f\n", true_result$t_jk))
  cat(sprintf("jackknife p-value  : %.6g\n", true_result$p_jk))
  cat(sprintf(
    "|T| rank           : %d / %d\n",
    true_result$rank_abs_t,
    sum(scan$eligible_for_rank & is.finite(scan$t_jk))
  ))
  cat(sprintf(
    "|jump| rank        : %d / %d\n",
    true_result$rank_abs_density_jump,
    sum(
      scan$eligible_for_rank &
        is.finite(scan$density_jump)
    )
  ))
  cat(sprintf(
    "placebos excluded  : %d (bandwidth crossed 4.75)\n",
    sum(scan$bandwidth_contains_true_cutoff, na.rm = TRUE)
  ))
  cat(sprintf(
    "excluded cutoffs   : %s\n",
    paste(
      format(
        scan$cutoff[scan$bandwidth_contains_true_cutoff],
        nsmall = 2
      ),
      collapse = ", "
    )
  ))
  cat("\n=== Does each selected bandwidth contain 4.75? ===\n")
  print(as.data.frame(bandwidth_audit), row.names = FALSE)
  cat("\n=== Placebo rejection rates ===\n")
  print(rejection_summary)

  invisible(list(
    host_quarter = host_quarter,
    scan = scan,
    bandwidth_audit = bandwidth_audit,
    true_result = true_result,
    rejection_summary = rejection_summary,
    true_cutoff_fit = true_cutoff_density$fit,
    true_cutoff_plot = true_cutoff_density$plot,
    jump_plot = jump_plot
  ))
}

# Simple user-facing entry point. The balanced three-month data are built only
# when quarterly_panel is absent from the global environment.
mccrary <- function(
    output_dir = OUTPUT_DIR,
    candidate_cutoffs = CANDIDATE_CUTOFFS,
    show_true_cutoff_plot = TRUE,
    save_figures = TRUE,
    refresh_data = FALSE
) {
  run_mccrary_rank(
    output_dir = output_dir,
    candidate_cutoffs = candidate_cutoffs,
    show_true_cutoff_plot = show_true_cutoff_plot,
    save_figures = save_figures,
    refresh_data = refresh_data
  )
}

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  output_dir <- if (length(args) >= 1L) args[[1L]] else OUTPUT_DIR
  mccrary(output_dir = output_dir)
}
