options(stringsAsFactors = FALSE)

placebo <- function(
    output_dir = NULL,
    refresh_data = FALSE
) {
  project_dir <- if (file.exists(file.path(
    getwd(), "func", "placebo.R"
  ))) {
    normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  } else if (file.exists(file.path(
    getwd(), "Final2", "func", "placebo.R"
  ))) {
    normalizePath(file.path(getwd(), "Final2"), winslash = "/", mustWork = TRUE)
  } else {
    stop("Run placebo() from the project root or the Final2 directory.")
  }

  .libPaths(c(file.path(project_dir, ".Rlib"), .libPaths()))
  required_packages <- c("dplyr", "rdrobust", "ggplot2")
  missing_packages <- required_packages[
    !vapply(required_packages, requireNamespace, logical(1L), quietly = TRUE)
  ]
  if (length(missing_packages) > 0L) {
    stop("Missing packages: ", paste(missing_packages, collapse = ", "))
  }

  source(file.path(project_dir, "func", "balanced_3month_data.R"))
  data_helpers <- suppressMessages(load_balanced_3month_helpers())
  trim_quarter_sample <- data_helpers$trim_quarter_sample
  target_quarters <- data_helpers$target_quarters

  TRUE_CUTOFF <- 4.75
  REVIEW_MIN <- 30L
  GRID_STEP <- 0.001
  TARGET_PER_SIDE <- 30L
  BWSELECT <- "msetwo"
  MASSPOINTS <- "adjust"
  OUTPUT_DIR <- if (is.null(output_dir)) {
    file.path(
      project_dir,
      "results",
      "placebo"
    )
  } else {
    output_dir
  }
  dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

  normalize_binary <- function(x) {
    if (is.logical(x)) return(as.numeric(x))
    if (is.numeric(x) || is.integer(x)) return(as.numeric(x))

    value <- tolower(trimws(as.character(x)))
    result <- rep(NA_real_, length(value))
    result[value %in% c("1", "t", "true", "yes", "y")] <- 1
    result[value %in% c("0", "f", "false", "no", "n")] <- 0
    result
  }

  build_placebo_base_sample <- function(refresh_data = FALSE) {
    get_balanced_3month_derived_data(
      key = "msetwo_outside_placebo_base_sample_v1",
      force = refresh_data,
      build = function(quarterly_panel) {
        dplyr::filter(
          quarterly_panel,
          !is.na(first_month_ltm),
          first_month_ltm >= 1,
          is.finite(price_diff)
        )
      }
    )
  }

  prepare_placebo_sample <- function(base_sample, panel, review_min) {
    threshold_sample <- dplyr::filter(
      base_sample,
      !is.na(first_month_number_of_reviews),
      first_month_number_of_reviews >= review_min
    )
    invisible(utils::capture.output(
      threshold_sample <- suppressMessages(trim_quarter_sample(
        threshold_sample,
        avg_price_pct = 0.01,
        price_diff_pct = 0
      ))
    ))
    panel_value <- if (panel == "A") "t" else "f"
    threshold_sample <- dplyr::filter(
      threshold_sample,
      !is.na(ex_super),
      is.finite(running_scr),
      is.finite(price_diff),
      !is.na(id)
    )
    threshold_sample <- dplyr::mutate(
      threshold_sample,
      quarter = factor(quarter, levels = target_quarters),
      .fuzzy_treatment = normalize_binary(host_is_superhost2)
    )
    threshold_sample <- dplyr::filter(
      threshold_sample,
      is.finite(.fuzzy_treatment),
      ex_super == panel_value
    )
    dplyr::mutate(
      threshold_sample,
      quarter = droplevels(quarter)
    )
  }

  make_rd_args <- function(data, cutoff) {
    quarter_dummies <- as.data.frame(
      stats::model.matrix(~ quarter - 1, data = data)
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
      p = 1
    )
  }

split_sample <- function(data, cutoff) {
  if (cutoff < TRUE_CUTOFF) {
    return(data[data$running_scr < TRUE_CUTOFF, , drop = FALSE])
  }
  if (cutoff > TRUE_CUTOFF) {
    return(data[data$running_scr > TRUE_CUTOFF, , drop = FALSE])
  }
  data
}

fit_cutoff <- function(data, cutoff) {
  fit_data <- split_sample(data, cutoff)
  args <- make_rd_args(fit_data, cutoff)
  args$bwrestrict <- TRUE
  args$masspoints <- MASSPOINTS
  fit <- tryCatch(
    suppressWarnings(do.call(
      rdrobust::rdrobust,
      c(args, list(bwselect = BWSELECT))
    )),
    error = function(error) error
  )

  if (inherits(fit, "error")) {
    return(data.frame(
      cutoff = cutoff,
      distance = round(abs(cutoff - TRUE_CUTOFF), 8L),
      side = sign(cutoff - TRUE_CUTOFF),
      conv = NA_real_,
      conv_se = NA_real_,
      conv_ci_low = NA_real_,
      conv_ci_high = NA_real_,
      h_left = NA_real_,
      h_right = NA_real_,
      n_left = NA_integer_,
      n_right = NA_integer_,
      error = conditionMessage(fit)
    ))
  }

  data.frame(
    cutoff = cutoff,
    distance = round(abs(cutoff - TRUE_CUTOFF), 8L),
    side = sign(cutoff - TRUE_CUTOFF),
    conv = as.numeric(fit$Estimate[[1L]]),
    conv_se = as.numeric(fit$se[[1L]]),
    conv_ci_low = as.numeric(fit$ci[1L, 1L]),
    conv_ci_high = as.numeric(fit$ci[1L, 2L]),
    h_left = as.numeric(fit$bws["h", "left"]),
    h_right = as.numeric(fit$bws["h", "right"]),
    n_left = as.integer(fit$N_h[[1L]]),
    n_right = as.integer(fit$N_h[[2L]]),
    error = NA_character_
  )
}

first_grid_point_outside <- function(bandwidth) {
  (floor(bandwidth / GRID_STEP) + 1L) * GRID_STEP
}

collect_successful_fits <- function(data, side, bandwidth) {
  first_distance <- first_grid_point_outside(bandwidth)
  support_max <- if (side < 0) 0.749 else 0.249
  candidate_distances <- seq(
    first_distance,
    support_max,
    by = GRID_STEP
  )
  successful <- list()
  successful_count <- 0L

  for (distance in candidate_distances) {
    cutoff <- TRUE_CUTOFF + side * distance
    result <- fit_cutoff(data, cutoff)
    if (
      is.na(result$error) &&
        is.finite(result$conv) &&
        is.finite(result$conv_ci_low) &&
        is.finite(result$conv_ci_high)
    ) {
      successful_count <- successful_count + 1L
      successful[[successful_count]] <- result
      if (successful_count == TARGET_PER_SIDE) break
    }
  }

  if (successful_count < TARGET_PER_SIDE) {
    stop(
      "Only ", successful_count, " successful fits were available on side ",
      side, " outside bandwidth ", sprintf("%.6f", bandwidth), "."
    )
  }
  dplyr::bind_rows(successful)
}

make_plot <- function(
  results,
  h_left,
  h_right
) {
  plot_data <- results[order(results$cutoff), , drop = FALSE]
  plot_data$outside_distance <- ifelse(
    plot_data$side < 0,
    -(plot_data$distance - h_left),
    ifelse(
      plot_data$side > 0,
      plot_data$distance - h_right,
      0
    )
  )
  center_gap <- 0.002
  plot_data$plot_x <- ifelse(
    plot_data$side < 0,
    plot_data$outside_distance - center_gap,
    ifelse(
      plot_data$side > 0,
      plot_data$outside_distance + center_gap,
      0
    )
  )
  axis_labels <- pretty(range(plot_data$outside_distance), n = 7L)
  axis_labels <- axis_labels[
    axis_labels >= min(plot_data$outside_distance) &
      axis_labels <= max(plot_data$outside_distance)
  ]
  axis_labels <- sort(unique(c(axis_labels, 0)))
  axis_breaks <- axis_labels + ifelse(
    axis_labels < 0,
    -center_gap,
    ifelse(axis_labels > 0, center_gap, 0)
  )
  plot_data$group <- factor(
    ifelse(
      plot_data$side < 0,
      "Left placebo",
      ifelse(plot_data$side > 0, "Right placebo", "True cutoff")
    ),
    levels = c("Left placebo", "Right placebo", "True cutoff")
  )
  true_row <- plot_data[plot_data$side == 0, , drop = FALSE]

  ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = plot_x, y = conv, color = group)
  ) +
    ggplot2::geom_hline(
      yintercept = 0,
      linetype = "dashed",
      color = "grey45",
      linewidth = 0.4
    ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = conv_ci_low, ymax = conv_ci_high),
      width = 0.0005,
      linewidth = 0.35,
      alpha = 0.65
    ) +
    ggplot2::geom_point(size = 1.0) +
    ggplot2::geom_vline(
      xintercept = 0,
      linetype = "dotted",
      color = "black",
      linewidth = 0.55
    ) +
    ggplot2::geom_point(
      data = true_row,
      size = 2.6,
      shape = 18,
      color = "#B2182B"
    ) +
    ggplot2::scale_color_manual(values = c(
      "Left placebo" = "#2166AC",
      "Right placebo" = "#1B7837",
      "True cutoff" = "#B2182B"
    )) +
    ggplot2::scale_x_continuous(
      breaks = axis_breaks,
      labels = sprintf("%.3f", axis_labels)
    ) +
    ggplot2::labs(
      x = paste0(
        "Signed distance beyond optimal bandwidth ",
        "(left < 0, right > 0)"
      ),
      y = "Conventional RD estimate",
      color = NULL
    ) +
    ggplot2::coord_cartesian(ylim = c(-0.3, 0.3)) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      legend.position = "top",
      plot.margin = ggplot2::margin(10, 12, 12, 10)
    )
}

run_panel <- function(base_sample, panel) {
  analysis_sample <- prepare_placebo_sample(
    base_sample,
    panel,
    REVIEW_MIN
  )
  true_fit <- fit_cutoff(analysis_sample, TRUE_CUTOFF)
  if (!is.na(true_fit$error)) {
    stop("True-cutoff fit failed for Panel ", panel, ": ", true_fit$error)
  }
  h_left <- true_fit$h_left
  h_right <- true_fit$h_right

  left_fits <- collect_successful_fits(analysis_sample, -1, h_left)
  right_fits <- collect_successful_fits(analysis_sample, 1, h_right)
  panel_lower <- tolower(panel)
  results <- dplyr::bind_rows(
    true_fit,
    left_fits,
    right_fits
  )
  rank <- 1L + sum(
    results$conv[results$side != 0] < true_fit$conv
  )
  denominator <- nrow(results)
  plot <- make_plot(
    results,
    h_left,
    h_right
  )

  eps_file <- file.path(
    OUTPUT_DIR,
    sprintf("placebo_%s.eps", panel_lower)
  )
  pdf_file <- file.path(
    OUTPUT_DIR,
    sprintf("placebo_%s.pdf", panel_lower)
  )
  ggplot2::ggsave(
    eps_file,
    plot,
    device = grDevices::cairo_ps,
    width = 12,
    height = 8.5,
    units = "in",
    onefile = FALSE,
    fallback_resolution = 600
  )
  ggplot2::ggsave(
    pdf_file,
    plot,
    device = grDevices::cairo_pdf,
    width = 12,
    height = 8.5,
    units = "in",
    onefile = FALSE
  )

  cat(sprintf(
    "Panel %s conventional rank: %d/%d\n",
    panel,
    rank,
    denominator
  ))

  invisible(list(
    panel = panel,
    h_left = h_left,
    h_right = h_right,
    results = results,
    true_estimate = true_fit$conv,
    true_se = true_fit$conv_se,
    conventional_rank = rank,
    denominator = denominator,
    eps_file = eps_file,
    pdf_file = pdf_file
  ))
}

  invisible(utils::capture.output(
    base_sample <- suppressMessages(build_placebo_base_sample(
      refresh_data = refresh_data
    ))
  ))
  outputs <- list(
    panel_a = run_panel(base_sample, "A"),
    panel_b = run_panel(base_sample, "B")
  )

  table_file <- file.path(OUTPUT_DIR, "placebo.tex")
  table_lines <- c(
    "\\begin{table}[htbp]",
    "\\centering",
    "\\caption{Placebo Estimates at the True Superhost Cutoff}",
    "\\label{tab:placebo_true_cutoff}",
    "\\begin{tabular}{lrrr}",
    "\\toprule",
    "Panel & Estimate & Standard error & Rank \\\\",
    "\\midrule",
    sprintf(
      "A & $%.3f$ & $%.3f$ & %d of %d \\\\",
      outputs$panel_a$true_estimate,
      outputs$panel_a$true_se,
      outputs$panel_a$conventional_rank,
      outputs$panel_a$denominator
    ),
    sprintf(
      "B & $%.3f$ & $%.3f$ & %d of %d \\\\",
      outputs$panel_b$true_estimate,
      outputs$panel_b$true_se,
      outputs$panel_b$conventional_rank,
      outputs$panel_b$denominator
    ),
    "\\bottomrule",
    "\\end{tabular}",
    "\\end{table}"
  )
  writeLines(table_lines, table_file, useBytes = TRUE)
  outputs$table_file <- table_file

  invisible(outputs)
}
