options(stringsAsFactors = FALSE)

placebo <- function(
    output_dir = NULL,
    targets_per_side = c(15L, 20L),
    review_min = 30L,
    grid_step = 0.001,
    true_cutoff = 4.75,
    bwselect = "msetwo",
    masspoints = "adjust",
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
  data_helpers <- load_balanced_3month_helpers()
  trim_quarter_sample <- data_helpers$trim_quarter_sample
  target_quarters <- data_helpers$target_quarters

  TRUE_CUTOFF <- true_cutoff
  REVIEW_MIN <- as.integer(review_min)
  GRID_STEP <- grid_step
  TARGETS_PER_SIDE <- as.integer(targets_per_side)
  MAX_TARGET_PER_SIDE <- max(TARGETS_PER_SIDE)
  BWSELECT <- bwselect
  MASSPOINTS <- masspoints
  OUTPUT_DIR <- if (is.null(output_dir)) {
    file.path(
      project_dir,
      "results",
      "msetwo_outside_bw_001_15_20_bc"
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
    threshold_sample <- trim_quarter_sample(
      threshold_sample,
      avg_price_pct = 0.01,
      price_diff_pct = 0
    )
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
      conv_ci_low = NA_real_,
      conv_ci_high = NA_real_,
      bc = NA_real_,
      bc_ci_low = NA_real_,
      bc_ci_high = NA_real_,
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
    conv_ci_low = as.numeric(fit$ci[1L, 1L]),
    conv_ci_high = as.numeric(fit$ci[1L, 2L]),
    bc = as.numeric(fit$Estimate[[2L]]),
    bc_ci_low = as.numeric(fit$ci[2L, 1L]),
    bc_ci_high = as.numeric(fit$ci[2L, 2L]),
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
        is.finite(result$bc) &&
        is.finite(result$bc_ci_low) &&
        is.finite(result$bc_ci_high)
    ) {
      successful_count <- successful_count + 1L
      successful[[successful_count]] <- result
      if (successful_count == MAX_TARGET_PER_SIDE) break
    }
  }

  if (successful_count < MAX_TARGET_PER_SIDE) {
    stop(
      "Only ", successful_count, " successful fits were available on side ",
      side, " outside bandwidth ", sprintf("%.6f", bandwidth), "."
    )
  }
  dplyr::bind_rows(successful)
}

make_plot <- function(
  results,
  panel,
  target_per_side,
  h_left,
  h_right,
  rank,
  denominator
) {
  plot_data <- results[order(results$cutoff), , drop = FALSE]
  plot_data$group <- factor(
    ifelse(
      plot_data$side < 0,
      "Left placebo",
      ifelse(plot_data$side > 0, "Right placebo", "True cutoff")
    ),
    levels = c("Left placebo", "Right placebo", "True cutoff")
  )
  true_row <- plot_data[plot_data$side == 0, , drop = FALSE]
  left_rows <- plot_data[plot_data$side < 0, , drop = FALSE]
  right_rows <- plot_data[plot_data$side > 0, , drop = FALSE]

  caption <- paste(
    sprintf(
      paste0(
        "Notes: Panel %s; first-month review count >= %d. Sharp RD with ",
        "cutoff-specific %s bandwidths; bias-corrected estimates and ",
        "bias-corrected 95%% confidence intervals."
      ),
      panel,
      REVIEW_MIN,
      BWSELECT
    ),
    sprintf(
      paste0(
        "At the true cutoff, h_left = %.6f and h_right = %.6f. ",
        "Starting at the first %.3f grid point strictly outside each ",
        "bandwidth, %d successful fits are retained per side."
      ),
      h_left,
      h_right,
      GRID_STEP,
      target_per_side
    ),
    sprintf(
      paste0(
        "Left distance range: [%.3f, %.3f]; right distance range: ",
        "[%.3f, %.3f]. True-cutoff bias-corrected rank: %d/%d (%.2f%%)."
      ),
      min(left_rows$distance),
      max(left_rows$distance),
      min(right_rows$distance),
      max(right_rows$distance),
      rank,
      denominator,
      100 * rank / denominator
    ),
    paste0(
      "Fake cutoffs below 4.75 use running_scr < 4.75 only; fake cutoffs ",
      "above 4.75 use running_scr > 4.75 only. Quarter indicators are ",
      "covariates; standard errors are clustered by listing ID."
    ),
    sprintf(
      paste0(
        "Triangular kernel; local linear p = 1; bwrestrict = TRUE; ",
        "masspoints = %s. Lower estimates rank first."
      ),
      MASSPOINTS
    ),
    sep = "\n"
  )
  caption <- paste(
    unlist(lapply(strsplit(caption, "\n", fixed = TRUE)[[1L]], strwrap, width = 150L)),
    collapse = "\n"
  )

  ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = cutoff, y = bc, color = group)
  ) +
    ggplot2::geom_hline(
      yintercept = 0,
      linetype = "dashed",
      color = "grey45",
      linewidth = 0.4
    ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = bc_ci_low, ymax = bc_ci_high),
      width = 0.0005,
      linewidth = 0.35,
      alpha = 0.65
    ) +
    ggplot2::geom_point(size = 1.8) +
    ggplot2::geom_vline(
      xintercept = TRUE_CUTOFF,
      linetype = "dotted",
      color = "black",
      linewidth = 0.55
    ) +
    ggplot2::geom_point(
      data = true_row,
      size = 3.2,
      shape = 18,
      color = "#B2182B"
    ) +
    ggplot2::annotate(
      "text",
      x = true_row$cutoff,
      y = true_row$bc,
      label = sprintf(
        "  4.75: %.3f; rank %d/%d",
        true_row$bc,
        rank,
        denominator
      ),
      hjust = 0,
      vjust = -1.1,
      size = 3.4,
      color = "#B2182B"
    ) +
    ggplot2::scale_color_manual(values = c(
      "Left placebo" = "#2166AC",
      "Right placebo" = "#1B7837",
      "True cutoff" = "#B2182B"
    )) +
    ggplot2::labs(
      title = sprintf(
        "Panel %s: placebo cutoffs outside the true-cutoff bandwidths",
        panel
      ),
      subtitle = paste0(
        "msetwo; 0.001 grid; ", target_per_side,
        " successful cutoffs per side; bias-corrected inference"
      ),
      x = "Candidate cutoff",
      y = "Bias-corrected RD estimate",
      color = NULL,
      caption = caption
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      legend.position = "top",
      plot.caption = ggplot2::element_text(
        hjust = 0,
        size = 7,
        lineheight = 1.05
      ),
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

  message(
    "[Panel ", panel, "] true-cutoff msetwo h_left=",
    sprintf("%.6f", h_left), ", h_right=", sprintf("%.6f", h_right)
  )
  left_fits <- collect_successful_fits(analysis_sample, -1, h_left)
  right_fits <- collect_successful_fits(analysis_sample, 1, h_right)
  panel_lower <- tolower(panel)
  target_outputs <- lapply(TARGETS_PER_SIDE, function(target_per_side) {
    results <- dplyr::bind_rows(
      true_fit,
      left_fits[seq_len(target_per_side), , drop = FALSE],
      right_fits[seq_len(target_per_side), , drop = FALSE]
    )
    rank <- 1L + sum(results$bc[results$side != 0] < true_fit$bc)
    denominator <- nrow(results)
    plot <- make_plot(
      results,
      panel,
      target_per_side,
      h_left,
      h_right,
      rank,
      denominator
    )

    eps_file <- file.path(
      OUTPUT_DIR,
      sprintf(
        "panel_%s_msetwo_outside_bw_001_%d_bc.eps",
        panel_lower,
        target_per_side
      )
    )
    csv_file <- file.path(
      OUTPUT_DIR,
      sprintf(
        "panel_%s_msetwo_outside_bw_001_%d_bc_results.csv",
        panel_lower,
        target_per_side
      )
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
    utils::write.csv(results, csv_file, row.names = FALSE)

    message(
      "[Panel ", panel, "] saved ", eps_file,
      "; bias-corrected rank ", rank, "/", denominator
    )
    list(
      target_per_side = target_per_side,
      results = results,
      bias_corrected_rank = rank,
      denominator = denominator,
      eps_file = eps_file,
      csv_file = csv_file
    )
  })
  names(target_outputs) <- paste0("n", TARGETS_PER_SIDE)

  invisible(list(
    panel = panel,
    h_left = h_left,
    h_right = h_right,
    outputs = target_outputs
  ))
}

  base_sample <- build_placebo_base_sample(refresh_data = refresh_data)
  outputs <- list(
    panel_a = run_panel(base_sample, "A"),
    panel_b = run_panel(base_sample, "B")
  )

  invisible(outputs)
}
