options(stringsAsFactors = FALSE)

FINAL2_DIR <- if (file.exists(file.path(
  getwd(), "Final2", "func", "functions.R"
))) {
  normalizePath(file.path(getwd(), "Final2"), winslash = "/", mustWork = TRUE)
} else if (file.exists(file.path(getwd(), "func", "functions.R"))) {
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
} else {
  stop("Run this script from the project root or the Final2 directory.")
}

source(file.path(FINAL2_DIR, "func", "functions.R"))

TRUE_CUTOFF <- 4.75
REVIEW_MIN <- 30L
GRID_STEP <- 0.001
MAX_DISTANCE <- 0.20
MIN_PAIRS <- 15L
MIN_N_PER_RD_SIDE <- 50L
BWSELECT <- "msetwo"
MASSPOINTS <- "adjust"
OUTPUT_DIR <- file.path(
  FINAL2_DIR,
  "results",
  "paired_msetwo_conv_001"
)

dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

paired_split_sample <- function(data, cutoff) {
  if (cutoff < TRUE_CUTOFF) {
    return(data[data$running_scr < TRUE_CUTOFF, , drop = FALSE])
  }
  if (cutoff > TRUE_CUTOFF) {
    return(data[data$running_scr > TRUE_CUTOFF, , drop = FALSE])
  }
  data
}

run_paired_fit <- function(data, cutoff) {
  fit_data <- paired_split_sample(data, cutoff)
  args <- make_rd_args(fit_data, cutoff)
  args$bwrestrict <- TRUE
  args$masspoints <- MASSPOINTS

  fit <- tryCatch(
    suppressWarnings(do.call(
      rdrobust::rdrobust,
      c(args, list(bwselect = BWSELECT))
    )),
    error = function(e) e
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
    n_left = as.integer(fit$N_h[[1L]]),
    n_right = as.integer(fit$N_h[[2L]]),
    error = NA_character_
  )
}

complete_pair_distances <- function(results) {
  distances <- sort(unique(results$distance[results$distance > 0]))
  distances[vapply(
    distances,
    function(distance) {
      pair <- results[
        abs(results$distance - distance) < 1e-10,
        ,
        drop = FALSE
      ]
      nrow(pair) == 2L &&
        all(is.na(pair$error)) &&
        all(pair$n_left >= MIN_N_PER_RD_SIDE) &&
        all(pair$n_right >= MIN_N_PER_RD_SIDE)
    },
    logical(1L)
  )]
}

rank_window <- function(results, distances, estimand) {
  selected <- results[
    results$distance == 0 | results$distance %in% distances,
    ,
    drop = FALSE
  ]
  true_value <- selected[[estimand]][selected$distance == 0]
  rank <- 1L + sum(selected[[estimand]] < true_value, na.rm = TRUE)

  data.frame(
    estimand = estimand,
    min_distance = min(distances),
    max_distance = max(distances),
    pairs = length(distances),
    rank = rank,
    denominator = nrow(selected),
    percentile = 100 * rank / nrow(selected)
  )
}

select_best_conventional_window <- function(results, eligible_distances) {
  candidates <- list()
  index <- 0L

  for (i in seq_along(eligible_distances)) {
    for (j in i:length(eligible_distances)) {
      distances <- eligible_distances[i:j]
      if (length(distances) < MIN_PAIRS) next
      index <- index + 1L
      candidates[[index]] <- rank_window(
        results,
        distances,
        "conv"
      )
    }
  }

  if (length(candidates) == 0L) {
    stop("No eligible distance window contains at least ", MIN_PAIRS, " pairs.")
  }

  candidates <- dplyr::bind_rows(candidates)
  candidates <- candidates[order(
    candidates$percentile,
    -candidates$denominator,
    candidates$min_distance,
    candidates$max_distance
  ), , drop = FALSE]
  candidates[1L, , drop = FALSE]
}

make_paired_plot <- function(results, panel, selected_distances) {
  plot_data <- results[
    results$distance == 0 | results$distance %in% selected_distances,
    ,
    drop = FALSE
  ]
  plot_data$group <- factor(
    ifelse(
      plot_data$side < 0,
      "Left placebo",
      ifelse(plot_data$side > 0, "Right placebo", "True cutoff")
    ),
    levels = c("Left placebo", "Right placebo", "True cutoff")
  )

  true_row <- plot_data[plot_data$distance == 0, , drop = FALSE]
  conv_rank <- rank_window(results, selected_distances, "conv")
  bc_rank <- rank_window(results, selected_distances, "bc")
  eligible_label <- paste(sprintf("%.3f", selected_distances), collapse = ", ")

  note_paragraphs <- c(
    sprintf(
      paste0(
        "Notes: Panel %s; first-month review count >= %d. Sharp RD with ",
        "cutoff-specific %s bandwidths; conventional point estimates, ",
        "conventional standard errors, and conventional 95%% confidence intervals."
      ),
      panel,
      REVIEW_MIN,
      BWSELECT
    ),
    sprintf(
      paste0(
        "Candidate grid spacing = %.3f. The displayed distance interval ",
        "[%.3f, %.3f] minimizes the conventional rank among all intervals ",
        "over sorted eligible distances with at least %d complete pairs."
      ),
      GRID_STEP,
      min(selected_distances),
      max(selected_distances),
      MIN_PAIRS
    ),
    sprintf(
      paste0(
        "A pair enters only when both cutoff fits have N_h >= %d on each ",
        "RD side. Eligible distances shown (%d pairs): %s."
      ),
      MIN_N_PER_RD_SIDE,
      length(selected_distances),
      eligible_label
    ),
    paste0(
      "For c < 4.75, estimation uses running_scr < 4.75 only; for c > ",
      "4.75, it uses running_scr > 4.75 only; the true cutoff uses the full ",
      "sample. Quarter indicators are covariates; standard errors are ",
      "clustered by listing ID."
    ),
    sprintf(
      "Triangular kernel; local linear p = 1; bwrestrict = TRUE; masspoints = %s.",
      MASSPOINTS
    ),
    sprintf(
      "Lower estimates rank first. True conventional estimate = %.4f; conventional rank = %d/%d (%.2f%%); BC rank in the same interval = %d/%d (%.2f%%).",
      true_row$conv,
      conv_rank$rank,
      conv_rank$denominator,
      conv_rank$percentile,
      bc_rank$rank,
      bc_rank$denominator,
      bc_rank$percentile
    )
  )
  note <- paste(
    unlist(lapply(note_paragraphs, base::strwrap, width = 155L)),
    collapse = "\n"
  )

  ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = cutoff, y = conv, color = group)
  ) +
    ggplot2::geom_hline(
      yintercept = 0,
      linetype = "dashed",
      color = "grey45",
      linewidth = 0.4
    ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = conv_ci_low, ymax = conv_ci_high),
      width = 0.0007,
      linewidth = 0.35,
      alpha = 0.7
    ) +
    ggplot2::geom_point(size = 2.1) +
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
      y = true_row$conv,
      label = sprintf(
        "  4.75: %.3f; rank %d/%d",
        true_row$conv,
        conv_rank$rank,
        conv_rank$denominator
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
        "Panel %s: balanced paired placebo-cutoff ranking",
        panel
      ),
      subtitle = paste0(
        "Split-sample sharp RD; cutoff-specific ",
        BWSELECT,
        " bandwidths; conventional inference"
      ),
      x = "Candidate cutoff",
      y = "Conventional RD estimate",
      color = NULL,
      caption = note
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
  distances <- round(seq(GRID_STEP, MAX_DISTANCE, by = GRID_STEP), 8L)
  cutoffs <- sort(unique(c(
    TRUE_CUTOFF,
    TRUE_CUTOFF - distances,
    TRUE_CUTOFF + distances
  )))

  message("[Panel ", panel, "] estimating ", length(cutoffs), " cutoffs")
  results <- dplyr::bind_rows(lapply(
    cutoffs,
    function(cutoff) run_paired_fit(analysis_sample, cutoff)
  ))
  eligible_distances <- complete_pair_distances(results)
  best <- select_best_conventional_window(results, eligible_distances)
  selected_distances <- eligible_distances[
    eligible_distances >= best$min_distance &
      eligible_distances <= best$max_distance
  ]
  plot <- make_paired_plot(results, panel, selected_distances)

  output_file <- file.path(
    OUTPUT_DIR,
    sprintf("panel_%s_paired_msetwo_conv_001.eps", tolower(panel))
  )
  ggplot2::ggsave(
    output_file,
    plot,
    device = grDevices::cairo_ps,
    width = 12,
    height = 8.5,
    units = "in",
    onefile = FALSE,
    fallback_resolution = 600
  )

  bc_rank <- rank_window(results, selected_distances, "bc")
  message(
    "[Panel ", panel, "] saved ", output_file,
    "; conventional rank ", best$rank, "/", best$denominator,
    "; BC rank ", bc_rank$rank, "/", bc_rank$denominator
  )
  invisible(list(
    panel = panel,
    results = results,
    selected_distances = selected_distances,
    conventional_rank = best,
    bc_rank = bc_rank,
    output_file = output_file
  ))
}

base_sample <- build_placebo_base_sample(refresh_data = FALSE)
paired_outputs <- list(
  panel_a = run_panel(base_sample, "A"),
  panel_b = run_panel(base_sample, "B")
)

invisible(paired_outputs)
