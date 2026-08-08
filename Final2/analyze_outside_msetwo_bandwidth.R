source(file.path("Final2", "func", "functions.R"))

true_cutoff <- 4.75
review_min <- 30L
target_counts <- c(20L, 25L, 30L)
grid_steps <- c(0.001, 0.005, 0.010)
bandwidths <- list(
  A = c(left = 0.155430, right = 0.0886639),
  B = c(left = 0.255549, right = 0.1139991)
)
result_dir <- file.path("Final2", "results", "paired_msetwo_conv_001")

load_existing <- function(panel) {
  panel_lower <- tolower(panel)
  files <- c(
    sprintf("panel_%s_full_results.rds", panel_lower),
    sprintf("panel_%s_extended_024_results.rds", panel_lower),
    sprintf("panel_%s_extended_0245_results.rds", panel_lower),
    sprintf("panel_%s_outside_msetwo_results.rds", panel_lower)
  )
  files <- file.path(result_dir, files)
  files <- files[file.exists(files)]
  results <- dplyr::bind_rows(lapply(files, readRDS))
  key <- paste(results$side, sprintf("%.8f", results$distance))
  results[!duplicated(key), , drop = FALSE]
}

candidate_distances <- function(bandwidth, grid_step, side) {
  first <- (floor(bandwidth / grid_step) + 1L) * grid_step
  support_max <- if (side < 0) 0.40 else 0.249
  candidates <- seq(first, support_max, by = grid_step)
  utils::head(candidates, max(target_counts) + 10L)
}

fit_at_cutoff <- function(data, cutoff) {
  fit_data <- if (cutoff < true_cutoff) {
    data[data$running_scr < true_cutoff, , drop = FALSE]
  } else {
    data[data$running_scr > true_cutoff, , drop = FALSE]
  }
  args <- make_rd_args(fit_data, cutoff)
  args$bwrestrict <- TRUE
  args$masspoints <- "adjust"
  fit <- tryCatch(
    suppressWarnings(do.call(
      rdrobust::rdrobust,
      c(args, list(bwselect = "msetwo"))
    )),
    error = function(error) error
  )
  if (inherits(fit, "error")) {
    return(data.frame(
      cutoff = cutoff,
      distance = round(abs(cutoff - true_cutoff), 8L),
      side = sign(cutoff - true_cutoff),
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
    distance = round(abs(cutoff - true_cutoff), 8L),
    side = sign(cutoff - true_cutoff),
    conv = as.numeric(fit$Estimate[[1L]]),
    conv_ci_low = as.numeric(fit$ci[1L, 1L]),
    conv_ci_high = as.numeric(fit$ci[1L, 2L]),
    bc = as.numeric(fit$Estimate[[2L]]),
    n_left = as.integer(fit$N_h[[1L]]),
    n_right = as.integer(fit$N_h[[2L]]),
    error = NA_character_
  )
}

required_for_panel <- function(panel) {
  required <- list()
  index <- 0L
  for (grid_step in grid_steps) {
    for (side in c(-1, 1)) {
      side_name <- if (side < 0) "left" else "right"
      distances <- candidate_distances(
        bandwidths[[panel]][[side_name]],
        grid_step,
        side
      )
      index <- index + 1L
      required[[index]] <- data.frame(
        side = side,
        distance = round(distances, 8L)
      )
    }
  }
  required <- do.call(rbind, required)
  key <- paste(required$side, sprintf("%.8f", required$distance))
  required[!duplicated(key), , drop = FALSE]
}

select_side <- function(results, side, bandwidth, grid_step) {
  first <- (floor(bandwidth / grid_step) + 1L) * grid_step
  on_grid <- results$side == side &
    results$distance >= first - 1e-10 &
    abs(results$distance / grid_step - round(results$distance / grid_step)) < 1e-8 &
    is.na(results$error) &
    is.finite(results$conv) &
    is.finite(results$bc)
  selected <- results[on_grid, , drop = FALSE]
  selected <- selected[order(selected$distance), , drop = FALSE]
  selected
}

base_sample <- build_placebo_base_sample(refresh_data = FALSE)
summary_rows <- list()
summary_index <- 0L

for (panel in c("A", "B")) {
  results <- load_existing(panel)
  required <- required_for_panel(panel)
  existing_keys <- paste(results$side, sprintf("%.8f", results$distance))
  required_keys <- paste(required$side, sprintf("%.8f", required$distance))
  missing <- required[!required_keys %in% existing_keys, , drop = FALSE]

  if (nrow(missing) > 0L) {
    analysis_sample <- prepare_placebo_sample(base_sample, panel, review_min)
    message("[Panel ", panel, "] estimating ", nrow(missing), " missing fits")
    extra <- dplyr::bind_rows(lapply(
      seq_len(nrow(missing)),
      function(index) {
        cutoff <- true_cutoff + missing$side[[index]] * missing$distance[[index]]
        fit_at_cutoff(analysis_sample, cutoff)
      }
    ))
    results <- dplyr::bind_rows(results, extra)
  }
  saveRDS(
    results,
    file.path(
      result_dir,
      sprintf("panel_%s_outside_msetwo_results.rds", tolower(panel))
    )
  )

  true_conv <- results$conv[results$distance == 0]
  true_bc <- results$bc[results$distance == 0]
  for (grid_step in grid_steps) {
    left <- select_side(
      results,
      -1,
      bandwidths[[panel]][["left"]],
      grid_step
    )
    right <- select_side(
      results,
      1,
      bandwidths[[panel]][["right"]],
      grid_step
    )
    for (target_each_side in target_counts) {
      feasible <- nrow(left) >= target_each_side && nrow(right) >= target_each_side
      used <- min(nrow(left), nrow(right), target_each_side)
      selected_left <- left[seq_len(used), , drop = FALSE]
      selected_right <- right[seq_len(used), , drop = FALSE]
      conv_values <- c(selected_left$conv, selected_right$conv)
      bc_values <- c(selected_left$bc, selected_right$bc)
      denominator <- 1L + 2L * used

      summary_index <- summary_index + 1L
      summary_rows[[summary_index]] <- data.frame(
        panel = panel,
        grid_step = grid_step,
        requested_each_side = target_each_side,
        h_left = bandwidths[[panel]][["left"]],
        h_right = bandwidths[[panel]][["right"]],
        feasible = feasible,
        used_each_side = used,
        available_left = nrow(left),
        available_right = nrow(right),
        left_start = min(selected_left$distance),
        left_end = max(selected_left$distance),
        right_start = min(selected_right$distance),
        right_end = max(selected_right$distance),
        conv_rank = 1L + sum(conv_values < true_conv),
        bc_rank = 1L + sum(bc_values < true_bc),
        denominator = denominator,
        conv_percentile = 100 * (1L + sum(conv_values < true_conv)) / denominator,
        bc_percentile = 100 * (1L + sum(bc_values < true_bc)) / denominator
      )
    }
  }
}

summary_table <- do.call(rbind, summary_rows)
write.csv(
  summary_table,
  file.path(result_dir, "outside_msetwo_bandwidth_20_25_30_each.csv"),
  row.names = FALSE
)
print(summary_table, row.names = FALSE)
