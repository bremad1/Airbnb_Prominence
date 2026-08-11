options(stringsAsFactors = FALSE)

.libPaths(c(file.path(getwd(), ".Rlib"), .libPaths()))

required_packages <- c("dplyr", "rdrobust", "rddensity", "ggplot2")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0L) {
  stop(
    "Missing packages: ", paste(missing_packages, collapse = ", "),
    "\nInstall them with: install.packages(c(",
    paste(sprintf('"%s"', missing_packages), collapse = ", "),
    "))"
  )
}

suppressPackageStartupMessages({
  library(dplyr)
  library(rdrobust)
  library(rddensity)
  library(ggplot2)
})

WINDOW_NAME <- "3-month"
DEFAULT_OUTPUT_DIR <- "results/rd_diagnostics_3month"

# The panel builder below is copied from the corresponding analysis script.
# Diagnostics are run immediately after build_quarter_panel(), before active,
# cumulative-review, trimming, finite-price-change, or ex_super restrictions.


first_value_at <- function(x, index, target_index = 1L) {
  value <- x[index == target_index & !is.na(x)]
  if (length(value) == 0L) NA_real_ else as.numeric(value[[1L]])
}

build_quarter_panel <- function(monthly_data) {
  cat(sprintf(
    "[build_quarter_panel] input monthly rows: %d\n",
    nrow(monthly_data)
  ))

  # NOTE (fix #4): the old whole-history "n_distinct(host_id) == 1L across
  # every month a listing was ever observed" filter is gone. Owner identity
  # only needs to hold (a) within a single quarter, and (b) between the two
  # specific quarters being differenced for price_diff -- not for the
  # listing's entire lifetime. Both are enforced further down.
  monthly <- monthly_data %>%
    mutate(
      Date = as.Date(Date),
      .year = as.integer(format(Date, "%Y")),
      .month = as.integer(format(Date, "%m")),
      .quarter_number = ((.month - 1L) %/% 3L) + 1L,
      .quarter_month = ((.month - 1L) %% 3L) + 1L,
      .quarter_index = 4L * .year + .quarter_number,
      quarter = sprintf("Q%d%02d", .quarter_number, .year %% 100L),
      .year_month = format(Date, "%Y-%m")
    ) %>%
    arrange(id, Date) %>%
    # If the source ever contains more than one scrape for an id-month, keep
    # the earliest scrape so a month cannot receive extra weight.
    group_by(id, .year_month) %>%
    slice(1L) %>%
    ungroup()

  cat(sprintf(
    "[build_quarter_panel] monthly rows after dedup: %d\n",
    nrow(monthly)
  ))

  quarterly <- monthly %>%
    group_by(id, .quarter_index, quarter) %>%
    # "Balanced" means that all three calendar months of the quarter exist.
    # Within each listing-quarter, host_id and host_is_superhost must both be
    # observed and constant. Superhost status may still change between two
    # adjacent quarters (e.g. f,f,f in Q4 followed by t,t,t in Q1).
    filter(
      n_distinct(.quarter_month) == 3L,
      all(1:3 %in% .quarter_month),
      all(is.finite(price)),
      all(price > 0),
      all(!is.na(host_id)),
      n_distinct(host_id) == 1L,
      all(!is.na(host_is_superhost)),
      all(host_is_superhost != ""),
      n_distinct(host_is_superhost) == 1L
    ) %>%
    arrange(.quarter_month, Date, .by_group = TRUE) %>%
    mutate(
      avg_price = mean(price, na.rm = TRUE),
      first_month_ltm = first_value_at(
        number_of_reviews_ltm,
        .quarter_month
      ),
      first_month_number_of_reviews = first_value_at(
        number_of_reviews,
        .quarter_month
      ),
      quarter_months_observed = n_distinct(.quarter_month)
    ) %>%
    # Retain the first-month row as the carrier for the quarter-level
    # treatment, running-variable, and host/listing characteristics.
    slice(1L) %>%
    ungroup()

  cat(sprintf(
    "[build_quarter_panel] rows after balanced-quarter (3-month) + within-quarter host_id filter: %d\n",
    nrow(quarterly)
  ))

  quarterly <- quarterly %>%
    group_by(host_id, .quarter_index) %>%
    filter(
      all(!is.na(host_is_superhost)),
      all(host_is_superhost != ""),
      n_distinct(host_is_superhost) == 1L
    ) %>%
    ungroup() %>%
    arrange(id, .quarter_index) %>%
    group_by(id) %>%
    mutate(
      previous_quarter_index = lag(.quarter_index),
      previous_quarter = lag(quarter),
      previous_host_id = lag(host_id),
      ex_avg = lag(avg_price),
      consecutive_previous_quarter =
        .quarter_index - previous_quarter_index == 1L,
      # fix #4: price_diff/raw_change now also require the SAME host_id in
      # the current quarter and its immediately preceding quarter -- i.e.
      # ownership only has to match across the one 6-month pair actually
      # being compared, not across the listing's whole history.
      same_host_as_previous_quarter =
        !is.na(previous_host_id) & host_id == previous_host_id,
      price_diff = if_else(
        consecutive_previous_quarter & same_host_as_previous_quarter,
        log(avg_price) - log(ex_avg),
        NA_real_
      ),
      raw_change = if_else(
        consecutive_previous_quarter & same_host_as_previous_quarter,
        (avg_price - ex_avg) / ex_avg,
        NA_real_
      )
    ) %>%
    ungroup() %>%
    filter(
      quarter %in% c("Q323", "Q423", "Q124", "Q224", "Q324", "Q424")
    )

  cat(sprintf(
    "[build_quarter_panel] rows after host-consistency + quarter-window filter: %d\n",
    nrow(quarterly)
  ))

  stopifnot(
    !anyDuplicated(quarterly[c("id", "quarter")]),
    all(quarterly$quarter_months_observed == 3L)
  )

  cat("[build_quarter_panel] rows by quarter:\n")
  print(table(quarterly$quarter))

  quarterly
}

# -----------------------------------------------------------------------------
# RD diagnostics settings
# -----------------------------------------------------------------------------
TRUE_CUTOFF <- 4.75
PLACEBO_CUTOFFS <- seq(4.50, 4.90, by = 0.01)
PLACEBO_CLUSTERS <- c("none", "id", "host_id", "quarter")
QUARTER_LEVELS <- c("Q323", "Q423", "Q124", "Q224", "Q324", "Q424")

normalize_binary <- function(x) {
  if (is.logical(x)) return(as.numeric(x))
  if (is.numeric(x) || is.integer(x)) return(as.numeric(x))

  value <- tolower(trimws(as.character(x)))
  out <- rep(NA_real_, length(value))
  out[value %in% c("1", "t", "true", "yes", "y")] <- 1
  out[value %in% c("0", "f", "false", "no", "n")] <- 0
  out
}

cluster_label <- function(x) {
  switch(
    x,
    none = "No clustering",
    id = "Listing ID",
    host_id = "Host ID",
    quarter = "Quarter (time)",
    x
  )
}

cluster_vector <- function(data, cluster_type) {
  switch(
    cluster_type,
    none = NULL,
    id = data$id,
    host_id = data$host_id,
    quarter = as.integer(factor(data$quarter)),
    stop("Unknown cluster type: ", cluster_type)
  )
}

flatten_numeric_component <- function(object, component, window_name) {
  x <- object[[component]]
  if (is.null(x)) return(data.frame())

  if (is.data.frame(x)) x <- as.matrix(x)
  if (is.matrix(x)) {
    rn <- rownames(x)
    cn <- colnames(x)
    if (is.null(rn)) rn <- as.character(seq_len(nrow(x)))
    if (is.null(cn)) cn <- as.character(seq_len(ncol(x)))
    grid <- expand.grid(
      row = rn,
      column = cn,
      stringsAsFactors = FALSE
    )
    grid$value <- as.numeric(x)
    grid$window <- window_name
    grid$component <- component
    return(grid[, c("window", "component", "row", "column", "value")])
  }

  if (is.list(x)) x <- unlist(x, recursive = TRUE, use.names = TRUE)
  values <- as.numeric(x)
  nm <- names(x)
  if (is.null(nm)) nm <- as.character(seq_along(values))
  data.frame(
    window = window_name,
    component = component,
    row = nm,
    column = NA_character_,
    value = values,
    stringsAsFactors = FALSE
  )
}

run_rddensity_test <- function(panel, output_dir, window_name) {
  density_data <- panel %>%
    filter(is.finite(running_scr))

  x <- density_data$running_scr
  cat(sprintf(
    "[%s] RDDensity: n=%d, unique running values=%d, cutoff=%.2f\n",
    window_name, length(x), n_distinct(x), TRUE_CUTOFF
  ))

  fit <- tryCatch(
    rddensity::rddensity(
      X = x,
      c = TRUE_CUTOFF,
      p = 2,
      fitselect = "unrestricted",
      kernel = "triangular",
      vce = "jackknife",
      massPoints = TRUE,
      bwselect = "comb",
      all = TRUE,
      bino = TRUE
    ),
    error = function(e) e
  )

  if (inherits(fit, "error")) {
    writeLines(
      paste("RDDensity failed:", conditionMessage(fit)),
      file.path(output_dir, "rddensity_error.txt")
    )
    return(list(fit = fit, sample = density_data, components = data.frame()))
  }

  capture.output(
    summary(fit),
    file = file.path(output_dir, "rddensity_cutoff_4_75_summary.txt")
  )

  component_names <- intersect(
    c("hat", "sd_asy", "sd_jk", "test", "h", "N", "bino"),
    names(fit)
  )
  components <- bind_rows(lapply(
    component_names,
    function(nm) flatten_numeric_component(fit, nm, window_name)
  ))
  write.csv(
    components,
    file.path(output_dir, "rddensity_cutoff_4_75_components.csv"),
    row.names = FALSE
  )

  plot_result <- tryCatch(
    rddensity::rdplotdensity(
      fit,
      X = x,
      plotRange = c(
        max(min(x, na.rm = TRUE), TRUE_CUTOFF - 0.50),
        min(max(x, na.rm = TRUE), TRUE_CUTOFF + 0.50)
      ),
      plotN = c(30, 30),
      plotGrid = "es",
      alpha = 0.05,
      type = "both",
      CItype = "region",
      CIuniform = FALSE,
      hist = TRUE,
      title = sprintf("RDDensity test: %s", window_name),
      xlabel = "Running score",
      ylabel = "Density"
    ),
    error = function(e) e
  )

  if (!inherits(plot_result, "error") && !is.null(plot_result$Estplot)) {
    ggplot2::ggsave(
      file.path(output_dir, "rddensity_cutoff_4_75.png"),
      plot_result$Estplot,
      width = 8,
      height = 5.5,
      dpi = 300
    )
    ggplot2::ggsave(
      file.path(output_dir, "rddensity_cutoff_4_75.pdf"),
      plot_result$Estplot,
      width = 8,
      height = 5.5
    )
  } else if (inherits(plot_result, "error")) {
    writeLines(
      paste("RDDensity plot failed:", conditionMessage(plot_result)),
      file.path(output_dir, "rddensity_plot_error.txt")
    )
  }

  list(fit = fit, sample = density_data, components = components)
}

extract_rdrobust <- function(fit) {
  estimate <- as.numeric(fit$Estimate[[1L]])
  se <- as.numeric(fit$se[[1L]])
  p_value <- as.numeric(fit$pv[[1L]])

  data.frame(
    estimate = estimate,
    se = se,
    p_value = p_value,
    ci_low = estimate - qnorm(0.975) * se,
    ci_high = estimate + qnorm(0.975) * se,
    bandwidth_left = as.numeric(fit$bws[1L, 1L]),
    bandwidth_right = as.numeric(fit$bws[1L, 2L]),
    effective_n = as.integer(sum(fit$N_h))
  )
}

run_first_stage_placebos <- function(panel, output_dir, window_name) {
  data <- panel %>%
    mutate(
      .treatment = normalize_binary(host_is_superhost2),
      quarter = factor(quarter, levels = QUARTER_LEVELS)
    ) %>%
    filter(
      is.finite(running_scr),
      is.finite(.treatment)
    )

  results <- list()
  result_index <- 0L

  for (cluster_type in PLACEBO_CLUSTERS) {
    cluster_data <- data
    if (cluster_type == "id") {
      cluster_data <- cluster_data %>% filter(!is.na(id))
    } else if (cluster_type == "host_id") {
      cluster_data <- cluster_data %>% filter(!is.na(host_id))
    } else if (cluster_type == "quarter") {
      cluster_data <- cluster_data %>% filter(!is.na(quarter))
    }

    for (cutoff in PLACEBO_CUTOFFS) {
      cat(sprintf(
        "[%s] first stage: cluster=%s cutoff=%.2f n=%d\n",
        window_name, cluster_type, cutoff, nrow(cluster_data)
      ))

      rd_args <- list(
        y = cluster_data$.treatment,
        x = cluster_data$running_scr,
        c = cutoff,
        kernel = "tri",
        bwselect = "msetwo",
        p = 1,
        masspoints = "adjust",
        bwrestrict = TRUE
      )

      time_dummies <- as.data.frame(
        model.matrix(~ quarter - 1, data = cluster_data)
      )
      if (ncol(time_dummies) > 1L) {
        rd_args$covs <- as.matrix(time_dummies[, -1L, drop = FALSE])
      }

      cl <- cluster_vector(cluster_data, cluster_type)
      if (!is.null(cl)) rd_args$cluster <- cl
      if ("all" %in% names(formals(rdrobust::rdrobust))) rd_args$all <- TRUE

      fit <- tryCatch(
        do.call(rdrobust::rdrobust, rd_args),
        error = function(e) e
      )

      result_index <- result_index + 1L
      if (inherits(fit, "error")) {
        results[[result_index]] <- data.frame(
          window = window_name,
          cluster_type = cluster_type,
          cluster_label = cluster_label(cluster_type),
          cutoff = cutoff,
          is_true_cutoff = abs(cutoff - TRUE_CUTOFF) < 1e-10,
          raw_n = nrow(cluster_data),
          estimate = NA_real_,
          se = NA_real_,
          p_value = NA_real_,
          ci_low = NA_real_,
          ci_high = NA_real_,
          bandwidth_left = NA_real_,
          bandwidth_right = NA_real_,
          effective_n = NA_integer_,
          error = conditionMessage(fit)
        )
      } else {
        extracted <- extract_rdrobust(fit)
        results[[result_index]] <- cbind(
          data.frame(
            window = window_name,
            cluster_type = cluster_type,
            cluster_label = cluster_label(cluster_type),
            cutoff = cutoff,
            is_true_cutoff = abs(cutoff - TRUE_CUTOFF) < 1e-10,
            raw_n = nrow(cluster_data),
            stringsAsFactors = FALSE
          ),
          extracted,
          error = NA_character_
        )
      }
    }
  }

  results <- bind_rows(results)
  write.csv(
    results,
    file.path(output_dir, "first_stage_placebo_cutoff_scan.csv"),
    row.names = FALSE
  )

  plot_data <- results %>% filter(is.finite(estimate), is.finite(ci_low), is.finite(ci_high))

  p <- ggplot(
    plot_data,
    aes(x = cutoff, y = estimate, group = cluster_label)
  ) +
    geom_ribbon(aes(ymin = ci_low, ymax = ci_high), alpha = 0.18) +
    geom_line(linewidth = 0.55) +
    geom_point(size = 1.2) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_vline(xintercept = TRUE_CUTOFF, linetype = "dashed") +
    facet_wrap(~ cluster_label, scales = "free_y", ncol = 2) +
    scale_x_continuous(
      breaks = seq(4.50, 4.90, by = 0.05),
      limits = c(4.50, 4.90)
    ) +
    labs(
      title = sprintf("First-stage cutoff placebo scan: %s", window_name),
      subtitle = "Shaded regions are pointwise 95% confidence intervals; vertical line is the true cutoff (4.75)",
      x = "Placebo cutoff",
      y = "Estimated jump in Superhost status"
    ) +
    theme_minimal(base_size = 11)

  ggsave(
    file.path(output_dir, "first_stage_placebo_cutoff_scan.png"),
    p,
    width = 10,
    height = 7,
    dpi = 300
  )
  ggsave(
    file.path(output_dir, "first_stage_placebo_cutoff_scan.pdf"),
    p,
    width = 10,
    height = 7
  )

  true_cutoff_results <- results %>%
    filter(is_true_cutoff) %>%
    select(
      window, cluster_type, cluster_label, cutoff, raw_n,
      estimate, se, p_value, ci_low, ci_high,
      bandwidth_left, bandwidth_right, effective_n, error
    )
  write.csv(
    true_cutoff_results,
    file.path(output_dir, "first_stage_true_cutoff_4_75.csv"),
    row.names = FALSE
  )

  results
}

run_diagnostics <- function(
    output_dir = DEFAULT_OUTPUT_DIR,
    data_path = "RData/Entire.RData"
) {
  if (!file.exists(data_path)) stop("Data file not found: ", data_path)
  load(data_path)
  if (!exists("Entire")) stop("The loaded file does not contain an object named Entire.")

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  cat(sprintf("=== Building %s pre-active quarter panel ===\n", WINDOW_NAME))
  quarterly_panel <- build_quarter_panel(Entire)
  quarterly_panel$quarter <- factor(
    quarterly_panel$quarter,
    levels = QUARTER_LEVELS
  )

  cat(sprintf(
    "[%s] rows before active/review/trim/panel restrictions: %d\n",
    WINDOW_NAME, nrow(quarterly_panel)
  ))

  density_result <- run_rddensity_test(
    quarterly_panel,
    output_dir,
    WINDOW_NAME
  )
  placebo_results <- run_first_stage_placebos(
    quarterly_panel,
    output_dir,
    WINDOW_NAME
  )

  saveRDS(
    list(
      window = WINDOW_NAME,
      true_cutoff = TRUE_CUTOFF,
      placebo_cutoffs = PLACEBO_CUTOFFS,
      placebo_clusters = PLACEBO_CLUSTERS,
      quarterly_panel_pre_active = quarterly_panel,
      rddensity_fit = density_result$fit,
      rddensity_components = density_result$components,
      first_stage_placebo = placebo_results
    ),
    file.path(output_dir, "rd_diagnostics_results.rds")
  )

  metadata <- c(
    paste0("Window: ", WINDOW_NAME),
    paste0("True cutoff: ", TRUE_CUTOFF),
    paste0("Quarter-panel rows before active/review/trim/panel restrictions: ", nrow(quarterly_panel)),
    paste0("RDDensity running-variable observations: ", nrow(density_result$sample)),
    paste0("Placebo cutoffs: ", min(PLACEBO_CUTOFFS), " to ", max(PLACEBO_CUTOFFS), " by 0.01"),
    paste0("Placebo clustering methods: ", paste(PLACEBO_CLUSTERS, collapse = ", ")),
    "Important: build_quarter_panel() restrictions are retained; first_month_ltm, review-threshold, trimming, finite-price-change, and ex_super panel restrictions are not applied.",
    "First-stage placebo regressions include quarter fixed effects, matching the main RD specification."
  )
  writeLines(metadata, file.path(output_dir, "diagnostic_metadata.txt"))

  cat("=== Diagnostics complete ===\n")
  cat("Output directory: ", normalizePath(output_dir, winslash = "/"), "\n", sep = "")

  invisible(list(
    quarterly_panel = quarterly_panel,
    density = density_result,
    placebo = placebo_results
  ))
}

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  output_dir <- if (length(args) >= 1L) args[[1L]] else DEFAULT_OUTPUT_DIR
  data_path <- if (length(args) >= 2L) args[[2L]] else "RData/Entire.RData"
  run_diagnostics(output_dir = output_dir, data_path = data_path)
}
