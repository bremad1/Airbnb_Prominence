options(stringsAsFactors = FALSE)

.libPaths(c(file.path(getwd(), ".Rlib"), .libPaths()))

required_packages <- c("dplyr", "rddensity")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1L), quietly = TRUE)
]
if (length(missing_packages) > 0L) {
  stop("Missing packages: ", paste(missing_packages, collapse = ", "))
}

suppressPackageStartupMessages({
  library(dplyr)
  library(rddensity)
})

# Reuse the requested balanced six-quarter, three-month panel definition.
# source() does not run balanced-3month.R's command-line analysis block.
source("Best script/balanced-3month.R")

CUTOFF <- 4.75
OUTPUT_DIR <- "results/balanced_3month_rddensity"

flatten_component <- function(x, component, sample_name) {
  if (is.null(x)) return(data.frame())

  if (is.data.frame(x)) x <- as.matrix(x)
  if (is.matrix(x)) {
    row_names <- rownames(x)
    col_names <- colnames(x)
    if (is.null(row_names)) row_names <- as.character(seq_len(nrow(x)))
    if (is.null(col_names)) col_names <- as.character(seq_len(ncol(x)))

    index <- expand.grid(
      row = row_names,
      column = col_names,
      stringsAsFactors = FALSE
    )
    index$value <- as.numeric(x)
    index$sample <- sample_name
    index$component <- component
    return(index[, c(
      "sample", "component", "row", "column", "value"
    )])
  }

  if (is.list(x)) x <- unlist(x, recursive = TRUE, use.names = TRUE)
  values <- as.numeric(x)
  value_names <- names(x)
  if (is.null(value_names)) {
    value_names <- as.character(seq_along(values))
  }

  data.frame(
    sample = sample_name,
    component = component,
    row = value_names,
    column = NA_character_,
    value = values,
    stringsAsFactors = FALSE
  )
}

run_density <- function(data, sample_name) {
  density_data <- data %>%
    filter(is.finite(running_scr))

  x <- density_data$running_scr
  if (length(x) == 0L) {
    stop("No finite running_scr observations for sample: ", sample_name)
  }

  cat(sprintf(
    "[rddensity] sample=%s n=%d unique_x=%d cutoff=%.2f\n",
    sample_name,
    length(x),
    n_distinct(x),
    CUTOFF
  ))

  fit <- rddensity::rddensity(
    X = x,
    c = CUTOFF,
    p = 2,
    fitselect = "unrestricted",
    kernel = "triangular",
    vce = "jackknife",
    massPoints = TRUE,
    bwselect = "comb",
    all = TRUE,
    bino = TRUE
  )

  summary_path <- file.path(
    OUTPUT_DIR,
    paste0(sample_name, "_summary.txt")
  )
  capture.output(summary(fit), file = summary_path)

  component_names <- intersect(
    c("hat", "sd_asy", "sd_jk", "test", "h", "N", "bino"),
    names(fit)
  )
  components <- bind_rows(lapply(
    component_names,
    function(component) {
      flatten_component(fit[[component]], component, sample_name)
    }
  ))

  list(
    fit = fit,
    sample = density_data,
    components = components,
    summary_path = summary_path
  )
}

run_balanced_3month_rddensity <- function() {
  dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

  balanced_base <- build_quarter_panel(Entire) %>%
    mutate(quarter = factor(quarter, levels = target_quarters))

  # This is the sample used before trimming and review-threshold sweeps in
  # balanced-3month.R.
  active_base <- balanced_base %>%
    filter(
      !is.na(first_month_ltm),
      first_month_ltm >= 1,
      is.finite(price_diff)
    )

  samples <- list(
    balanced_base = balanced_base,
    active_base = active_base
  )

  sample_audit <- bind_rows(lapply(
    names(samples),
    function(sample_name) {
      samples[[sample_name]] %>%
        group_by(quarter) %>%
        summarise(
          rows = n(),
          listings = n_distinct(id),
          finite_running_scr = sum(is.finite(running_scr)),
          unique_running_scr = n_distinct(
            running_scr[is.finite(running_scr)]
          ),
          .groups = "drop"
        ) %>%
        mutate(sample = sample_name, .before = 1L)
    }
  ))

  write.csv(
    sample_audit,
    file.path(OUTPUT_DIR, "sample_audit.csv"),
    row.names = FALSE
  )

  density_results <- lapply(
    names(samples),
    function(sample_name) run_density(samples[[sample_name]], sample_name)
  )
  names(density_results) <- names(samples)

  components <- bind_rows(lapply(
    density_results,
    function(result) result$components
  ))
  write.csv(
    components,
    file.path(OUTPUT_DIR, "rddensity_components.csv"),
    row.names = FALSE
  )

  saveRDS(
    list(
      cutoff = CUTOFF,
      target_quarters = target_quarters,
      sample_audit = sample_audit,
      balanced_base = balanced_base,
      active_base = active_base,
      density = lapply(density_results, function(result) result$fit)
    ),
    file.path(OUTPUT_DIR, "balanced_3month_rddensity_results.rds")
  )

  cat("[rddensity] completed\n")
  invisible(list(
    sample_audit = sample_audit,
    density_results = density_results
  ))
}

if (sys.nframe() == 0L) {
  run_balanced_3month_rddensity()
}
