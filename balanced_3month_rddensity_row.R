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

CUTOFF <- 4.75
TARGET_QUARTERS <- c("Q323", "Q423", "Q124", "Q224", "Q324", "Q424")
PANEL_RESULT_FILE <-
  "results/balanced_3month_rddensity/balanced_3month_rddensity_results.rds"
OUTPUT_DIR <- "results/balanced_3month_rddensity_row"

if (!file.exists(PANEL_RESULT_FILE)) {
  stop(
    "Balanced cohort result not found: ", PANEL_RESULT_FILE,
    "\nRun balanced_3month_rddensity.R first."
  )
}

load("RData/Entire.RData")
panel_result <- readRDS(PANEL_RESULT_FILE)
balanced_ids <- unique(panel_result$balanced_base$id)

monthly_rows <- Entire %>%
  mutate(
    Date = as.Date(Date),
    .year = as.integer(format(Date, "%Y")),
    .month = as.integer(format(Date, "%m")),
    .quarter_number = ((.month - 1L) %/% 3L) + 1L,
    quarter = sprintf("Q%d%02d", .quarter_number, .year %% 100L),
    .year_month = format(Date, "%Y-%m")
  ) %>%
  filter(
    id %in% balanced_ids,
    quarter %in% TARGET_QUARTERS
  ) %>%
  arrange(id, Date)

# The test below deliberately uses the source rows themselves. No quarterly
# aggregation, quarterly carrier-row selection, or quarter fixed effect is
# applied.
density_rows <- monthly_rows %>%
  filter(is.finite(running_scr))

if (nrow(density_rows) == 0L) {
  stop("No finite running_scr observations in the monthly row sample.")
}

dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

row_audit <- data.frame(
  balanced_listings = length(balanced_ids),
  source_rows = nrow(monthly_rows),
  distinct_id_months = n_distinct(
    paste(monthly_rows$id, monthly_rows$.year_month, sep = "::")
  ),
  duplicated_id_month_rows =
    nrow(monthly_rows) -
    n_distinct(paste(monthly_rows$id, monthly_rows$.year_month, sep = "::")),
  finite_running_rows = nrow(density_rows),
  unique_running_values = n_distinct(density_rows$running_scr),
  cutoff = CUTOFF
)
write.csv(
  row_audit,
  file.path(OUTPUT_DIR, "row_sample_audit.csv"),
  row.names = FALSE
)

cat(sprintf(
  paste0(
    "[row rddensity] listings=%d source_rows=%d finite_x=%d ",
    "unique_x=%d cutoff=%.2f\n"
  ),
  row_audit$balanced_listings,
  row_audit$source_rows,
  row_audit$finite_running_rows,
  row_audit$unique_running_values,
  CUTOFF
))

fit <- rddensity::rddensity(
  X = density_rows$running_scr,
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

capture.output(
  summary(fit),
  file = file.path(OUTPUT_DIR, "monthly_row_summary.txt")
)

component_names <- intersect(
  c("hat", "sd_asy", "sd_jk", "test", "h", "N", "bino"),
  names(fit)
)
components <- bind_rows(lapply(
  component_names,
  function(component) {
    value <- fit[[component]]
    if (is.data.frame(value)) value <- as.matrix(value)

    if (is.matrix(value)) {
      rows <- rownames(value)
      columns <- colnames(value)
      if (is.null(rows)) rows <- as.character(seq_len(nrow(value)))
      if (is.null(columns)) columns <- as.character(seq_len(ncol(value)))
      index <- expand.grid(
        row = rows,
        column = columns,
        stringsAsFactors = FALSE
      )
      index$value <- as.numeric(value)
      index$component <- component
      return(index[, c("component", "row", "column", "value")])
    }

    if (is.list(value)) {
      value <- unlist(value, recursive = TRUE, use.names = TRUE)
    }
    value_names <- names(value)
    if (is.null(value_names)) {
      value_names <- as.character(seq_along(value))
    }
    data.frame(
      component = component,
      row = value_names,
      column = NA_character_,
      value = as.numeric(value),
      stringsAsFactors = FALSE
    )
  }
))
write.csv(
  components,
  file.path(OUTPUT_DIR, "monthly_row_components.csv"),
  row.names = FALSE
)

saveRDS(
  list(
    cutoff = CUTOFF,
    row_audit = row_audit,
    monthly_rows = monthly_rows,
    density_rows = density_rows,
    fit = fit
  ),
  file.path(OUTPUT_DIR, "monthly_row_rddensity_results.rds")
)

cat("[row rddensity] completed\n")
