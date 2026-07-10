base_dir <- "C:/Users/admin/Documents/Codex/2026-07-09/s"
project_root <- "C:/Users/admin/Documents/Codex/2026-07-07/rlt/work/Airbnb_Prominence"
deps_lib <- "C:/Users/admin/Documents/Codex/2026-07-07/rlt/work/Rlibs"
.libPaths(c(deps_lib, .libPaths()))

suppressPackageStartupMessages(library(dplyr))

load(file.path(project_root, "Quarterly_dataset1.RData"))
load(file.path(project_root, "RData", "Entire.RData"))

quarter_map <- data.frame(
  quarter = c("Q323", "Q423", "Q124", "Q224", "Q324", "Q424"),
  target_year = c(2023, 2023, 2024, 2024, 2024, 2024),
  month_a = c(8, 11, 2, 5, 8, 11),
  month_b = c(9, 12, 3, 6, 9, 12),
  stringsAsFactors = FALSE
)

quarter_list <- list(Q323 = Q323, Q423 = Q423, Q124 = Q124, Q224 = Q224, Q324 = Q324, Q424 = Q424)

entire_dates <- Entire %>%
  mutate(
    .date = as.Date(Date),
    .year = as.integer(format(.date, "%Y")),
    .month = as.integer(format(.date, "%m"))
  )

availability_sums <- lapply(seq_len(nrow(quarter_map)), function(i) {
  qm <- quarter_map[i, ]
  entire_dates %>%
    filter(.year == qm$target_year, .month %in% c(qm$month_a, qm$month_b)) %>%
    group_by(id) %>%
    summarise(
      quarter = qm$quarter,
      availability_365_sum_q = sum(availability_365, na.rm = TRUE),
      price_obs_months_q = sum(!is.na(price)),
      price_pos_months_q = sum(!is.na(price) & price > 0),
      price_missing_months_q = sum(is.na(price)),
      .groups = "drop"
    )
}) %>%
  bind_rows()

z <- bind_rows(lapply(names(quarter_list), function(qname) {
  quarter_list[[qname]] %>% mutate(quarter = qname)
}))
z <- z %>% left_join(availability_sums, by = c("id", "quarter"))

full_condition <- z$ex_q1 == 1 | z$ex_q2 == 1 | z$ex_q3 == 1 | z$ex_q4 == 1
full_condition[is.na(full_condition)] <- FALSE

raw_summary <- data.frame(
  raw_rows = nrow(Entire),
  raw_av365_zero = sum(Entire$availability_365 == 0, na.rm = TRUE),
  raw_av365_zero_price_observed = sum(Entire$availability_365 == 0 & !is.na(Entire$price), na.rm = TRUE),
  raw_av365_zero_price_positive = sum(Entire$availability_365 == 0 & !is.na(Entire$price) & Entire$price > 0, na.rm = TRUE),
  raw_av365_zero_price_missing = sum(Entire$availability_365 == 0 & is.na(Entire$price), na.rm = TRUE)
)

raw_by_date <- Entire %>%
  filter(availability_365 == 0) %>%
  summarise(
    n = n(),
    price_obs = sum(!is.na(price)),
    price_pos = sum(!is.na(price) & price > 0),
    price_missing = sum(is.na(price)),
    .by = Date
  ) %>%
  arrange(Date)

quarterly_row_summary <- data.frame(
  z_rows = nrow(z),
  row_av365_zero = sum(z$availability_365 == 0, na.rm = TRUE),
  row_av365_zero_avg_price_observed = sum(z$availability_365 == 0 & !is.na(z$avg_price), na.rm = TRUE),
  row_av365_zero_avg_price_positive = sum(z$availability_365 == 0 & !is.na(z$avg_price) & z$avg_price > 0, na.rm = TRUE),
  row_av365_zero_avg_price_missing = sum(z$availability_365 == 0 & is.na(z$avg_price), na.rm = TRUE)
)

quarter_sum_summary <- data.frame(
  z_rows = nrow(z),
  sum_av365_zero = sum(z$availability_365_sum_q == 0, na.rm = TRUE),
  sum_av365_zero_avg_price_observed = sum(z$availability_365_sum_q == 0 & !is.na(z$avg_price), na.rm = TRUE),
  sum_av365_zero_avg_price_positive = sum(z$availability_365_sum_q == 0 & !is.na(z$avg_price) & z$avg_price > 0, na.rm = TRUE),
  sum_av365_zero_avg_price_missing = sum(z$availability_365_sum_q == 0 & is.na(z$avg_price), na.rm = TRUE),
  sum_av365_zero_raw_price_months_observed = sum(z$price_obs_months_q[z$availability_365_sum_q == 0], na.rm = TRUE),
  sum_av365_zero_raw_price_months_positive = sum(z$price_pos_months_q[z$availability_365_sum_q == 0], na.rm = TRUE),
  sum_av365_zero_raw_price_months_missing = sum(z$price_missing_months_q[z$availability_365_sum_q == 0], na.rm = TRUE)
)

analysis_sample_summary <- z %>%
  mutate(
    panel = case_when(
      ex_super == "t" & ex_super2 == "t" ~ "panel_a_ex2",
      ex_super == "f" & ex_super2 == "t" ~ "panel_b_ex2",
      TRUE ~ "other"
    ),
    verified = host_identity_verified == "t"
  ) %>%
  filter(full_condition, panel %in% c("panel_a_ex2", "panel_b_ex2")) %>%
  summarise(
    n = n(),
    row_av365_zero = sum(availability_365 == 0, na.rm = TRUE),
    row_av365_zero_avg_price_obs = sum(availability_365 == 0 & !is.na(avg_price), na.rm = TRUE),
    row_av365_zero_avg_price_pos = sum(availability_365 == 0 & !is.na(avg_price) & avg_price > 0, na.rm = TRUE),
    sum_av365_zero = sum(availability_365_sum_q == 0, na.rm = TRUE),
    sum_av365_zero_avg_price_obs = sum(availability_365_sum_q == 0 & !is.na(avg_price), na.rm = TRUE),
    sum_av365_zero_avg_price_pos = sum(availability_365_sum_q == 0 & !is.na(avg_price) & avg_price > 0, na.rm = TRUE),
    .by = c(panel, verified)
  ) %>%
  arrange(panel, verified)

cat("Raw summary:\n")
print(raw_summary, row.names = FALSE)
cat("\nRaw av365==0 by Date:\n")
print(raw_by_date, n = 100)
cat("\nQuarterly row-level summary:\n")
print(quarterly_row_summary, row.names = FALSE)
cat("\nQuarterly quarter-sum summary:\n")
print(quarter_sum_summary, row.names = FALSE)
cat("\nAnalysis sample summary:\n")
print(analysis_sample_summary, n = 20)
