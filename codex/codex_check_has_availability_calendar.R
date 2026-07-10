base_dir <- "C:/Users/admin/Documents/Codex/2026-07-09/s"
project_root <- "C:/Users/admin/Documents/Codex/2026-07-07/rlt/work/Airbnb_Prominence"
deps_lib <- "C:/Users/admin/Documents/Codex/2026-07-07/rlt/work/Rlibs"
out_dir <- file.path(base_dir, "outputs")

.libPaths(c(deps_lib, .libPaths()))
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

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

raw <- Entire %>%
  mutate(
    Date = as.Date(Date),
    calendar_last_scraped = as.Date(calendar_last_scraped),
    calendar_lag_days = as.numeric(Date - calendar_last_scraped),
    price_observed = !is.na(price),
    av365_group = case_when(
      availability_365 == 0 ~ "av365_0",
      availability_365 > 0 ~ "av365_positive",
      TRUE ~ "av365_missing"
    )
  )

raw_combo <- raw %>%
  group_by(av365_group, price_observed, source, has_availability) %>%
  summarise(
    n = n(),
    mean_calendar_lag_days = mean(calendar_lag_days, na.rm = TRUE),
    median_calendar_lag_days = median(calendar_lag_days, na.rm = TRUE),
    same_day_calendar = sum(calendar_lag_days == 0, na.rm = TRUE),
    same_day_share = mean(calendar_lag_days == 0, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(av365_group, desc(n))

raw_av3650_price_observed_calendar <- raw %>%
  filter(availability_365 == 0, price_observed) %>%
  group_by(source, has_availability, calendar_lag_days) %>%
  summarise(n = n(), .groups = "drop") %>%
  arrange(source, has_availability, calendar_lag_days)

target_months <- lapply(seq_len(nrow(quarter_map)), function(i) {
  qm <- quarter_map[i, ]
  raw %>%
    mutate(
      year = as.integer(format(Date, "%Y")),
      month = as.integer(format(Date, "%m"))
    ) %>%
    filter(year == qm$target_year, month %in% c(qm$month_a, qm$month_b)) %>%
    group_by(id) %>%
    summarise(
      quarter = qm$quarter,
      availability_365_sum_q = sum(availability_365, na.rm = TRUE),
      has_availability_values_q = paste(sort(unique(has_availability)), collapse = "|"),
      has_availability_t_months_q = sum(has_availability == "t", na.rm = TRUE),
      has_availability_f_months_q = sum(has_availability == "f", na.rm = TRUE),
      calendar_same_day_months_q = sum(calendar_lag_days == 0, na.rm = TRUE),
      calendar_lag_max_q = max(calendar_lag_days, na.rm = TRUE),
      previous_scrape_months_q = sum(source == "previous scrape", na.rm = TRUE),
      city_scrape_months_q = sum(source == "city scrape", na.rm = TRUE),
      price_observed_months_q = sum(!is.na(price)),
      .groups = "drop"
    )
}) %>%
  bind_rows()

z <- bind_rows(lapply(names(quarter_list), function(qname) {
  quarter_list[[qname]] %>% mutate(quarter = qname)
})) %>%
  left_join(target_months, by = c("id", "quarter")) %>%
  mutate(
    Date = as.Date(Date),
    calendar_last_scraped = as.Date(calendar_last_scraped),
    calendar_lag_days = as.numeric(Date - calendar_last_scraped),
    full_condition = ex_q1 == 1 | ex_q2 == 1 | ex_q3 == 1 | ex_q4 == 1,
    full_condition = ifelse(is.na(full_condition), FALSE, full_condition),
    panel = case_when(
      ex_super == "t" & ex_super2 == "t" ~ "panel_a_ex2",
      ex_super == "f" & ex_super2 == "t" ~ "panel_b_ex2",
      TRUE ~ "other"
    ),
    av365_sum_group = case_when(
      availability_365_sum_q == 0 ~ "av365_sum0",
      availability_365_sum_q > 0 ~ "av365_sum_positive",
      TRUE ~ "av365_sum_missing"
    )
  )

quarter_combo <- z %>%
  filter(full_condition) %>%
  group_by(av365_sum_group, panel, source, has_availability, has_availability_values_q) %>%
  summarise(
    n = n(),
    avg_price_obs = sum(!is.na(avg_price)),
    avg_price_positive = sum(!is.na(avg_price) & avg_price > 0),
    calendar_same_day_row = sum(calendar_lag_days == 0, na.rm = TRUE),
    calendar_same_day_row_share = mean(calendar_lag_days == 0, na.rm = TRUE),
    calendar_same_day_both_target_months = sum(calendar_same_day_months_q == 2, na.rm = TRUE),
    calendar_same_day_both_target_months_share = mean(calendar_same_day_months_q == 2, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(av365_sum_group, panel, desc(n))

panel_b_filter_counts <- z %>%
  filter(full_condition, panel == "panel_b_ex2") %>%
  summarise(
    n = n(),
    n_has_availability_t = sum(has_availability == "t", na.rm = TRUE),
    n_calendar_same_day = sum(calendar_lag_days == 0, na.rm = TRUE),
    n_has_t_and_calendar_same_day = sum(has_availability == "t" & calendar_lag_days == 0, na.rm = TRUE),
    n_av365_sum0 = sum(availability_365_sum_q == 0, na.rm = TRUE),
    n_av365_sum0_has_t = sum(availability_365_sum_q == 0 & has_availability == "t", na.rm = TRUE),
    n_av365_sum0_calendar_same_day = sum(availability_365_sum_q == 0 & calendar_lag_days == 0, na.rm = TRUE),
    n_av365_sum0_has_t_calendar_same_day = sum(availability_365_sum_q == 0 & has_availability == "t" & calendar_lag_days == 0, na.rm = TRUE),
    .groups = "drop"
  )

write.csv(raw_combo, file.path(out_dir, "has_availability_calendar_raw_combo.csv"), row.names = FALSE)
write.csv(raw_av3650_price_observed_calendar, file.path(out_dir, "has_availability_calendar_raw_av3650_price_obs.csv"), row.names = FALSE)
write.csv(quarter_combo, file.path(out_dir, "has_availability_calendar_quarter_combo.csv"), row.names = FALSE)
write.csv(panel_b_filter_counts, file.path(out_dir, "has_availability_calendar_panel_b_filter_counts.csv"), row.names = FALSE)

cat("Raw combo:\n")
print(raw_combo, n = 50, width = Inf)
cat("\nRaw av365=0 & price observed by calendar lag:\n")
print(raw_av3650_price_observed_calendar, n = 50, width = Inf)
cat("\nQuarter combo:\n")
print(quarter_combo, n = 80, width = Inf)
cat("\nPanel B filter counts:\n")
print(panel_b_filter_counts, width = Inf)
