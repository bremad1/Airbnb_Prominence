base_dir <- "C:/Users/admin/Documents/Codex/2026-07-09/s"
project_root <- "C:/Users/admin/Documents/Codex/2026-07-07/rlt/work/Airbnb_Prominence"
deps_lib <- "C:/Users/admin/Documents/Codex/2026-07-07/rlt/work/Rlibs"
out_dir <- file.path(base_dir, "outputs")

.libPaths(c(deps_lib, .libPaths()))
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages(library(dplyr))

load(file.path(project_root, "Quarterly_dataset1.RData"))
load(file.path(project_root, "RData", "Entire.RData"))

to_date <- function(x) as.Date(x)
is_t <- function(x) !is.na(x) & as.character(x) == "t"
pct <- function(x) mean(x, na.rm = TRUE)
median_na <- function(x) median(x, na.rm = TRUE)
q25 <- function(x) as.numeric(quantile(x, 0.25, na.rm = TRUE, names = FALSE))
q75 <- function(x) as.numeric(quantile(x, 0.75, na.rm = TRUE, names = FALSE))

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
    Date = to_date(Date),
    calendar_last_scraped_date = to_date(calendar_last_scraped),
    year = as.integer(format(Date, "%Y")),
    month = as.integer(format(Date, "%m")),
    year_month = format(Date, "%Y-%m"),
    av365_zero = availability_365 == 0,
    price_observed = !is.na(price),
    price_positive = !is.na(price) & price > 0,
    cal_scrape_lag_days = as.numeric(Date - calendar_last_scraped_date)
  )

raw_date_summary <- entire_dates %>%
  filter(av365_zero) %>%
  group_by(Date) %>%
  summarise(
    n_av365_zero = n(),
    n_price_observed = sum(price_observed),
    n_price_missing = sum(!price_observed),
    price_observed_rate = mean(price_observed),
    mean_price_if_observed = mean(price[price_observed], na.rm = TRUE),
    median_price_if_observed = median_na(price[price_observed]),
    mean_calendar_lag_days = mean(cal_scrape_lag_days, na.rm = TRUE),
    same_calendar_scrape_share = pct(cal_scrape_lag_days == 0),
    .groups = "drop"
  ) %>%
  arrange(Date)

raw_year_summary <- entire_dates %>%
  filter(av365_zero) %>%
  group_by(year) %>%
  summarise(
    n_av365_zero = n(),
    n_price_observed = sum(price_observed),
    n_price_missing = sum(!price_observed),
    price_observed_rate = mean(price_observed),
    same_calendar_scrape_share = pct(cal_scrape_lag_days == 0),
    .groups = "drop"
  ) %>%
  arrange(year)

raw_source_summary <- entire_dates %>%
  filter(av365_zero) %>%
  group_by(source, has_availability) %>%
  summarise(
    n_av365_zero = n(),
    n_price_observed = sum(price_observed),
    n_price_missing = sum(!price_observed),
    price_observed_rate = mean(price_observed),
    .groups = "drop"
  ) %>%
  arrange(desc(n_av365_zero))

raw_group_characteristics <- entire_dates %>%
  mutate(
    group = case_when(
      av365_zero & price_observed ~ "av365_0_price_observed",
      av365_zero & !price_observed ~ "av365_0_price_missing",
      availability_365 > 0 & price_observed ~ "av365_pos_price_observed",
      TRUE ~ "other"
    ),
    days_since_last_review = as.numeric(Date - to_date(last_review)),
    host_age_days = as.numeric(Date - to_date(host_since))
  ) %>%
  filter(group != "other") %>%
  group_by(group) %>%
  summarise(
    n = n(),
    distinct_ids = n_distinct(id),
    share_2023 = pct(year == 2023),
    share_2024 = pct(year == 2024),
    share_source_city_scrape = pct(source == "city scrape"),
    share_has_availability_t = pct(is_t(has_availability)),
    share_host_identity_verified_t = pct(is_t(host_identity_verified)),
    share_host_superhost_t = pct(is_t(host_is_superhost)),
    share_instant_bookable_t = pct(is_t(instant_bookable)),
    mean_price = mean(price, na.rm = TRUE),
    median_price = median_na(price),
    mean_availability_30 = mean(availability_30, na.rm = TRUE),
    mean_availability_90 = mean(availability_90, na.rm = TRUE),
    mean_availability_365 = mean(availability_365, na.rm = TRUE),
    mean_reviews_ltm = mean(number_of_reviews_ltm, na.rm = TRUE),
    median_reviews_ltm = median_na(number_of_reviews_ltm),
    mean_days_since_last_review = mean(days_since_last_review, na.rm = TRUE),
    median_days_since_last_review = median_na(days_since_last_review),
    share_no_last_review = pct(is.na(last_review)),
    mean_minimum_nights = mean(minimum_nights, na.rm = TRUE),
    median_minimum_nights = median_na(minimum_nights),
    mean_review_rating = mean(review_scores_rating, na.rm = TRUE),
    mean_calendar_lag_days = mean(cal_scrape_lag_days, na.rm = TRUE),
    share_calendar_scraped_same_day = pct(cal_scrape_lag_days == 0),
    .groups = "drop"
  )

availability_sums <- lapply(seq_len(nrow(quarter_map)), function(i) {
  qm <- quarter_map[i, ]
  entire_dates %>%
    filter(year == qm$target_year, month %in% c(qm$month_a, qm$month_b)) %>%
    group_by(id) %>%
    summarise(
      quarter = qm$quarter,
      target_month_dates = paste(sort(unique(as.character(Date))), collapse = ";"),
      availability_365_sum_q = sum(availability_365, na.rm = TRUE),
      availability_90_sum_q = sum(availability_90, na.rm = TRUE),
      price_obs_months_q = sum(!is.na(price)),
      price_pos_months_q = sum(!is.na(price) & price > 0),
      price_missing_months_q = sum(is.na(price)),
      min_calendar_lag_days_q = min(cal_scrape_lag_days, na.rm = TRUE),
      max_calendar_lag_days_q = max(cal_scrape_lag_days, na.rm = TRUE),
      same_calendar_scrape_months_q = sum(cal_scrape_lag_days == 0, na.rm = TRUE),
      .groups = "drop"
    )
}) %>%
  bind_rows()

z <- bind_rows(lapply(names(quarter_list), function(qname) {
  quarter_list[[qname]] %>% mutate(quarter = qname)
})) %>%
  left_join(availability_sums, by = c("id", "quarter")) %>%
  mutate(
    Date = to_date(Date),
    calendar_last_scraped_date = to_date(calendar_last_scraped),
    year_month = format(Date, "%Y-%m"),
    full_condition = ex_q1 == 1 | ex_q2 == 1 | ex_q3 == 1 | ex_q4 == 1,
    full_condition = ifelse(is.na(full_condition), FALSE, full_condition),
    panel = case_when(
      ex_super == "t" & ex_super2 == "t" ~ "panel_a_ex2",
      ex_super == "f" & ex_super2 == "t" ~ "panel_b_ex2",
      TRUE ~ "other"
    ),
    verified = host_identity_verified == "t",
    days_since_last_review = as.numeric(Date - to_date(last_review)),
    host_age_days = as.numeric(Date - to_date(host_since)),
    av365_sum0 = availability_365_sum_q == 0,
    row_av3650 = availability_365 == 0
  )

quarter_count_summary <- z %>%
  filter(full_condition) %>%
  group_by(quarter) %>%
  summarise(
    n = n(),
    n_row_av3650 = sum(row_av3650, na.rm = TRUE),
    row_av3650_share = mean(row_av3650, na.rm = TRUE),
    n_av365_sum0 = sum(av365_sum0, na.rm = TRUE),
    av365_sum0_share = mean(av365_sum0, na.rm = TRUE),
    av365_sum0_avg_price_obs = sum(av365_sum0 & !is.na(avg_price), na.rm = TRUE),
    av365_sum0_two_price_months = sum(av365_sum0 & price_obs_months_q == 2, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(quarter)

analysis_sample_counts <- z %>%
  filter(full_condition, panel %in% c("panel_a_ex2", "panel_b_ex2")) %>%
  group_by(panel, verified) %>%
  summarise(
    n = n(),
    n_row_av3650 = sum(row_av3650, na.rm = TRUE),
    n_av365_sum0 = sum(av365_sum0, na.rm = TRUE),
    av365_sum0_share = mean(av365_sum0, na.rm = TRUE),
    av365_sum0_two_price_months = sum(av365_sum0 & price_obs_months_q == 2, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(panel, verified)

quarter_group_characteristics <- z %>%
  filter(full_condition) %>%
  mutate(group = ifelse(av365_sum0, "av365_sum0", "av365_sum_positive")) %>%
  group_by(group) %>%
  summarise(
    n = n(),
    distinct_ids = n_distinct(id),
    share_panel_a = pct(panel == "panel_a_ex2"),
    share_panel_b = pct(panel == "panel_b_ex2"),
    share_verified = pct(verified),
    share_source_previous_scrape = pct(source == "previous scrape"),
    share_source_city_scrape = pct(source == "city scrape"),
    share_host_superhost_current = pct(is_t(host_is_superhost)),
    share_instant_bookable = pct(is_t(instant_bookable)),
    share_has_availability = pct(is_t(has_availability)),
    mean_avg_price = mean(avg_price, na.rm = TRUE),
    median_avg_price = median_na(avg_price),
    p25_avg_price = q25(avg_price),
    p75_avg_price = q75(avg_price),
    mean_ex_avg = mean(ex_avg, na.rm = TRUE),
    median_ex_avg = median_na(ex_avg),
    mean_price_diff = mean(price_diff, na.rm = TRUE),
    median_price_diff = median_na(price_diff),
    mean_availability_30 = mean(availability_30, na.rm = TRUE),
    mean_availability_90 = mean(availability_90, na.rm = TRUE),
    mean_availability_365 = mean(availability_365, na.rm = TRUE),
    mean_availability_365_sum_q = mean(availability_365_sum_q, na.rm = TRUE),
    mean_number_reviews_ltm = mean(number_of_reviews_ltm, na.rm = TRUE),
    median_number_reviews_ltm = median_na(number_of_reviews_ltm),
    share_number_reviews_ltm_zero = pct(number_of_reviews_ltm == 0),
    mean_ex_quarter_ltm = mean(ex_quarter_ltm, na.rm = TRUE),
    median_ex_quarter_ltm = median_na(ex_quarter_ltm),
    share_ex_quarter_ltm_zero = pct(ex_quarter_ltm == 0),
    mean_days_since_last_review = mean(days_since_last_review, na.rm = TRUE),
    median_days_since_last_review = median_na(days_since_last_review),
    share_no_last_review = pct(is.na(last_review)),
    mean_minimum_nights = mean(minimum_nights, na.rm = TRUE),
    median_minimum_nights = median_na(minimum_nights),
    mean_review_rating = mean(review_scores_rating, na.rm = TRUE),
    median_running_scr = median_na(running_scr),
    .groups = "drop"
  )

quarter_group_by_panel <- z %>%
  filter(full_condition, panel %in% c("panel_a_ex2", "panel_b_ex2")) %>%
  mutate(group = ifelse(av365_sum0, "av365_sum0", "av365_sum_positive")) %>%
  group_by(panel, verified, group) %>%
  summarise(
    n = n(),
    mean_avg_price = mean(avg_price, na.rm = TRUE),
    median_avg_price = median_na(avg_price),
    mean_price_diff = mean(price_diff, na.rm = TRUE),
    median_price_diff = median_na(price_diff),
    mean_number_reviews_ltm = mean(number_of_reviews_ltm, na.rm = TRUE),
    median_number_reviews_ltm = median_na(number_of_reviews_ltm),
    share_number_reviews_ltm_zero = pct(number_of_reviews_ltm == 0),
    mean_ex_quarter_ltm = mean(ex_quarter_ltm, na.rm = TRUE),
    median_ex_quarter_ltm = median_na(ex_quarter_ltm),
    share_ex_quarter_ltm_zero = pct(ex_quarter_ltm == 0),
    median_days_since_last_review = median_na(days_since_last_review),
    share_instant_bookable = pct(is_t(instant_bookable)),
    share_has_availability = pct(is_t(has_availability)),
    .groups = "drop"
  ) %>%
  arrange(panel, verified, group)

quarter_source_summary <- z %>%
  filter(full_condition) %>%
  mutate(group = ifelse(av365_sum0, "av365_sum0", "av365_sum_positive")) %>%
  group_by(group, source, has_availability) %>%
  summarise(
    n = n(),
    panel_a = sum(panel == "panel_a_ex2", na.rm = TRUE),
    panel_b = sum(panel == "panel_b_ex2", na.rm = TRUE),
    verified = sum(verified, na.rm = TRUE),
    mean_avg_price = mean(avg_price, na.rm = TRUE),
    median_days_since_last_review = median_na(days_since_last_review),
    .groups = "drop"
  ) %>%
  arrange(group, desc(n))

target_month_pattern <- z %>%
  filter(full_condition, av365_sum0) %>%
  group_by(quarter, target_month_dates) %>%
  summarise(
    n = n(),
    panel_a = sum(panel == "panel_a_ex2", na.rm = TRUE),
    panel_b = sum(panel == "panel_b_ex2", na.rm = TRUE),
    verified = sum(verified, na.rm = TRUE),
    two_price_months = sum(price_obs_months_q == 2, na.rm = TRUE),
    same_calendar_scrape_both_months = sum(same_calendar_scrape_months_q == 2, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(quarter, target_month_dates)

write.csv(raw_date_summary, file.path(out_dir, "av365_zero_price_raw_by_date.csv"), row.names = FALSE)
write.csv(raw_year_summary, file.path(out_dir, "av365_zero_price_raw_by_year.csv"), row.names = FALSE)
write.csv(raw_source_summary, file.path(out_dir, "av365_zero_price_raw_by_source.csv"), row.names = FALSE)
write.csv(raw_group_characteristics, file.path(out_dir, "av365_zero_price_raw_characteristics.csv"), row.names = FALSE)
write.csv(quarter_count_summary, file.path(out_dir, "av365_zero_price_quarter_counts.csv"), row.names = FALSE)
write.csv(analysis_sample_counts, file.path(out_dir, "av365_zero_price_analysis_sample_counts.csv"), row.names = FALSE)
write.csv(quarter_group_characteristics, file.path(out_dir, "av365_zero_price_quarter_characteristics.csv"), row.names = FALSE)
write.csv(quarter_group_by_panel, file.path(out_dir, "av365_zero_price_quarter_by_panel.csv"), row.names = FALSE)
write.csv(quarter_source_summary, file.path(out_dir, "av365_zero_price_quarter_by_source.csv"), row.names = FALSE)
write.csv(target_month_pattern, file.path(out_dir, "av365_zero_price_target_month_pattern.csv"), row.names = FALSE)

cat("Raw av365==0 by year:\n")
print(raw_year_summary, row.names = FALSE)
cat("\nRaw av365==0 by source/has_availability:\n")
print(raw_source_summary, row.names = FALSE)
cat("\nRaw group characteristics:\n")
print(raw_group_characteristics, row.names = FALSE)
cat("\nQuarter counts among full_condition:\n")
print(quarter_count_summary, row.names = FALSE)
cat("\nAnalysis sample counts:\n")
print(analysis_sample_counts, row.names = FALSE)
cat("\nQuarter-sum group characteristics:\n")
print(quarter_group_characteristics, row.names = FALSE)
cat("\nQuarter-sum group by panel:\n")
print(quarter_group_by_panel, n = 20, width = Inf)
cat("\nQuarter-sum group by source:\n")
print(quarter_source_summary, n = 20, width = Inf)
cat("\nTarget month patterns for av365_sum0:\n")
print(target_month_pattern, n = 50, width = Inf)
