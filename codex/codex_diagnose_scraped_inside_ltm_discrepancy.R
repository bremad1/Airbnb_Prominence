base_dir <- "C:/Users/admin/Documents/Codex/2026-07-09/s"
project_root <- "C:/Users/admin/Documents/Codex/2026-07-07/rlt/work/Airbnb_Prominence"
deps_lib <- "C:/Users/admin/Documents/Codex/2026-07-07/rlt/work/Rlibs"
out_dir <- file.path(base_dir, "outputs")

.libPaths(c(deps_lib, .libPaths()))
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages(library(dplyr))

load(file.path(project_root, "Quarterly_dataset1.RData"))
load(file.path(project_root, "RData", "Entire.RData"))

quarter_list <- list(Q323 = Q323, Q423 = Q423, Q124 = Q124, Q224 = Q224, Q324 = Q324, Q424 = Q424)

z <- bind_rows(lapply(names(quarter_list), function(qname) {
  quarter_list[[qname]] %>% mutate(quarter = qname)
}))

full_condition <- z$ex_q1 == 1 | z$ex_q2 == 1 | z$ex_q3 == 1 | z$ex_q4 == 1
full_condition[is.na(full_condition)] <- FALSE

z <- z %>%
  mutate(
    full_condition = full_condition,
    scraped_host_ltm_count = case_when(
      quarter == "Q323" ~ total_running_23jul_count,
      quarter == "Q423" ~ total_running_23oct_count,
      quarter == "Q124" ~ total_running_24jan_count,
      quarter == "Q224" ~ total_running_24apr_count,
      quarter == "Q324" ~ total_running_24jul_count,
      quarter == "Q424" ~ total_running_24oct_count,
      TRUE ~ NA_real_
    ),
    ltm_map_mismatch = ltm_scr != scraped_host_ltm_count
  )

host_quarter <- z %>%
  distinct(
    quarter,
    host_id,
    id,
    full_condition,
    ex_super,
    ex_super2,
    scraped_host_ltm_count,
    ltm_scr,
    ex_quarter_ltm,
    first_month_ltm,
    number_of_reviews_ltm,
    calculated_host_listings_count,
    host_listings_count,
    host_total_listings_count,
    source
  ) %>%
  group_by(quarter, host_id) %>%
  summarise(
    host_n_listings_q = n_distinct(id),
    any_full = any(full_condition, na.rm = TRUE),
    any_panel_b = any(ex_super == "f" & full_condition, na.rm = TRUE),
    any_panel_b_ex2 = any(ex_super == "f" & ex_super2 == "t" & full_condition, na.rm = TRUE),
    scraped_unique_n = n_distinct(scraped_host_ltm_count[!is.na(scraped_host_ltm_count)]),
    scraped = ifelse(scraped_unique_n == 1, first(scraped_host_ltm_count[!is.na(scraped_host_ltm_count)]), NA_real_),
    inside_ex_ltm = sum(ex_quarter_ltm, na.rm = TRUE),
    inside_ex_missing_n = sum(is.na(ex_quarter_ltm)),
    inside_first_ltm = sum(first_month_ltm, na.rm = TRUE),
    inside_first_missing_n = sum(is.na(first_month_ltm)),
    inside_current_ltm = sum(number_of_reviews_ltm, na.rm = TRUE),
    inside_current_missing_n = sum(is.na(number_of_reviews_ltm)),
    calc_host_listings = suppressWarnings(max(calculated_host_listings_count, na.rm = TRUE)),
    host_listings = suppressWarnings(max(host_listings_count, na.rm = TRUE)),
    host_total_listings = suppressWarnings(max(host_total_listings_count, na.rm = TRUE)),
    any_previous_scrape = any(source == "previous scrape", na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    calc_host_listings = ifelse(is.infinite(calc_host_listings), NA_real_, calc_host_listings),
    host_listings = ifelse(is.infinite(host_listings), NA_real_, host_listings),
    host_total_listings = ifelse(is.infinite(host_total_listings), NA_real_, host_total_listings),
    diff_ex = scraped - inside_ex_ltm,
    diff_first = scraped - inside_first_ltm,
    diff_current = scraped - inside_current_ltm,
    abs_diff_ex = abs(diff_ex),
    abs_diff_first = abs(diff_first),
    abs_diff_current = abs(diff_current),
    best_inside_snapshot = case_when(
      is.na(scraped) ~ NA_character_,
      abs_diff_ex <= abs_diff_first & abs_diff_ex <= abs_diff_current ~ "ex_quarter_ltm",
      abs_diff_first <= abs_diff_ex & abs_diff_first <= abs_diff_current ~ "first_month_ltm",
      abs_diff_current <= abs_diff_ex & abs_diff_current <= abs_diff_first ~ "current_month_ltm",
      TRUE ~ NA_character_
    ),
    exact_ex = !is.na(scraped) & inside_ex_missing_n == 0 & scraped == inside_ex_ltm,
    exact_first = !is.na(scraped) & inside_first_missing_n == 0 & scraped == inside_first_ltm,
    exact_current = !is.na(scraped) & inside_current_missing_n == 0 & scraped == inside_current_ltm,
    within1_ex = !is.na(scraped) & inside_ex_missing_n == 0 & abs_diff_ex <= 1,
    within2_ex = !is.na(scraped) & inside_ex_missing_n == 0 & abs_diff_ex <= 2,
    scraped_gt_ex = diff_ex > 0,
    inside_gt_ex = diff_ex < 0,
    observed_all_calc_listings = !is.na(calc_host_listings) & host_n_listings_q == calc_host_listings,
    observed_lt_calc_listings = !is.na(calc_host_listings) & host_n_listings_q < calc_host_listings,
    single_listing_host_q = host_n_listings_q == 1
  )

summarise_sample <- function(data, sample_name) {
  data.frame(
    sample = sample_name,
    host_quarters = nrow(data),
    exact_ex_n = sum(data$exact_ex, na.rm = TRUE),
    exact_ex_share = mean(data$exact_ex, na.rm = TRUE),
    within1_ex_share = mean(data$within1_ex, na.rm = TRUE),
    within2_ex_share = mean(data$within2_ex, na.rm = TRUE),
    exact_first_share = mean(data$exact_first, na.rm = TRUE),
    exact_current_share = mean(data$exact_current, na.rm = TRUE),
    scraped_gt_ex_share = mean(data$scraped_gt_ex, na.rm = TRUE),
    inside_gt_ex_share = mean(data$inside_gt_ex, na.rm = TRUE),
    median_diff_ex = median(data$diff_ex, na.rm = TRUE),
    mean_diff_ex = mean(data$diff_ex, na.rm = TRUE),
    median_abs_diff_ex = median(data$abs_diff_ex, na.rm = TRUE),
    mean_abs_diff_ex = mean(data$abs_diff_ex, na.rm = TRUE),
    median_scraped = median(data$scraped, na.rm = TRUE),
    median_inside_ex = median(data$inside_ex_ltm, na.rm = TRUE),
    single_listing_share = mean(data$single_listing_host_q, na.rm = TRUE),
    observed_all_calc_share = mean(data$observed_all_calc_listings, na.rm = TRUE),
    previous_scrape_share = mean(data$any_previous_scrape, na.rm = TRUE),
    best_ex_share = mean(data$best_inside_snapshot == "ex_quarter_ltm", na.rm = TRUE),
    best_first_share = mean(data$best_inside_snapshot == "first_month_ltm", na.rm = TRUE),
    best_current_share = mean(data$best_inside_snapshot == "current_month_ltm", na.rm = TRUE),
    stringsAsFactors = FALSE
  )
}

sample_summary <- bind_rows(
  summarise_sample(host_quarter, "all_host_quarters"),
  summarise_sample(host_quarter %>% filter(any_full), "full_condition"),
  summarise_sample(host_quarter %>% filter(any_panel_b), "panel_b_no_ex2"),
  summarise_sample(host_quarter %>% filter(any_panel_b_ex2), "panel_b_ex2"),
  summarise_sample(host_quarter %>% filter(any_panel_b, single_listing_host_q), "panel_b_single_listing"),
  summarise_sample(host_quarter %>% filter(any_panel_b, observed_all_calc_listings), "panel_b_observed_all_calc")
)

coverage_summary <- host_quarter %>%
  filter(any_panel_b) %>%
  mutate(
    coverage_group = case_when(
      single_listing_host_q ~ "single_listing_host_q",
      observed_all_calc_listings ~ "observed_all_calc_listings",
      observed_lt_calc_listings ~ "observed_lt_calc_listings",
      TRUE ~ "unknown_coverage"
    )
  ) %>%
  group_by(coverage_group) %>%
  summarise(
    host_quarters = n(),
    exact_ex_share = mean(exact_ex, na.rm = TRUE),
    within2_ex_share = mean(within2_ex, na.rm = TRUE),
    scraped_gt_ex_share = mean(scraped_gt_ex, na.rm = TRUE),
    median_diff_ex = median(diff_ex, na.rm = TRUE),
    mean_diff_ex = mean(diff_ex, na.rm = TRUE),
    median_abs_diff_ex = median(abs_diff_ex, na.rm = TRUE),
    median_scraped = median(scraped, na.rm = TRUE),
    median_inside_ex = median(inside_ex_ltm, na.rm = TRUE),
    .groups = "drop"
  )

quarter_summary <- host_quarter %>%
  filter(any_panel_b) %>%
  group_by(quarter) %>%
  summarise(
    host_quarters = n(),
    exact_ex_share = mean(exact_ex, na.rm = TRUE),
    within2_ex_share = mean(within2_ex, na.rm = TRUE),
    scraped_gt_ex_share = mean(scraped_gt_ex, na.rm = TRUE),
    median_diff_ex = median(diff_ex, na.rm = TRUE),
    median_abs_diff_ex = median(abs_diff_ex, na.rm = TRUE),
    exact_first_share = mean(exact_first, na.rm = TRUE),
    exact_current_share = mean(exact_current, na.rm = TRUE),
    .groups = "drop"
  )

write.csv(host_quarter, file.path(out_dir, "scraped_inside_ltm_host_quarter_diagnostics.csv"), row.names = FALSE)
write.csv(sample_summary, file.path(out_dir, "scraped_inside_ltm_sample_summary.csv"), row.names = FALSE)
write.csv(coverage_summary, file.path(out_dir, "scraped_inside_ltm_coverage_summary.csv"), row.names = FALSE)
write.csv(quarter_summary, file.path(out_dir, "scraped_inside_ltm_quarter_summary.csv"), row.names = FALSE)

cat("ltm_scr date-mapped count mismatches:", sum(z$ltm_map_mismatch, na.rm = TRUE), "\n\n")
cat("Sample summary\n")
print(sample_summary)
cat("\nCoverage summary: Panel B no ex_super2\n")
print(coverage_summary)
cat("\nQuarter summary: Panel B no ex_super2\n")
print(quarter_summary)
