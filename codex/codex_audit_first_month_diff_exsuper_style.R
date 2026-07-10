base_dir <- "C:/Users/admin/Documents/Codex/2026-07-09/s"
project_root <- "C:/Users/admin/Documents/Codex/2026-07-07/rlt/work/Airbnb_Prominence"
deps_lib <- "C:/Users/admin/Documents/Codex/2026-07-07/rlt/work/Rlibs"
out_dir <- file.path(base_dir, "outputs")

.libPaths(c(deps_lib, .libPaths()))
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages(library(dplyr))

date_year <- function(x) as.integer(format(as.Date(x), "%Y"))
date_month <- function(x) as.integer(format(as.Date(x), "%m"))

load(file.path(project_root, "Quarterly_dataset1.RData"))
load(file.path(project_root, "RData", "Entire.RData"))

quarter_list <- list(Q323 = Q323, Q423 = Q423, Q124 = Q124, Q224 = Q224, Q324 = Q324, Q424 = Q424)

z <- bind_rows(lapply(names(quarter_list), function(qname) {
  quarter_list[[qname]] %>% mutate(quarter = qname)
}))

# This mirrors the ex_super2-style case_when, but uses id and number_of_reviews.
previous_first_month <- Entire %>%
  select(Date, id, number_of_reviews) %>%
  distinct(Date, id, .keep_all = TRUE) %>%
  arrange(id, Date) %>%
  group_by(id) %>%
  mutate(
    previous_first_month_number_of_reviews_case = case_when(
      date_month(Date) %in% c(7, 8, 9) & date_year(Date) == 2023 ~
        ifelse(any(date_month(Date) == 4 & date_year(Date) == 2023),
          as.numeric(first(number_of_reviews[date_month(Date) == 4 & date_year(Date) == 2023])),
          NA_real_
        ),
      date_month(Date) %in% c(10, 11, 12) & date_year(Date) == 2023 ~
        ifelse(any(date_month(Date) == 7 & date_year(Date) == 2023),
          as.numeric(first(number_of_reviews[date_month(Date) == 7 & date_year(Date) == 2023])),
          NA_real_
        ),
      date_month(Date) %in% c(1, 2, 3) & date_year(Date) == 2024 ~
        ifelse(any(date_month(Date) == 10 & date_year(Date) == 2023),
          as.numeric(first(number_of_reviews[date_month(Date) == 10 & date_year(Date) == 2023])),
          NA_real_
        ),
      date_month(Date) %in% c(4, 5, 6) & date_year(Date) == 2024 ~
        ifelse(any(date_month(Date) == 1 & date_year(Date) == 2024),
          as.numeric(first(number_of_reviews[date_month(Date) == 1 & date_year(Date) == 2024])),
          NA_real_
        ),
      date_month(Date) %in% c(7, 8, 9) & date_year(Date) == 2024 ~
        ifelse(any(date_month(Date) == 4 & date_year(Date) == 2024),
          as.numeric(first(number_of_reviews[date_month(Date) == 4 & date_year(Date) == 2024])),
          NA_real_
        ),
      date_month(Date) %in% c(10, 11, 12) & date_year(Date) == 2024 ~
        ifelse(any(date_month(Date) == 7 & date_year(Date) == 2024),
          as.numeric(first(number_of_reviews[date_month(Date) == 7 & date_year(Date) == 2024])),
          NA_real_
        ),
      TRUE ~ NA_real_
    )
  ) %>%
  ungroup() %>%
  select(Date, id, previous_first_month_number_of_reviews_case)

quarter_map <- data.frame(
  quarter = c("Q323", "Q423", "Q124", "Q224", "Q324", "Q424"),
  current_ym = c("2023-07", "2023-10", "2024-01", "2024-04", "2024-07", "2024-10"),
  previous_ym = c("2023-04", "2023-07", "2023-10", "2024-01", "2024-04", "2024-07"),
  stringsAsFactors = FALSE
)

entire_reviews <- Entire %>%
  mutate(ym = format(as.Date(Date), "%Y-%m")) %>%
  filter(ym %in% unique(c(quarter_map$current_ym, quarter_map$previous_ym))) %>%
  group_by(id, ym) %>%
  summarise(number_of_reviews_snapshot = first(number_of_reviews), .groups = "drop")

quarter_review_diff <- bind_rows(lapply(seq_len(nrow(quarter_map)), function(i) {
  qm <- quarter_map[i, ]
  current <- entire_reviews %>%
    filter(ym == qm$current_ym) %>%
    transmute(id, current_first_month_number_of_reviews_join = number_of_reviews_snapshot)
  previous <- entire_reviews %>%
    filter(ym == qm$previous_ym) %>%
    transmute(id, previous_first_month_number_of_reviews_join = number_of_reviews_snapshot)

  full_join(current, previous, by = "id") %>%
    mutate(
      quarter = qm$quarter,
      current_ym = qm$current_ym,
      previous_ym = qm$previous_ym,
      listing_first_month_review_diff_join =
        current_first_month_number_of_reviews_join - previous_first_month_number_of_reviews_join
    )
}))

audit <- z %>%
  left_join(previous_first_month, by = c("Date", "id")) %>%
  left_join(quarter_review_diff, by = c("quarter", "id")) %>%
  mutate(
    listing_first_month_review_diff_case =
      first_month_number_of_reviews - previous_first_month_number_of_reviews_case,
    current_first_month_mismatch =
      first_month_number_of_reviews != current_first_month_number_of_reviews_join,
    previous_first_month_mismatch =
      previous_first_month_number_of_reviews_case != previous_first_month_number_of_reviews_join,
    diff_mismatch =
      listing_first_month_review_diff_case != listing_first_month_review_diff_join
  )

audit_summary <- audit %>%
  group_by(quarter, current_ym, previous_ym) %>%
  summarise(
    rows = n(),
    missing_current_first_month = sum(is.na(first_month_number_of_reviews)),
    missing_previous_first_month_case = sum(is.na(previous_first_month_number_of_reviews_case)),
    missing_previous_first_month_join = sum(is.na(previous_first_month_number_of_reviews_join)),
    current_first_month_mismatch = sum(current_first_month_mismatch, na.rm = TRUE),
    previous_first_month_mismatch = sum(previous_first_month_mismatch, na.rm = TRUE),
    diff_mismatch = sum(diff_mismatch, na.rm = TRUE),
    negative_diff_case = sum(listing_first_month_review_diff_case < 0, na.rm = TRUE),
    mean_diff_case = mean(listing_first_month_review_diff_case, na.rm = TRUE),
    .groups = "drop"
  )

write.csv(audit_summary, file.path(out_dir, "first_month_diff_exsuper_style_audit.csv"), row.names = FALSE)

cat("ex_super2-style previous-first-month audit\n")
print(audit_summary)
cat("total previous mismatch=", sum(audit$previous_first_month_mismatch, na.rm = TRUE), "\n")
cat("total diff mismatch=", sum(audit$diff_mismatch, na.rm = TRUE), "\n")
