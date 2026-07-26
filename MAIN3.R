load("RData/Entire.RData")

options(stringsAsFactors = FALSE)

.libPaths(c(
  file.path(getwd(), ".Rlib"),
  "C:/Users/admin/Documents/Codex/2026-07-09/s/work/rdrobust_versions/rdrobust_3_0_0",
  "C:/Users/admin/Documents/Codex/2026-07-07/rlt/work/Rlibs",
  .libPaths()
))

suppressPackageStartupMessages({
  library(dplyr)
  library(rdrobust)
  library(rddensity)
})

args <- commandArgs(trailingOnly = TRUE)
output_dir <- if (length(args) >= 1L) args[[1L]] else "results"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

cutoff <- 4.75
ltm_min <- 1
review_min <- 30

quarter_specs <- list(
  Q323 = list(valid = c(5, 6), analysis = 4:9, year = 2023L,
              target = c(8, 9), cross = FALSE),
  Q423 = list(valid = c(8, 9), analysis = 7:12, year = 2023L,
              target = c(11, 12), cross = FALSE),
  Q124 = list(valid = c(11, 12), analysis = c(10, 11, 12, 1, 2, 3),
              year = 2024L, target = c(2, 3), cross = TRUE),
  Q224 = list(valid = c(2, 3), analysis = 1:6, year = 2024L,
              target = c(5, 6), cross = FALSE),
  Q324 = list(valid = c(5, 6), analysis = 4:9, year = 2024L,
              target = c(8, 9), cross = FALSE),
  Q424 = list(valid = c(8, 9), analysis = 7:12, year = 2024L,
              target = c(11, 12), cross = FALSE)
)

prepare_entire <- function(data, activity_filter) {
  x <- data %>%
    mutate(
      .year = as.integer(format(as.Date(Date), "%Y")),
      .month = as.integer(format(as.Date(Date), "%m")),
      .last_review_date = as.Date(last_review)
    )

  if (activity_filter) {
    # Both restrictions define the RD analysis population. No trimming follows.
    x <- x %>% filter(
      !is.na(first_month_ltm), first_month_ltm >= ltm_min,
      !is.na(first_month_number_of_reviews),
      first_month_number_of_reviews >= review_min
    )
  }

  x %>%
    arrange(id, Date) %>%
    group_by(id) %>%
    mutate(
      date_diff = 12L * (.year - lag(.year)) + (.month - lag(.month)),
      ex_price1 = if_else(date_diff == 1L, lag(price, 1), NA_real_),
      ex_price2 = if_else(
        date_diff == 2L, lag(price, 1),
        if_else(date_diff == 1L, lag(price, 2), NA_real_)
      ),
      ex_price3 = if_else(
        date_diff == 3L, lag(price, 1),
        if_else(date_diff == 2L, lag(price, 2),
          if_else(date_diff == 1L, lag(price, 3), NA_real_))
      ),
      ex_price4 = if_else(
        date_diff == 4L, lag(price, 1),
        if_else(date_diff == 3L, lag(price, 2),
          if_else(date_diff == 2L, lag(price, 3),
            if_else(date_diff == 1L, lag(price, 4), NA_real_)))
      )
    ) %>%
    ungroup()
}

build_quarter <- function(data, spec) {
  no_dup <- data %>%
    group_by(id) %>%
    filter(n_distinct(host_id) == 1L) %>%
    ungroup()

  if (!spec$cross) {
    valid_ids <- no_dup %>%
      filter(.year == spec$year, .month %in% spec$valid) %>%
      filter(!is.na(price), host_is_superhost != "",
             !is.na(host_is_superhost)) %>%
      group_by(host_id) %>%
      filter(n_distinct(host_is_superhost) == 1L) %>%
      distinct(id) %>%
      pull(id)

    temp <- no_dup %>%
      filter(.year == spec$year, .month %in% spec$analysis) %>%
      filter(host_is_superhost != "", !is.na(host_is_superhost),
             !is.na(price)) %>%
      group_by(id) %>%
      filter(all(spec$analysis %in% .month)) %>%
      ungroup() %>%
      filter(.year == spec$year, .month %in% spec$target)
  } else {
    valid_ids <- no_dup %>%
      filter(.year == spec$year - 1L, .month %in% spec$valid,
             !is.na(price), host_is_superhost != "",
             !is.na(host_is_superhost)) %>%
      group_by(host_id) %>%
      filter(n_distinct(host_is_superhost) == 1L) %>%
      distinct(id) %>%
      pull(id)

    temp <- no_dup %>%
      filter(
        (.year == spec$year - 1L & .month %in% spec$analysis[1:3]) |
          ((.year == spec$year & .month %in% spec$analysis[4:6]) &
             host_is_superhost != "" & !is.na(host_is_superhost) &
             !is.na(price))
      ) %>%
      group_by(id) %>%
      filter(all(spec$analysis %in% .month)) %>%
      ungroup() %>%
      filter(.year == spec$year, .month %in% spec$target)
  }

  temp %>%
    filter(id %in% valid_ids) %>%
    group_by(host_id) %>%
    filter(n_distinct(host_is_superhost) == 1L) %>%
    ungroup() %>%
    group_by(id) %>%
    mutate(avg_price = ifelse(
      sum(!is.na(price)) > 1L, mean(price, na.rm = TRUE),
      price[!is.na(price)]
    )) %>%
    slice(1L) %>%
    ungroup() %>%
    mutate(
      ex_avg = ifelse(
        .month %in% c(2, 5, 8, 11),
        rowMeans(cbind(ex_price2, ex_price3), na.rm = TRUE),
        ifelse(
          .month %in% c(1, 4, 7, 10),
          rowMeans(cbind(ex_price1, ex_price2), na.rm = TRUE),
          rowMeans(cbind(ex_price3, ex_price4), na.rm = TRUE)
        )
      ),
      raw_change = (avg_price - ex_avg) / ex_avg,
      price_diff = log(avg_price) - log(ex_avg)
    ) %>%
    filter(is.finite(raw_change), is.finite(running_scr))
}

add_price_quartiles <- function(d) {
  q <- quantile(d$ex_avg, c(.25, .50, .75), na.rm = TRUE, names = FALSE)
  d %>% mutate(
    ex_q1 = as.integer(ex_avg < q[[1L]]),
    ex_q2 = as.integer(ex_avg >= q[[1L]] & ex_avg < q[[2L]]),
    ex_q3 = as.integer(ex_avg >= q[[2L]] & ex_avg < q[[3L]]),
    ex_q4 = as.integer(ex_avg >= q[[3L]])
  )
}

make_cell <- function(activity_filter) {
  prepared <- prepare_entire(Entire, activity_filter = activity_filter)
  lapply(quarter_specs, function(spec) build_quarter(prepared, spec)) %>%
    lapply(add_price_quartiles) %>%
    bind_rows(.id = "quarter") %>%
    mutate(date3_ym = factor(format(as.Date(Date), "%Y-%m")))
}

condition_names <- c("FULL", "Q1Q2", "Q2Q3", "Q3Q4")
condition_index <- function(d, condition) {
  switch(condition,
    FULL = rep(TRUE, nrow(d)),
    Q1Q2 = d$ex_q1 == 1 | d$ex_q2 == 1,
    Q2Q3 = d$ex_q2 == 1 | d$ex_q3 == 1,
    Q3Q4 = d$ex_q3 == 1 | d$ex_q4 == 1
  )
}

make_covariates <- function(d) {
  mm <- as.data.frame(model.matrix(~ date3_ym - 1, data = d))
  if (ncol(mm) <= 1L) return(NULL)
  as.matrix(mm[, -1, drop = FALSE])
}

fit_one <- function(d, specification, first_fit = NULL) {
  rd_args <- list(
    y = d$price_diff,
    x = d$running_scr - cutoff,
    fuzzy = d$host_is_superhost2,
    all = TRUE,
    cluster = d$id,
    kernel = "tri",
    bwselect = "msetwo",
    p = 1,
    masspoints = "off",
    bwrestrict = TRUE
  )
  if (specification <= 5L) rd_args$covs <- make_covariates(d)
  if (specification == 2L) {
    rd_args$h <- 2 * first_fit$bws[1, 1:2]
  } else if (specification == 3L) {
    rd_args$h <- c(.2, .1)
  } else if (specification == 4L) {
    rd_args$h <- c(.3, .15)
  } else if (specification == 5L) {
    rd_args$h <- c(.4, .2)
  }
  do.call(rdrobust, rd_args)
}

run_condition <- function(cell, condition) {
  d <- cell[condition_index(cell, condition), , drop = FALSE]
  d$date3_ym <- droplevels(d$date3_ym)
  first_fit <- tryCatch(fit_one(d, 1L), error = function(e) e)
  fits <- vector("list", 6L)
  fits[[1L]] <- first_fit
  for (specification in 2:6) {
    fits[[specification]] <- if (inherits(first_fit, "error")) first_fit else
      tryCatch(fit_one(d, specification, first_fit), error = function(e) e)
  }

  bind_rows(lapply(seq_along(fits), function(specification) {
    fit <- fits[[specification]]
    if (inherits(fit, "error")) {
      data.frame(
        condition = condition, specification = specification,
        raw_n = nrow(d), coef_conv = NA_real_, se_conv = NA_real_,
        p_conv = NA_real_, coef_bc = NA_real_, se_robust = NA_real_,
        p_robust = NA_real_, h_left = NA_real_, h_right = NA_real_,
        obs_h = NA_integer_, error = conditionMessage(fit)
      )
    } else {
      data.frame(
        condition = condition, specification = specification,
        raw_n = nrow(d), coef_conv = fit$Estimate[[1L]],
        se_conv = fit$se[[1L]], p_conv = fit$pv[[1L]],
        coef_bc = fit$Estimate[[2L]], se_robust = fit$se[[3L]],
        p_robust = fit$pv[[3L]], h_left = fit$bws[1, 1],
        h_right = fit$bws[1, 2], obs_h = sum(fit$N_h),
        error = NA_character_
      )
    }
  }))
}

run_density <- function(x, sample_name, primary) {
  x <- x[is.finite(x)]
  fit <- tryCatch(
    rddensity(x, c = cutoff, massPoints = TRUE),
    error = function(e) e
  )
  if (inherits(fit, "error")) {
    return(data.frame(
      sample = sample_name, primary = primary, cutoff = cutoff,
      n = length(x), n_left = sum(x < cutoff), n_right = sum(x >= cutoff),
      h_left = NA_real_, h_right = NA_real_, n_eff_left = NA_integer_,
      n_eff_right = NA_integer_, statistic = NA_real_, p_value = NA_real_,
      error = conditionMessage(fit)
    ))
  }
  data.frame(
    sample = sample_name, primary = primary, cutoff = cutoff,
    n = fit$N$full, n_left = fit$N$left, n_right = fit$N$right,
    h_left = fit$h$left, h_right = fit$h$right,
    n_eff_left = fit$N$eff_left, n_eff_right = fit$N$eff_right,
    statistic = fit$test$t_jk, p_value = fit$test$p_jk,
    error = NA_character_
  )
}

message("Building Panel B sample before activity filters for McCrary test...")
panel_b_prefilter <- make_cell(activity_filter = FALSE) %>%
  filter(ex_super == "f")

message("Building filtered, no-trimming Panel B RD sample...")
panel_b_filtered <- make_cell(activity_filter = TRUE) %>%
  filter(ex_super == "f")

message("Running Panel B fuzzy RD specifications...")
rd_results <- bind_rows(lapply(
  condition_names, function(x) run_condition(panel_b_filtered, x)
)) %>%
  mutate(
    panel = "B", price_trim = 0, change_trim = 0,
    first_month_ltm_min = ltm_min,
    first_month_number_of_reviews_min = review_min
  ) %>%
  select(panel, condition, specification, price_trim, change_trim,
         first_month_ltm_min, first_month_number_of_reviews_min,
         everything())

message("Running McCrary density tests at 4.75...")
density_results <- bind_rows(
  run_density(
    panel_b_prefilter$running_scr,
    "Panel B before LTM/review filters", FALSE
  ),
  run_density(
    panel_b_filtered$running_scr,
    "Panel B after LTM>=1 and reviews>=30 filters", TRUE
  )
)

rd_csv <- file.path(output_dir, "MAIN3_panel_b_no_trimming_rd.csv")
density_csv <- file.path(output_dir, "MAIN3_panel_b_mccrary_4_75.csv")
bundle_rds <- file.path(output_dir, "MAIN3_panel_b_results.rds")
summary_txt <- file.path(output_dir, "MAIN3_summary.txt")

write.csv(rd_results, rd_csv, row.names = FALSE, na = "")
write.csv(density_results, density_csv, row.names = FALSE, na = "")
saveRDS(list(rd = rd_results, mccrary = density_results), bundle_rds)

main_rd <- rd_results %>%
  filter(condition == "FULL", specification == 1L)
primary_density <- density_results %>% filter(primary)
summary_lines <- c(
  "MAIN3: Panel B, no trimming",
  sprintf("Filters for RD: first_month_ltm >= %d; first_month_number_of_reviews >= %d",
          ltm_min, review_min),
  sprintf("FULL main RD: N=%d, estimate=%.6f, SE=%.6f, p=%.6g",
          main_rd$raw_n, main_rd$coef_conv, main_rd$se_conv, main_rd$p_conv),
  sprintf(paste0("Primary McCrary (filtered analysis sample, cutoff %.2f): ",
                 "N=%d, statistic=%.6f, p=%.6g"),
          cutoff, primary_density$n, primary_density$statistic,
          primary_density$p_value),
  "The before-filter McCrary result is reported as a sensitivity check."
)
writeLines(summary_lines, summary_txt)

cat(paste(summary_lines, collapse = "\n"), "\n")
message("RD results: ", normalizePath(rd_csv, winslash = "/"))
message("McCrary results: ", normalizePath(density_csv, winslash = "/"))
