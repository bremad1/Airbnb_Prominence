options(stringsAsFactors = FALSE)

.libPaths(c(file.path(getwd(), ".Rlib"), .libPaths()))

suppressPackageStartupMessages({
  library(dplyr)
  library(rdrobust)
})

# This script deliberately constructs the listing-quarter panel before any
# active-listing or trimming restriction is imposed.
#
# Order of operations:
#   1. Monthly observations -> complete listing-quarter observations.
#   2. Create first-month activity variables and the quarterly average price.
#   3. Link each listing to its immediately preceding quarter and calculate
#      the quarterly price difference.
#   4. Apply active/review restrictions.
#   5. Calculate both trim cutoffs separately within each quarter, using the
#      same pre-trim sample, and retain observations passing both cutoffs.
#   6. Estimate the main fuzzy RD specification for panels A, B, and C.

strict_bounds <- function(x, pct) {
  finite_x <- x[is.finite(x)]
  if (length(finite_x) == 0L) return(c(NA_real_, NA_real_))
  if (pct == 0) return(c(-Inf, Inf))
  quantile(
    finite_x,
    probs = c(pct, 1 - pct),
    na.rm = TRUE,
    names = FALSE
  )
}

first_value_at <- function(x, index, target_index = 1L) {
  value <- x[index == target_index & !is.na(x)]
  if (length(value) == 0L) NA_real_ else as.numeric(value[[1L]])
}

build_quarter_panel <- function(monthly_data) {
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
    ungroup() %>%
    group_by(id) %>%
    filter(n_distinct(host_id) == 1L) %>%
    ungroup()

  quarterly <- monthly %>%
    group_by(id, .quarter_index, quarter) %>%
    # "Balanced" means that all three calendar months of the quarter exist.
    filter(
      n_distinct(.quarter_month) == 3L,
      all(1:3 %in% .quarter_month),
      all(is.finite(price)),
      all(price > 0)
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
    ungroup() %>%
    group_by(host_id, .quarter_index) %>%
    filter(
      n_distinct(host_is_superhost[!is.na(host_is_superhost) &
                                     host_is_superhost != ""]) == 1L
    ) %>%
    ungroup() %>%
    arrange(id, .quarter_index) %>%
    group_by(id) %>%
    mutate(
      previous_quarter_index = lag(.quarter_index),
      previous_quarter = lag(quarter),
      ex_avg = lag(avg_price),
      consecutive_previous_quarter =
        .quarter_index - previous_quarter_index == 1L,
      price_diff = if_else(
        consecutive_previous_quarter,
        log(avg_price) - log(ex_avg),
        NA_real_
      ),
      raw_change = if_else(
        consecutive_previous_quarter,
        (avg_price - ex_avg) / ex_avg,
        NA_real_
      )
    ) %>%
    ungroup() %>%
    filter(
      quarter %in% c("Q323", "Q423", "Q124", "Q224", "Q324", "Q424")
    )

  stopifnot(
    !anyDuplicated(quarterly[c("id", "quarter")]),
    all(quarterly$quarter_months_observed == 3L)
  )

  quarterly
}

trim_quarter_sample <- function(
    data,
    avg_price_pct,
    price_diff_pct
) {
  data %>%
    group_by(quarter) %>%
    group_modify(function(.x, .y) {
      avg_bounds <- strict_bounds(.x$avg_price, avg_price_pct)
      diff_bounds <- strict_bounds(.x$price_diff, price_diff_pct)

      .x %>%
        mutate(
          avg_price_trim_low = avg_bounds[[1L]],
          avg_price_trim_high = avg_bounds[[2L]],
          price_diff_trim_low = diff_bounds[[1L]],
          price_diff_trim_high = diff_bounds[[2L]]
        ) %>%
        filter(
          is.finite(avg_price),
          is.finite(price_diff),
          avg_price > avg_bounds[[1L]],
          avg_price < avg_bounds[[2L]],
          price_diff > diff_bounds[[1L]],
          price_diff < diff_bounds[[2L]]
        )
    }) %>%
    ungroup()
}

make_trim_audit <- function(
    before_trim,
    after_trim,
    review_min,
    avg_price_pct,
    price_diff_pct
) {
  before_counts <- before_trim %>%
    count(quarter, name = "n_before_trim")

  after_trim %>%
    group_by(quarter) %>%
    summarise(
      n_after_trim = n(),
      avg_price_trim_low = first(avg_price_trim_low),
      avg_price_trim_high = first(avg_price_trim_high),
      price_diff_trim_low = first(price_diff_trim_low),
      price_diff_trim_high = first(price_diff_trim_high),
      .groups = "drop"
    ) %>%
    full_join(before_counts, by = "quarter") %>%
    mutate(
      n_after_trim = coalesce(n_after_trim, 0L),
      review_min = review_min,
      avg_price_trim = avg_price_pct,
      price_diff_trim = price_diff_pct
    ) %>%
    select(
      review_min,
      avg_price_trim,
      price_diff_trim,
      quarter,
      n_before_trim,
      n_after_trim,
      everything()
    )
}

fit_main_rd <- function(data) {
  if (nrow(data) == 0L) stop("No observations remain in this panel.")

  time_dummies <- as.data.frame(
    model.matrix(~ quarter - 1, data = data)
  )

  rd_args <- list(
    y = data$price_diff,
    x = data$running_scr - 4.75,
    fuzzy = data$host_is_superhost2,
    cluster = data$id,
    kernel = "tri",
    bwselect = "msetwo",
    p = 1,
    masspoints = "off",
    bwrestrict = TRUE
  )

  if (ncol(time_dummies) > 1L) {
    rd_args$covs <- as.matrix(time_dummies[, -1L, drop = FALSE])
  }
  if ("all" %in% names(formals(rdrobust))) rd_args$all <- TRUE

  do.call(rdrobust, rd_args)
}

run_panel <- function(panel_name, sample_data) {
  data <- sample_data
  if (panel_name == "A") data <- data %>% filter(ex_super == "t")
  if (panel_name == "B") data <- data %>% filter(ex_super == "f")
  data$quarter <- droplevels(factor(
    data$quarter,
    levels = c("Q323", "Q423", "Q124", "Q224", "Q324", "Q424")
  ))

  fit <- tryCatch(fit_main_rd(data), error = function(e) e)
  if (inherits(fit, "error")) {
    return(data.frame(
      panel = panel_name,
      raw_n = nrow(data),
      coef = NA_real_,
      se = NA_real_,
      p = NA_real_,
      h_left = NA_real_,
      h_right = NA_real_,
      obs_h = NA_integer_,
      error = conditionMessage(fit)
    ))
  }

  data.frame(
    panel = panel_name,
    raw_n = nrow(data),
    coef = fit$Estimate[[1L]],
    se = fit$se[[1L]],
    p = fit$pv[[1L]],
    h_left = fit$bws[1, 1],
    h_right = fit$bws[1, 2],
    obs_h = sum(fit$N_h),
    error = NA_character_
  )
}

run_analysis <- function(
    results_file =
      "results/balanced_quarterly_active_review_trim_results.csv",
    data_file =
      "results/balanced_quarterly_active_review_trim_data.rds",
    audit_file =
      "results/balanced_quarterly_active_review_trim_audit.csv"
) {
  load("RData/Entire.RData")

  quarterly_panel <- build_quarter_panel(Entire)

  review_thresholds <- seq(0L, 50L, by = 5L)
  avg_price_trims <- c(0, 0.01)
  price_diff_trims <- c(0, 0.01)
  panels <- c("A", "B", "C")

  results_list <- list()
  audit_list <- list()

  for (review_min in review_thresholds) {
    # Active/review restrictions are imposed before either trim cutoff is
    # calculated. A missing/non-consecutive previous quarter produces an NA
    # price_diff above and is therefore omitted here without an additional
    # balanced-pair filter.
    eligible <- quarterly_panel %>%
      filter(
        !is.na(first_month_ltm),
        first_month_ltm >= 1,
        !is.na(first_month_number_of_reviews),
        first_month_number_of_reviews >= review_min,
        is.finite(price_diff)
      )

    for (avg_price_trim in avg_price_trims) {
      for (price_diff_trim in price_diff_trims) {
        sample_data <- trim_quarter_sample(
          eligible,
          avg_price_pct = avg_price_trim,
          price_diff_pct = price_diff_trim
        )

        audit_list[[length(audit_list) + 1L]] <- make_trim_audit(
          eligible,
          sample_data,
          review_min,
          avg_price_trim,
          price_diff_trim
        )

        panel_results <- bind_rows(lapply(
          panels,
          run_panel,
          sample_data = sample_data
        )) %>%
          mutate(
            review_min = review_min,
            avg_price_trim = avg_price_trim,
            price_diff_trim = price_diff_trim
          )

        results_list[[length(results_list) + 1L]] <- panel_results
      }
    }
  }

  results <- bind_rows(results_list) %>%
    select(
      review_min,
      avg_price_trim,
      price_diff_trim,
      panel,
      everything()
    ) %>%
    arrange(
      review_min,
      price_diff_trim,
      avg_price_trim,
      match(panel, panels)
    )

  trim_audit <- bind_rows(audit_list) %>%
    arrange(
      review_min,
      price_diff_trim,
      avg_price_trim,
      match(
        quarter,
        c("Q323", "Q423", "Q124", "Q224", "Q324", "Q424")
      )
    )

  for (path in c(results_file, data_file, audit_file)) {
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  }

  write.csv(results, results_file, row.names = FALSE)
  write.csv(trim_audit, audit_file, row.names = FALSE)
  saveRDS(
    list(
      quarterly_panel = quarterly_panel,
      results = results,
      trim_audit = trim_audit
    ),
    data_file
  )

  message("Quarter panel rows: ", nrow(quarterly_panel))
  message("Regression result rows: ", nrow(results))
  message("Results: ", normalizePath(results_file, winslash = "/"))
  message("Data: ", normalizePath(data_file, winslash = "/"))
  message("Trim audit: ", normalizePath(audit_file, winslash = "/"))

  invisible(list(
    quarterly_panel = quarterly_panel,
    results = results,
    trim_audit = trim_audit
  ))
}

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  results_file <- if (length(args) >= 1L) args[[1L]] else
    "results/balanced_quarterly_active_review_trim_results.csv"
  data_file <- if (length(args) >= 2L) args[[2L]] else
    "results/balanced_quarterly_active_review_trim_data.rds"
  audit_file <- if (length(args) >= 3L) args[[3L]] else
    "results/balanced_quarterly_active_review_trim_audit.csv"

  run_analysis(results_file, data_file, audit_file)
}
