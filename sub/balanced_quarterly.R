options(stringsAsFactors = FALSE)

.libPaths(c(file.path(getwd(), ".Rlib"), .libPaths()))

suppressPackageStartupMessages({
  library(dplyr)
  library(rdrobust)
})

load("RData/Entire.RData")

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

target_quarters <- c("Q323", "Q423", "Q124", "Q224", "Q324", "Q424")
history_quarter <- "Q223"

quarter_index_from_label <- function(label) {
  quarter_number <- as.integer(substr(label, 2L, 2L))
  year <- 2000L + as.integer(substr(label, 3L, 4L))
  4L * year + quarter_number
}

# Balanced-panel definition used here:
#   * the same listing must have all three monthly observations in every one
#     of the six target quarters;
#   * Q2 2023 is not a seventh balanced-panel quarter. Only its last two
#     monthly prices are required to construct the Q3 2023 baseline price;
#   * host status must be observed and constant throughout each quarter;
#   * avg_price uses only months 2 and 3 of each quarter.
build_quarter_panel <- function(monthly_data) {
  target_indices <- quarter_index_from_label(target_quarters)
  history_index <- quarter_index_from_label(history_quarter)

  cat(sprintf(
    "[build_balanced_quarter_panel] input monthly rows: %d\n",
    nrow(monthly_data)
  ))

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
    # Prevent multiple scrapes in one id-month from receiving extra weight.
    group_by(id, .year_month) %>%
    slice(1L) %>%
    ungroup() %>%
    # A listing transferred between hosts is not treated as one panel unit.
    group_by(id) %>%
    filter(
      all(!is.na(host_id)),
      n_distinct(host_id) == 1L
    ) %>%
    ungroup()

  quarterly_candidates <- monthly %>%
    filter(.quarter_index %in% target_indices) %>%
    group_by(id, .quarter_index, quarter) %>%
    # All three calendar months must exist. Host status must be nonmissing
    # and identical in all three months of the listing-quarter.
    filter(
      n_distinct(.quarter_month) == 3L,
      all(1:3 %in% .quarter_month),
      all(is.finite(price)),
      all(price > 0),
      all(!is.na(host_is_superhost)),
      all(host_is_superhost != ""),
      n_distinct(host_is_superhost) == 1L
    ) %>%
    arrange(.quarter_month, Date, .by_group = TRUE) %>%
    mutate(
      # Deliberately use only the last two months of the quarter.
      avg_price = mean(price[.quarter_month %in% c(2L, 3L)]),
      first_month_ltm = first_value_at(
        number_of_reviews_ltm,
        .quarter_month
      ),
      first_month_number_of_reviews = first_value_at(
        number_of_reviews,
        .quarter_month
      ),
      quarter_months_observed = n_distinct(.quarter_month),
      avg_price_months_used = paste(
        sort(unique(.quarter_month[.quarter_month %in% c(2L, 3L)])),
        collapse = ","
      )
    ) %>%
    # First month carries treatment, running variable, and characteristics.
    slice(1L) %>%
    ungroup() %>%
    # The same host-quarter status must also agree across all listings
    # belonging to that host.
    group_by(host_id, .quarter_index) %>%
    filter(
      all(!is.na(host_is_superhost)),
      all(host_is_superhost != ""),
      n_distinct(host_is_superhost) == 1L
    ) %>%
    ungroup() %>%
    # Keep the eventual RD sample balanced as well: if a required regression
    # variable is missing in one quarter, remove that listing through the
    # all-required-quarters intersection below rather than dropping only one
    # listing-quarter inside rdrobust().
    filter(
      is.finite(running_scr),
      !is.na(host_is_superhost2),
      !is.na(id)
    )

  # Q2 2023 supplies only the baseline average for the Q3 2023 outcome.
  # running_scr and treatment variables are intentionally not required here
  # because they are defined only from Q3 2023 onward.
  history_prices <- monthly %>%
    filter(
      .quarter_index == history_index,
      .quarter_month %in% c(2L, 3L),
      is.finite(price),
      price > 0
    ) %>%
    group_by(id) %>%
    filter(
      n_distinct(.quarter_month) == 2L,
      all(c(2L, 3L) %in% .quarter_month)
    ) %>%
    summarise(
      q223_avg_price = mean(price),
      .groups = "drop"
    )

  balanced_ids <- quarterly_candidates %>%
    group_by(id) %>%
    summarise(
      n_target_quarters = n_distinct(.quarter_index),
      has_all_target_quarters =
        all(target_indices %in% .quarter_index),
      .groups = "drop"
    ) %>%
    filter(
      n_target_quarters == length(target_indices),
      has_all_target_quarters,
      id %in% history_prices$id
    ) %>%
    pull(id)

  if (length(balanced_ids) == 0L) {
    stop(
      paste0(
        "No listing has all six target quarters plus valid Q223 month-2/3 ",
        "price history, quarter-consistent host status, and complete target-",
        "quarter RD variables."
      )
    )
  }

  quarterly <- quarterly_candidates %>%
    filter(id %in% balanced_ids) %>%
    left_join(history_prices, by = "id") %>%
    arrange(id, .quarter_index) %>%
    group_by(id) %>%
    mutate(
      previous_quarter_index = if_else(
        row_number() == 1L,
        history_index,
        lag(.quarter_index)
      ),
      previous_quarter = if_else(
        row_number() == 1L,
        history_quarter,
        lag(quarter)
      ),
      ex_avg = if_else(
        row_number() == 1L,
        q223_avg_price,
        lag(avg_price)
      ),
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
    mutate(
      quarter = factor(quarter, levels = target_quarters)
    ) %>%
    arrange(id, quarter)

  id_balance_check <- quarterly %>%
    group_by(id) %>%
    summarise(
      n_target_quarters = n_distinct(quarter),
      has_all_target_quarters = all(target_quarters %in% as.character(quarter)),
      .groups = "drop"
    )

  stopifnot(
    !anyDuplicated(quarterly[c("id", "quarter")]),
    all(quarterly$quarter_months_observed == 3L),
    all(quarterly$avg_price_months_used == "2,3"),
    all(is.finite(quarterly$avg_price)),
    all(is.finite(quarterly$price_diff)),
    all(id_balance_check$n_target_quarters == length(target_quarters)),
    all(id_balance_check$has_all_target_quarters)
  )

  cat(sprintf(
    paste0(
      "[build_balanced_quarter_panel] balanced listings: %d; ",
      "target rows: %d\n"
    ),
    length(balanced_ids), nrow(quarterly)
  ))
  print(table(quarterly$quarter))

  quarterly
}

# Intended order from this point:
#   balanced six-quarter base -> active/review restriction
#   -> quarter-specific trimming -> pool remaining rows -> RD.
# There is deliberately no second six-quarter intersection after either the
# active restriction or trimming.
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

      out <- .x %>%
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

      cat(sprintf(
        paste0(
          "[trim_quarter_sample] quarter=%s before=%d after=%d ",
          "(avg_price_pct=%.3f, price_diff_pct=%.3f)\n"
        ),
        as.character(.y$quarter[[1L]]),
        nrow(.x),
        nrow(out),
        avg_price_pct,
        price_diff_pct
      ))

      out
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

  cutoff_rows <- after_trim %>%
    group_by(quarter) %>%
    summarise(
      n_after_trim = n(),
      avg_price_trim_low = first(avg_price_trim_low),
      avg_price_trim_high = first(avg_price_trim_high),
      price_diff_trim_low = first(price_diff_trim_low),
      price_diff_trim_high = first(price_diff_trim_high),
      .groups = "drop"
    )

  full_join(before_counts, cutoff_rows, by = "quarter") %>%
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

  data <- data %>%
    filter(
      is.finite(price_diff),
      is.finite(running_scr),
      !is.na(host_is_superhost2),
      !is.na(id),
      !is.na(quarter)
    )
  if (nrow(data) == 0L) {
    stop("No complete RD observations remain in this panel.")
  }

  # Quarter fixed effects enter rdrobust through covs. Drop one quarter dummy
  # as the reference category.
  quarter_dummies <- as.data.frame(
    model.matrix(~ quarter - 1, data = data)
  )
  if (ncol(quarter_dummies) < 2L) {
    stop("At least two quarters are required for quarter fixed effects.")
  }
  quarter_covs <- as.matrix(
    quarter_dummies[, -1L, drop = FALSE]
  )

  rd_args <- list(
    y = data$price_diff,
    x = data$running_scr - 4.75,
    fuzzy = data$host_is_superhost2,
    covs = quarter_covs,
    cluster = data$id,
    kernel = "tri",
    bwselect = "msetwo",
    p = 1,
    masspoints = "off",
    bwrestrict = TRUE
  )
  if ("all" %in% names(formals(rdrobust))) rd_args$all <- TRUE

  do.call(rdrobust, rd_args)
}

run_panel <- function(panel_name, sample_data) {
  data <- sample_data
  if (panel_name == "A") data <- data %>% filter(ex_super == "t")
  if (panel_name == "B") data <- data %>% filter(ex_super == "f")

  data$quarter <- droplevels(factor(
    data$quarter,
    levels = target_quarters
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
    h_left = fit$bws[1L, 1L],
    h_right = fit$bws[1L, 2L],
    obs_h = sum(fit$N_h),
    error = NA_character_
  )
}

escape_latex <- function(x) {
  x <- gsub("\\\\", "\\\\textbackslash{}", x)
  gsub("([%$#_{}&])", "\\\\\\1", x)
}

format_number <- function(x, digits = 3L) {
  out <- formatC(x, digits = digits, format = "f")
  out[is.na(x)] <- ""
  out
}

df_to_latex <- function(df, digits = 3L, caption, label) {
  is_numeric <- vapply(df, is.numeric, logical(1L))
  formatted <- Map(
    function(x, numeric_column) {
      if (numeric_column) {
        format_number(x, digits)
      } else {
        out <- escape_latex(as.character(x))
        out[is.na(x)] <- ""
        out
      }
    },
    df,
    is_numeric
  )

  rows <- do.call(paste, c(formatted, sep = " & "))
  rows <- paste0(rows, " \\\\")
  header <- paste0(
    paste(escape_latex(names(df)), collapse = " & "),
    " \\\\"
  )

  paste(
    c(
      "\\begin{table}[htbp]",
      "\\centering",
      paste0("\\caption{", caption, "}"),
      paste0("\\label{", label, "}"),
      paste0(
        "\\begin{tabular}{",
        paste(ifelse(is_numeric, "r", "l"), collapse = ""),
        "}"
      ),
      "\\hline",
      header,
      "\\hline",
      rows,
      "\\hline",
      "\\end{tabular}",
      "\\end{table}"
    ),
    collapse = "\n"
  )
}

format_estimate_cell <- function(coef, p, digits = 3L) {
  if (is.na(coef)) return("")
  stars <- if (is.na(p)) {
    ""
  } else if (p < 0.01) {
    "***"
  } else if (p < 0.05) {
    "**"
  } else if (p < 0.1) {
    "*"
  } else {
    ""
  }
  sprintf("$%s%s$", formatC(coef, digits = digits, format = "f"), stars)
}

review_threshold_panel_table <- function(
    results,
    panel_letter,
    review_seq = seq(0L, 50L, by = 5L),
    digits = 3L
) {
  combos <- list(
    c(avg = 0, diff = 0),
    c(avg = 0.01, diff = 0),
    c(avg = 0, diff = 0.01),
    c(avg = 0.01, diff = 0.01)
  )
  sub <- results %>%
    filter(panel == panel_letter, review_min %in% review_seq)

  body <- character(0L)
  for (review_min in review_seq) {
    estimate_cells <- se_cells <- n_cells <- character(4L)

    for (i in seq_along(combos)) {
      combo <- combos[[i]]
      row <- sub %>%
        filter(
          review_min == .env$review_min,
          abs(avg_price_trim - combo[["avg"]]) < 1e-9,
          abs(price_diff_trim - combo[["diff"]]) < 1e-9
        )

      if (nrow(row) == 0L) {
        estimate_cells[[i]] <- ""
        se_cells[[i]] <- ""
        n_cells[[i]] <- ""
      } else {
        row <- row[1L, ]
        estimate_cells[[i]] <- format_estimate_cell(
          row$coef, row$p, digits
        )
        se_cells[[i]] <- if (is.na(row$se)) {
          ""
        } else {
          sprintf(
            "$(%s)$",
            formatC(row$se, digits = digits, format = "f")
          )
        }
        n_cells[[i]] <- format(
          row$raw_n,
          big.mark = ",",
          scientific = FALSE,
          trim = TRUE
        )
      }
    }

    body <- c(
      body,
      sprintf(
        "$\\geq %d$ & Estimate & %s \\\\",
        review_min,
        paste(estimate_cells, collapse = " & ")
      ),
      sprintf(" & SE & %s \\\\", paste(se_cells, collapse = " & ")),
      sprintf(" & $N$ & %s \\\\", paste(n_cells, collapse = " & ")),
      "\\addlinespace"
    )
  }
  if (length(body) > 0L) body <- body[-length(body)]

  panel_note <- switch(
    panel_letter,
    A = "Panel A restricts to \\texttt{ex\\_super=``t''}.",
    B = "Panel B restricts to \\texttt{ex\\_super=``f''}.",
    C = "Panel C applies no \\texttt{ex\\_super} restriction."
  )

  paste(
    c(
      "\\begin{table}[!htbp]",
      "\\centering",
      sprintf(
        "\\caption{Balanced-panel RD estimates: Panel %s}",
        panel_letter
      ),
      sprintf(
        "\\label{tab:balanced_review_threshold_0to50_panel_%s}",
        tolower(panel_letter)
      ),
      "\\small",
      "\\begin{tabular}{llcccc}",
      "\\toprule",
      paste0(
        "Reviews & Statistic & $(0\\%,0\\%)$ & $(1\\%,0\\%)$ & ",
        "$(0\\%,1\\%)$ & $(1\\%,1\\%)$ \\\\"
      ),
      "\\midrule",
      body,
      "\\bottomrule",
      "\\end{tabular}",
      "\\begin{minipage}{0.96\\textwidth}",
      "\\footnotesize",
      paste0(
        "\\textit{Notes:} The initial data contain the same listings in all ",
        "six target quarters. Active/review restrictions and quarter-specific ",
        "trimming are imposed afterward. ",
        panel_note,
        " RD estimates include quarter fixed effects through \\texttt{covs} ",
        "and cluster standard errors by listing ID."
      ),
      "\\end{minipage}",
      "\\end{table}",
      "\\clearpage"
    ),
    collapse = "\n"
  )
}

review_threshold_tables_tex <- function(
    results,
    review_seq = seq(0L, 50L, by = 5L),
    digits = 3L,
    panels = c("A", "B", "C")
) {
  tables <- vapply(
    panels,
    function(panel) {
      review_threshold_panel_table(
        results,
        panel,
        review_seq,
        digits
      )
    },
    character(1L)
  )
  paste(tables, collapse = "\n")
}

run_analysis <- function(
    results_file =
      "results/balanced_quarterly_review_threshold_panels.tex",
    data_file =
      "results/balanced_quarterly_active_review_trim_data.rds",
    audit_file =
      "results/balanced_quarterly_active_review_trim_audit.tex",
    review_seq = seq(0L, 50L, by = 5L)
) {
  quarterly_panel <- build_quarter_panel(Entire)

  review_thresholds <- seq(0L, 50L, by = 5L)
  avg_price_trims <- c(0, 0.01)
  price_diff_trims <- c(0, 0.01)
  panels <- c("A", "B", "C")
  results_list <- list()
  audit_list <- list()

  for (review_min in review_thresholds) {
    # Active/review restrictions occur only after the balanced base panel has
    # been fixed.
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
      match(quarter, target_quarters)
    )

  for (path in c(results_file, data_file, audit_file)) {
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  }

  writeLines(
    review_threshold_tables_tex(results, review_seq = review_seq),
    results_file
  )
  writeLines(
    df_to_latex(
      trim_audit,
      caption = "Balanced-panel quarter-level trim audit",
      label = "tab:balanced_quarterly_trim_audit"
    ),
    audit_file
  )
  saveRDS(
    list(
      quarterly_panel = quarterly_panel,
      results = results,
      trim_audit = trim_audit
    ),
    data_file
  )

  invisible(list(
    quarterly_panel = quarterly_panel,
    results = results,
    trim_audit = trim_audit
  ))
}

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  results_file <- if (length(args) >= 1L) args[[1L]] else
    "results/balanced_quarterly_review_threshold_panels.tex"
  data_file <- if (length(args) >= 2L) args[[2L]] else
    "results/balanced_quarterly_active_review_trim_data.rds"
  audit_file <- if (length(args) >= 3L) args[[3L]] else
    "results/balanced_quarterly_active_review_trim_audit.tex"

  run_analysis(
    results_file = results_file,
    data_file = data_file,
    audit_file = audit_file,
    review_seq = seq(0L, 50L, by = 5L)
  )
}
