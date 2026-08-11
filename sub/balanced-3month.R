options(stringsAsFactors = FALSE)

.libPaths(c(file.path(getwd(), ".Rlib"), .libPaths()))

suppressPackageStartupMessages({
  library(dplyr)
  library(rdrobust)
})
load("RData/Entire.RData")


# This script deliberately constructs a balanced six-quarter listing panel
# before any active-listing, review-threshold, or trimming restriction is
# imposed.
#
# BALANCED 3-MONTH VARIANT:
#   * the same listing must be observed in all three calendar months of every
#     target quarter (Q323, Q423, Q124, Q224, Q324, Q424);
#   * avg_price is the arithmetic mean of all three monthly prices;
#   * host-validity (host_is_superhost consistency) uses only months 2 and 3;
#   * month 1 still carries the RD, treatment, and activity variables.
#
# Order of operations:
#   1. Monthly observations -> complete listing-quarter candidates.
#   2. Intersect listing IDs across all six consecutive target quarters.
#   3. Create first-month activity variables and the three-month average price.
#   4. Link each listing to its immediately preceding quarter and calculate
#      the quarterly price difference.
#   5. Apply active/review restrictions.
#   6. Calculate both trim cutoffs separately within each quarter, using the
#      same pre-trim sample, and retain observations passing both cutoffs.
#   7. Pool the cleaned observations across all quarters and estimate the
#      main fuzzy RD specification (with quarter dummies as covariates) for
#      panels A, B, and C.
#
# Output: two .tex files (main results table, trim audit table) and one
# .rds file with the underlying data frames. No CSV is written anywhere.

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

build_quarter_panel <- function(monthly_data) {
  cat(sprintf(
    "[build_quarter_panel] input monthly rows: %d\n",
    nrow(monthly_data)
  ))

  # NOTE (fix #4): the old whole-history "n_distinct(host_id) == 1L across
  # every month a listing was ever observed" filter is gone. Owner identity
  # only needs to hold (a) within a single quarter, and (b) between the two
  # specific quarters being differenced for price_diff -- not for the
  # listing's entire lifetime. Both are enforced further down.
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
    ungroup()

  cat(sprintf(
    "[build_quarter_panel] monthly rows after dedup: %d\n",
    nrow(monthly)
  ))

  quarterly_candidates <- monthly %>%
    group_by(id, .quarter_index, quarter) %>%
    # All three calendar months must exist and contribute to avg_price.
    # host_id must be valid throughout the quarter. Per the requested
    # host-validity rule, host_is_superhost only has to be observed and equal
    # in months 2 and 3; month 1 may differ or be missing.
    filter(
      n_distinct(.quarter_month) == 3L,
      all(1:3 %in% .quarter_month),
      all(is.finite(price)),
      all(price > 0),
      all(!is.na(host_id)),
      n_distinct(host_id) == 1L,
      all(!is.na(host_is_superhost[.quarter_month %in% c(2L, 3L)])),
      all(host_is_superhost[.quarter_month %in% c(2L, 3L)] != ""),
      n_distinct(
        host_is_superhost[.quarter_month %in% c(2L, 3L)]
      ) == 1L
    ) %>%
    arrange(.quarter_month, Date, .by_group = TRUE) %>%
    mutate(
      avg_price = mean(price),
      avg_price_months_used = "1,2,3",
      host_validity = TRUE,
      quarter_superhost_status =
        host_is_superhost[.quarter_month == 2L][1L],
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
    ungroup()

  quarterly_candidates <- quarterly_candidates %>%
    group_by(host_id, .quarter_index) %>%
    filter(
      all(!is.na(quarter_superhost_status)),
      all(quarter_superhost_status != ""),
      n_distinct(quarter_superhost_status) == 1L
    ) %>%
    ungroup()

  balanced_ids <- quarterly_candidates %>%
    filter(quarter %in% target_quarters) %>%
    group_by(id) %>%
    summarise(
      n_target_quarters = n_distinct(quarter),
      has_all_target_quarters = all(target_quarters %in% quarter),
      .groups = "drop"
    ) %>%
    filter(
      n_target_quarters == length(target_quarters),
      has_all_target_quarters
    ) %>%
    pull(id)

  if (length(balanced_ids) == 0L) {
    stop(
      paste0(
        "No listing is observed in all three months of all six target ",
        "quarters after the price and host-validity checks."
      )
    )
  }

  cat(sprintf(
    paste0(
      "[build_quarter_panel] balanced listings after six-quarter ",
      "intersection: %d\n"
    ),
    length(balanced_ids)
  ))

  quarterly <- quarterly_candidates %>%
    filter(id %in% balanced_ids) %>%
    arrange(id, .quarter_index) %>%
    group_by(id) %>%
    mutate(
      previous_quarter_index = lag(.quarter_index),
      previous_quarter = lag(quarter),
      previous_host_id = lag(host_id),
      ex_avg = lag(avg_price),
      consecutive_previous_quarter =
        .quarter_index - previous_quarter_index == 1L,
      # fix #4: price_diff/raw_change now also require the SAME host_id in
      # the current quarter and its immediately preceding quarter -- i.e.
      # ownership only has to match across the one 6-month pair actually
      # being compared, not across the listing's whole history.
      same_host_as_previous_quarter =
        !is.na(previous_host_id) & host_id == previous_host_id,
      price_diff = if_else(
        consecutive_previous_quarter & same_host_as_previous_quarter,
        log(avg_price) - log(ex_avg),
        NA_real_
      ),
      raw_change = if_else(
        consecutive_previous_quarter & same_host_as_previous_quarter,
        (avg_price - ex_avg) / ex_avg,
        NA_real_
      )
    ) %>%
    ungroup() %>%
    filter(quarter %in% target_quarters)

  cat(sprintf(
    "[build_quarter_panel] rows after price-difference + quarter-window filter: %d\n",
    nrow(quarterly)
  ))

  id_balance_check <- quarterly %>%
    group_by(id) %>%
    summarise(
      n_target_quarters = n_distinct(quarter),
      has_all_target_quarters = all(target_quarters %in% quarter),
      .groups = "drop"
    )

  stopifnot(
    !anyDuplicated(quarterly[c("id", "quarter")]),
    all(quarterly$quarter_months_observed == 3L),
    all(quarterly$avg_price_months_used == "1,2,3"),
    all(quarterly$host_validity),
    all(id_balance_check$n_target_quarters == length(target_quarters)),
    all(id_balance_check$has_all_target_quarters)
  )

  cat("[build_quarter_panel] rows by quarter:\n")
  print(table(quarterly$quarter))

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
        "[trim_quarter_sample] quarter=%s before=%d after=%d (avg_price_pct=%.3f, price_diff_pct=%.3f)\n",
        .y$quarter[[1L]], nrow(.x), nrow(out), avg_price_pct, price_diff_pct
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

fit_main_rd <- function(data, cluster_type = "none") {
  if (nrow(data) == 0L) stop("No observations remain in this panel.")

  valid_cluster_types <- c("none", "id", "host_id", "quarter")
  if (!cluster_type %in% valid_cluster_types) {
    stop(sprintf(
      "Unknown cluster_type '%s'. Choose one of: %s",
      cluster_type,
      paste(valid_cluster_types, collapse = ", ")
    ))
  }

  time_dummies <- as.data.frame(
    model.matrix(~ quarter - 1, data = data)
  )

  rd_args <- list(
    y = data$price_diff,
    x = data$running_scr - 4.75,
    fuzzy = data$host_is_superhost2,
    kernel = "tri",
    bwselect = "msetwo",
    p = 1,
    masspoints = "off",
    bwrestrict = TRUE
  )

  # For the no-clustering specification, omit the cluster argument entirely.
  if (cluster_type == "id") {
    rd_args$cluster <- data$id
  } else if (cluster_type == "host_id") {
    rd_args$cluster <- data$host_id
  } else if (cluster_type == "quarter") {
    rd_args$cluster <- as.integer(factor(data$quarter))
  }

  if (ncol(time_dummies) > 1L) {
    rd_args$covs <- as.matrix(time_dummies[, -1L, drop = FALSE])
  }
  if ("all" %in% names(formals(rdrobust))) rd_args$all <- TRUE

  do.call(rdrobust, rd_args)
}

run_panel <- function(panel_name, sample_data, cluster_type) {
  data <- sample_data
  if (panel_name == "A") data <- data %>% filter(ex_super == "t")
  if (panel_name == "B") data <- data %>% filter(ex_super == "f")
  data$quarter <- droplevels(factor(
    data$quarter,
    levels = c("Q323", "Q423", "Q124", "Q224", "Q324", "Q424")
  ))

  cat(sprintf(
    "  [run_panel] cluster=%s panel=%s pooled n=%d (quarters present: %s)\n",
    cluster_type, panel_name, nrow(data), paste(levels(data$quarter), collapse = ",")
  ))

  fit <- tryCatch(
    fit_main_rd(data, cluster_type = cluster_type),
    error = function(e) e
  )
  if (inherits(fit, "error")) {
    cat(sprintf(
      "  [run_panel] cluster=%s panel=%s FAILED: %s\n",
      cluster_type, panel_name, conditionMessage(fit)
    ))
    return(data.frame(
      cluster_type = cluster_type,
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

  cat(sprintf(
    "  [run_panel] cluster=%s panel=%s coef=%.4f se=%.4f p=%.4f obs_h=%d\n",
    cluster_type, panel_name, fit$Estimate[[1L]], fit$se[[1L]],
    fit$pv[[1L]], sum(fit$N_h)
  ))

  data.frame(
    cluster_type = cluster_type,
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

# ---------------------------------------------------------------------------
# Minimal, dependency-free LaTeX writers (no xtable/stargazer required)
# ---------------------------------------------------------------------------
escape_latex <- function(x) {
  x <- gsub("\\\\", "\\\\textbackslash{}", x)
  x <- gsub("([%$#_{}&])", "\\\\\\1", x)
  x
}

# Generic table — used only for the trim audit, which is a diagnostic dump
# and doesn't need the panel/threshold layout below.
df_to_latex <- function(df, digits = 3, caption = NULL, label = NULL) {
  is_num <- vapply(df, is.numeric, logical(1))

  fmt_col <- function(col, is_num_col) {
    if (is_num_col) {
      out <- formatC(col, digits = digits, format = "f")
      out[is.na(col)] <- ""
    } else {
      out <- escape_latex(as.character(col))
      out[is.na(col)] <- ""
    }
    out
  }

  formatted <- Map(fmt_col, df, is_num)
  body_rows <- do.call(paste, c(formatted, sep = " & "))
  body_rows <- paste0(body_rows, " \\\\")

  col_align <- paste(ifelse(is_num, "r", "l"), collapse = "")
  header <- paste0(paste(escape_latex(names(df)), collapse = " & "), " \\\\")

  lines <- c(
    "\\begin{table}[htbp]",
    "\\centering",
    if (!is.null(caption)) paste0("\\caption{", caption, "}"),
    if (!is.null(label)) paste0("\\label{", label, "}"),
    paste0("\\begin{tabular}{", col_align, "}"),
    "\\hline",
    header,
    "\\hline",
    body_rows,
    "\\hline",
    "\\end{tabular}",
    "\\end{table}"
  )
  paste(lines, collapse = "\n")
}

# ---------------------------------------------------------------------------
# Review-threshold-by-panel results table (the main deliverable)
#
# One booktabs table per panel (A, B, C). Rows are review thresholds
# (>=0 ... >=50, step 5), each with an Estimate / SE / N sub-row. Columns
# are the four (avg_price_trim, price_diff_trim) combinations, in the order
# (0%,0%), (1%,0%), (0%,1%), (1%,1%).
# ---------------------------------------------------------------------------
format_estimate_cell <- function(coef, p, digits = 3) {
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
  estimate <- formatC(coef, digits = digits, format = "f")
  if (stars == "") {
    sprintf("$%s$", estimate)
  } else {
    sprintf("$%s^{%s}$", estimate, stars)
  }
}

format_se_cell <- function(se, digits = 3) {
  if (is.na(se)) return("")
  sprintf("$(%s)$", formatC(se, digits = digits, format = "f"))
}

format_n_cell <- function(n) {
  if (is.na(n)) return("")
  format(n, big.mark = ",", scientific = FALSE, trim = TRUE)
}

cluster_display_name <- function(cluster_type) {
  switch(
    cluster_type,
    "none" = "No clustering",
    "id" = "Listing ID",
    "host_id" = "Host ID",
    "quarter" = "Quarter (time)",
    stop(sprintf("Unknown cluster_type: %s", cluster_type))
  )
}

cluster_note <- function(cluster_type) {
  switch(
    cluster_type,
    "none" = "Standard errors are not clustered.",
    "id" = "Standard errors are clustered by listing ID.",
    "host_id" = "Standard errors are clustered by host ID.",
    "quarter" = "Standard errors are clustered by quarter.",
    stop(sprintf("Unknown cluster_type: %s", cluster_type))
  )
}

review_threshold_panel_table <- function(
    results,
    panel_letter,
    cluster_type,
    review_seq = seq(0L, 50L, by = 5L),
    digits = 3
) {
  combos <- list(
    c(avg = 0,    diff = 0),
    c(avg = 0.01, diff = 0),
    c(avg = 0,    diff = 0.01),
    c(avg = 0.01, diff = 0.01)
  )

  sub <- results %>%
    filter(
      panel == panel_letter,
      cluster_type == .env$cluster_type,
      review_min %in% review_seq
    )

  row_lines <- character(0)
  for (rm in review_seq) {
    est_cells <- character(4)
    se_cells  <- character(4)
    n_cells   <- character(4)

    for (i in seq_along(combos)) {
      cb <- combos[[i]]
      row <- sub %>%
        filter(
          review_min == rm,
          abs(avg_price_trim - cb[["avg"]]) < 1e-9,
          abs(price_diff_trim - cb[["diff"]]) < 1e-9
        )

      if (nrow(row) == 0L) {
        est_cells[i] <- ""
        se_cells[i]  <- ""
        n_cells[i]   <- ""
      } else {
        row <- row[1L, ]
        est_cells[i] <- format_estimate_cell(row$coef, row$p, digits)
        se_cells[i]  <- format_se_cell(row$se, digits)
        n_cells[i]   <- format_n_cell(row$raw_n)
      }
    }

    latex_row_end <- paste0(" ", "\\", "\\")

    row_lines <- c(
      row_lines,
      paste0(
        sprintf("$\\geq %d$ & Estimate & %s", rm, paste(est_cells, collapse = " & ")),
        latex_row_end
      ),
      paste0(
        sprintf(" & SE & %s", paste(se_cells, collapse = " & ")),
        latex_row_end
      ),
      paste0(
        sprintf(" & $N$ & %s", paste(n_cells, collapse = " & ")),
        latex_row_end
      ),
      "\\addlinespace"
    )
  }

  if (length(row_lines) > 0L) {
    row_lines <- row_lines[-length(row_lines)]
  }

  ex_super_note <- switch(
    panel_letter,
    "A" = "Panel A restricts to \\texttt{ex\\_super=``t''}.",
    "B" = "Panel B restricts to \\texttt{ex\\_super=``f''}.",
    "C" = "Panel C applies no \\texttt{ex\\_super} restriction."
  )

  notes <- paste0(
    "The base panel keeps only listings observed in all three months of each ",
    "of the six target quarters. Quarterly average price uses all three ",
    "months, while host-status validity compares months 2 and 3. ",
    "Before trimming, the sample is restricted to \\texttt{first\\_month\\_ltm} ",
    "$\\geq 1$ and finite price changes. Quarter-specific trimming is then ",
    "applied, followed by the displayed ",
    "\\texttt{first\\_month\\_number\\_of\\_reviews} threshold. ",
    "Column headings report (price trim, price-change trim). ",
    ex_super_note, " ",
    "Estimates use the main fuzzy RD specification with time fixed effects, a ",
    "triangular kernel, and MSE-optimal side-specific bandwidths. ",
    cluster_note(cluster_type), " ",
    "$N$ is the full pre-bandwidth analysis sample size. ",
    "$^{***}p<0.01$, $^{**}p<0.05$, $^{*}p<0.10$."
  )

  cluster_label <- gsub("_", "-", cluster_type)

  paste(
    c(
      "\\begin{table}[!htbp]",
      "\\centering",
      sprintf(
        "\\caption{RD estimates by initial cumulative review threshold: Panel %s (%s)}",
        panel_letter,
        cluster_display_name(cluster_type)
      ),
      sprintf(
        "\\label{tab:review_threshold_0to50_%s_panel_%s}",
        cluster_label,
        tolower(panel_letter)
      ),
      "\\small",
      "\\begin{tabular}{llcccc}",
      "\\toprule",
      paste0(
        "Reviews & Statistic & $(0\\%,0\\%)$ & $(1\\%,0\\%)$ & ",
        "$(0\\%,1\\%)$ & $(1\\%,1\\%)$",
        paste0(" ", "\\", "\\")
      ),
      "\\midrule",
      row_lines,
      "\\bottomrule",
      "\\end{tabular}",
      "\\begin{minipage}{0.96\\textwidth}",
      "\\footnotesize",
      paste0("\\textit{Notes:} ", notes),
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
    digits = 3,
    panels = c("A", "B", "C"),
    cluster_types = c("none", "id", "host_id", "quarter")
) {
  sections <- vapply(
    cluster_types,
    function(cl) {
      tables <- vapply(
        panels,
        function(p) review_threshold_panel_table(
          results = results,
          panel_letter = p,
          cluster_type = cl,
          review_seq = review_seq,
          digits = digits
        ),
        character(1L)
      )

      paste(
        c(
          sprintf("\\subsection{Clustering: %s}", cluster_display_name(cl)),
          tables
        ),
        collapse = "\n"
      )
    },
    character(1L)
  )

  paste(sections, collapse = "\n\n")
}

run_analysis <- function(
    results_file =
      "results/balanced_3month_review_threshold_panels.tex",
    data_file =
      "results/balanced_3month_active_review_trim_data.rds",
    audit_file =
      "results/balanced_3month_active_review_trim_audit.tex",
    review_seq = seq(0L, 50L, by = 5L)
) {

  cat("=== [run_analysis] building balanced quarterly panel ===\n")
  quarterly_panel <- build_quarter_panel(Entire)

  review_thresholds <- seq(0L, 50L, by = 5L)
  avg_price_trims <- c(0, 0.01)
  price_diff_trims <- c(0, 0.01)
  panels <- c("A", "B", "C")
  cluster_types <- c("none", "id", "host_id", "quarter")

  results_list <- list()
  audit_list <- list()

  # The activity definition is imposed before trimming. The cumulative
  # review threshold is deliberately imposed only after the quarter-specific
  # trim bounds have been calculated and applied.
  active_base <- quarterly_panel %>%
    filter(
      !is.na(first_month_ltm),
      first_month_ltm >= 1,
      is.finite(price_diff)
    )

  for (avg_price_trim in avg_price_trims) {
    for (price_diff_trim in price_diff_trims) {
      cat(sprintf(
        "[run_analysis] trimming active base: avg_price_pct=%.3f price_diff_pct=%.3f\n",
        avg_price_trim, price_diff_trim
      ))

      trimmed_data <- trim_quarter_sample(
        active_base,
        avg_price_pct = avg_price_trim,
        price_diff_pct = price_diff_trim
      )

      for (review_min in review_thresholds) {
        sample_data <- trimmed_data %>%
          filter(
            !is.na(first_month_number_of_reviews),
            first_month_number_of_reviews >= review_min
          )

        cat(sprintf(
          paste0(
            "[run_analysis] after trim + review_min=%d: ",
            "pooled n=%d\n"
          ),
          review_min,
          nrow(sample_data)
        ))

        review_counts <- sample_data %>%
          count(quarter, name = "n_after_review_cutoff")

        audit_list[[length(audit_list) + 1L]] <-
          make_trim_audit(
            active_base,
            trimmed_data,
            review_min,
            avg_price_trim,
            price_diff_trim
          ) %>%
          left_join(review_counts, by = "quarter") %>%
          mutate(
            n_after_review_cutoff =
              coalesce(n_after_review_cutoff, 0L)
          )

        for (cluster_type in cluster_types) {
          cat(sprintf(
            "[run_analysis] clustering=%s review_min=%d\n",
            cluster_type,
            review_min
          ))

          panel_results <- bind_rows(lapply(
            panels,
            run_panel,
            sample_data = sample_data,
            cluster_type = cluster_type
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
  }

  results <- bind_rows(results_list) %>%
    select(
      cluster_type,
      review_min,
      avg_price_trim,
      price_diff_trim,
      panel,
      everything()
    ) %>%
    arrange(
      match(cluster_type, cluster_types),
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

  cat(sprintf(
    "[run_analysis] building review-threshold panel tables for: %s\n",
    paste(review_seq, collapse = ", ")
  ))
  writeLines(
    review_threshold_tables_tex(
      results,
      review_seq = review_seq,
      cluster_types = cluster_types
    ),
    results_file
  )
  writeLines(
    df_to_latex(
      trim_audit,
      caption = "Balanced three-month quarter-level trim audit",
      label = "tab:balanced_3month_trim_audit"
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

  cat("\n=== [run_analysis] done ===\n")
  cat(sprintf("Quarter panel rows: %d\n", nrow(quarterly_panel)))
  cat(sprintf("Regression result rows: %d\n", nrow(results)))
  cat(sprintf("Results (tex): %s\n", normalizePath(results_file, winslash = "/")))
  cat(sprintf("Data (rds): %s\n", normalizePath(data_file, winslash = "/")))
  cat(sprintf("Trim audit (tex): %s\n", normalizePath(audit_file, winslash = "/")))

  invisible(list(
    quarterly_panel = quarterly_panel,
    results = results,
    trim_audit = trim_audit
  ))
}

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  results_file <- if (length(args) >= 1L) args[[1L]] else
    "results/balanced_3month_review_threshold_panels.tex"
  data_file <- if (length(args) >= 2L) args[[2L]] else
    "results/balanced_3month_active_review_trim_data.rds"
  audit_file <- if (length(args) >= 3L) args[[3L]] else
    "results/balanced_3month_active_review_trim_audit.tex"

  run_analysis(results_file, data_file, audit_file)
}
