base_dir <- "C:/Users/admin/Documents/Codex/2026-07-09/s"
project_root <- "C:/Users/admin/Documents/Codex/2026-07-07/rlt/work/Airbnb_Prominence"
lib_300 <- file.path(base_dir, "work", "rdrobust_versions", "rdrobust_3_0_0")
deps_lib <- "C:/Users/admin/Documents/Codex/2026-07-07/rlt/work/Rlibs"
out_dir <- file.path(base_dir, "outputs")

.libPaths(c(lib_300, deps_lib, .libPaths()))
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(rdrobust))

load(file.path(project_root, "Quarterly_dataset1.RData"))

if (as.character(packageVersion("rdrobust")) != "3.0.0") {
  stop("Expected rdrobust 3.0.0, got ", packageVersion("rdrobust"))
}

quarter_list <- list(Q323 = Q323, Q423 = Q423, Q124 = Q124, Q224 = Q224, Q324 = Q324, Q424 = Q424)

z <- bind_rows(lapply(names(quarter_list), function(qname) {
  quarter_list[[qname]] %>% mutate(quarter = qname)
}))

z <- z %>%
  mutate(
    date3 = factor(Date),
    date3_ym = format(as.Date(date3), "%Y-%m"),
    scraped_ltm_count = ltm_scr,
    scraped_ltm_rating_sum = case_when(
      quarter == "Q323" ~ total_running_23jul_sum,
      quarter == "Q423" ~ total_running_23oct_sum,
      quarter == "Q124" ~ total_running_24jan_sum,
      quarter == "Q224" ~ total_running_24apr_sum,
      quarter == "Q324" ~ total_running_24jul_sum,
      quarter == "Q424" ~ total_running_24oct_sum,
      TRUE ~ NA_real_
    )
  )

full_condition <- z$ex_q1 == 1 | z$ex_q2 == 1 | z$ex_q3 == 1 | z$ex_q4 == 1
full_condition[is.na(full_condition)] <- FALSE

host_increment_match <- z %>%
  distinct(
    quarter,
    host_id,
    id,
    first_month_number_of_reviews,
    ex_quarter_number_of_reviews,
    first_month_ltm,
    scraped_ltm_count,
    scraped_ltm_rating_sum
  ) %>%
  mutate(
    listing_first_month_increment = first_month_number_of_reviews - ex_quarter_number_of_reviews
  ) %>%
  group_by(quarter, host_id) %>%
  summarise(
    host_n_listings_q = n_distinct(id),
    host_first_month_increment = ifelse(
      all(is.na(listing_first_month_increment)),
      NA_real_,
      sum(listing_first_month_increment, na.rm = TRUE)
    ),
    host_increment_missing_n = sum(is.na(listing_first_month_increment)),
    host_increment_negative_n = sum(listing_first_month_increment < 0, na.rm = TRUE),
    host_inside_first_month_ltm = ifelse(
      all(is.na(first_month_ltm)),
      NA_real_,
      sum(first_month_ltm, na.rm = TRUE)
    ),
    host_first_month_ltm_missing_n = sum(is.na(first_month_ltm)),
    host_scraped_ltm_count_unique_n = n_distinct(scraped_ltm_count[!is.na(scraped_ltm_count)]),
    host_scraped_ltm_count = ifelse(
      host_scraped_ltm_count_unique_n == 1,
      first(scraped_ltm_count[!is.na(scraped_ltm_count)]),
      NA_real_
    ),
    host_scraped_ltm_sum_unique_n = n_distinct(scraped_ltm_rating_sum[!is.na(scraped_ltm_rating_sum)]),
    host_scraped_ltm_rating_sum = ifelse(
      host_scraped_ltm_sum_unique_n == 1,
      first(scraped_ltm_rating_sum[!is.na(scraped_ltm_rating_sum)]),
      NA_real_
    ),
    .groups = "drop"
  ) %>%
  mutate(
    host_first_month_increment_match = !is.na(host_scraped_ltm_count) &
      !is.na(host_first_month_increment) &
      host_increment_missing_n == 0 &
      host_scraped_ltm_count_unique_n == 1 &
      host_scraped_ltm_count == host_first_month_increment,
    host_first_month_increment_match_nonnegative = host_first_month_increment_match &
      host_increment_negative_n == 0,
    host_first_month_ltm_match = !is.na(host_scraped_ltm_count) &
      !is.na(host_inside_first_month_ltm) &
      host_first_month_ltm_missing_n == 0 &
      host_scraped_ltm_count_unique_n == 1 &
      host_scraped_ltm_count == host_inside_first_month_ltm,
    host_first_month_increment_match_rating_sum = !is.na(host_scraped_ltm_rating_sum) &
      !is.na(host_first_month_increment) &
      host_increment_missing_n == 0 &
      host_scraped_ltm_sum_unique_n == 1 &
      abs(host_scraped_ltm_rating_sum - host_first_month_increment) < 1e-9
  )

z <- z %>% left_join(host_increment_match, by = c("quarter", "host_id"))

samples <- list(
  panel_b_ex2 = list(
    label = "ex_super=f & ex_super2=t",
    keep = z$ex_super == "f" & z$ex_super2 == "t" & full_condition
  ),
  panel_b_no_ex2 = list(
    label = "ex_super=f",
    keep = z$ex_super == "f" & full_condition
  )
)

filters <- list(
  baseline = list(
    label = "baseline",
    keep = rep(TRUE, nrow(z))
  ),
  host_first_month_increment_match = list(
    label = "host scraped LTM count = host sum first-month review increment",
    keep = z$host_first_month_increment_match
  ),
  host_first_month_increment_match_nonnegative = list(
    label = "same, requiring no negative listing increments",
    keep = z$host_first_month_increment_match_nonnegative
  ),
  host_first_month_ltm_match = list(
    label = "host scraped LTM count = host sum Inside first_month_ltm",
    keep = z$host_first_month_ltm_match
  ),
  host_first_month_increment_match_rating_sum = list(
    label = "diagnostic: rating-sum variable equals increment",
    keep = z$host_first_month_increment_match_rating_sum
  )
)

diagnostics <- bind_rows(lapply(names(samples), function(sample_id) {
  sample_keep <- samples[[sample_id]]$keep
  sample_keep[is.na(sample_keep)] <- FALSE
  bind_rows(lapply(names(filters), function(filter_id) {
    filter_keep <- filters[[filter_id]]$keep
    filter_keep[is.na(filter_keep)] <- FALSE
    data <- z[sample_keep & filter_keep, , drop = FALSE]
    data.frame(
      sample_id = sample_id,
      sample = samples[[sample_id]]$label,
      filter_id = filter_id,
      filter = filters[[filter_id]]$label,
      raw_n = sum(sample_keep),
      kept_n = nrow(data),
      unique_hosts = n_distinct(data$host_id),
      mean_host_increment = mean(data$host_first_month_increment, na.rm = TRUE),
      mean_host_first_month_ltm = mean(data$host_inside_first_month_ltm, na.rm = TRUE),
      mean_scraped_ltm_count = mean(data$host_scraped_ltm_count, na.rm = TRUE),
      negative_increment_rows = sum(data$host_increment_negative_n > 0, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  }))
}))

write.csv(host_increment_match, file.path(out_dir, "first_month_review_increment_host_panel.csv"), row.names = FALSE)
write.csv(diagnostics, file.path(out_dir, "first_month_review_increment_match_counts.csv"), row.names = FALSE)

rd_call <- function(data, model = c("price", "first_stage"), h = NULL, use_time_fe = TRUE) {
  model <- match.arg(model)
  x <- data$running_scr - 4.75
  if (model == "price") {
    y <- log(data$avg_price) - log(data$ex_avg)
    fuzzy <- data$host_is_superhost2
    usable <- is.finite(y) & is.finite(x) & !is.na(fuzzy) & !is.na(data$id)
  } else {
    y <- as.numeric(data$host_is_superhost2)
    fuzzy <- NULL
    usable <- is.finite(y) & is.finite(x) & !is.na(data$id)
  }
  data <- data[usable, , drop = FALSE]
  x <- x[usable]
  y <- y[usable]
  if (!is.null(fuzzy)) fuzzy <- fuzzy[usable]
  if (nrow(data) < 30) {
    stop("Too few usable observations for rdrobust: ", nrow(data))
  }

  args <- list(
    x = x,
    y = y,
    cluster = data$id,
    kernel = "tri",
    bwselect = "msetwo",
    p = 1,
    masspoints = "off",
    bwrestrict = TRUE,
    all = TRUE
  )
  if (model == "price") args$fuzzy <- fuzzy
  if (!is.null(h)) args$h <- h
  if (use_time_fe) {
    date_factor <- droplevels(factor(data$date3_ym))
    dummy_vars <- as.data.frame(model.matrix(~ date_factor - 1))
    if (ncol(dummy_vars) > 1) {
      dummy_vars <- dummy_vars[, -1, drop = FALSE]
    } else {
      dummy_vars <- dummy_vars[, FALSE, drop = FALSE]
    }
    if (ncol(dummy_vars) > 0) {
      keep_dummy <- vapply(dummy_vars, function(col) length(unique(col[!is.na(col)])) > 1, logical(1))
      dummy_vars <- dummy_vars[, keep_dummy, drop = FALSE]
    }
    if (ncol(dummy_vars) > 0) args$covs <- cbind(dummy_vars)
  }
  do.call(rdrobust, args)
}

extract <- function(est, prefix) {
  out <- data.frame(
    conv = as.numeric(est[["Estimate"]][1]),
    se_conv = as.numeric(est[["se"]][1]),
    pv_conv = as.numeric(est[["pv"]][1]),
    bc = as.numeric(est[["Estimate"]][2]),
    se_robust = as.numeric(est[["se"]][3]),
    pv_robust = as.numeric(est[["pv"]][3]),
    h_left = as.numeric(est[["bws"]][1, 1]),
    h_right = as.numeric(est[["bws"]][1, 2]),
    obs_h = sum(est[["N_h"]]),
    stringsAsFactors = FALSE
  )
  names(out) <- paste(prefix, names(out), sep = "_")
  out
}

empty_extract <- function(prefix) {
  out <- data.frame(
    conv = NA_real_,
    se_conv = NA_real_,
    pv_conv = NA_real_,
    bc = NA_real_,
    se_robust = NA_real_,
    pv_robust = NA_real_,
    h_left = NA_real_,
    h_right = NA_real_,
    obs_h = NA_integer_,
    stringsAsFactors = FALSE
  )
  names(out) <- paste(prefix, names(out), sep = "_")
  out
}

fit_or_error <- function(expr) {
  tryCatch(expr, error = function(e) e)
}

spec_names <- c("msetwo_fe", "twomse_fe", "h020_010_fe", "h030_015_fe", "h040_020_fe", "msetwo_no_fe")
rows <- list()

for (sample_id in names(samples)) {
  sample_keep <- samples[[sample_id]]$keep
  sample_keep[is.na(sample_keep)] <- FALSE

  for (filter_id in names(filters)) {
    filter_keep <- filters[[filter_id]]$keep
    filter_keep[is.na(filter_keep)] <- FALSE
    data <- z[sample_keep & filter_keep, , drop = FALSE]

    price1 <- fit_or_error(rd_call(data, model = "price"))
    fs1 <- fit_or_error(rd_call(data, model = "first_stage"))

    ests <- list(
      msetwo_fe = list(price = price1, first_stage = fs1),
      twomse_fe = list(
        price = if (inherits(price1, "error")) simpleError("price msetwo_fe failed; twomse skipped") else
          fit_or_error(rd_call(data, model = "price", h = c(2 * price1[["bws"]][1, 1], 2 * price1[["bws"]][1, 2]))),
        first_stage = if (inherits(fs1, "error")) simpleError("first_stage msetwo_fe failed; twomse skipped") else
          fit_or_error(rd_call(data, model = "first_stage", h = c(2 * fs1[["bws"]][1, 1], 2 * fs1[["bws"]][1, 2])))
      ),
      h020_010_fe = list(
        price = fit_or_error(rd_call(data, model = "price", h = c(0.2, 0.1))),
        first_stage = fit_or_error(rd_call(data, model = "first_stage", h = c(0.2, 0.1)))
      ),
      h030_015_fe = list(
        price = fit_or_error(rd_call(data, model = "price", h = c(0.3, 0.15))),
        first_stage = fit_or_error(rd_call(data, model = "first_stage", h = c(0.3, 0.15)))
      ),
      h040_020_fe = list(
        price = fit_or_error(rd_call(data, model = "price", h = c(0.4, 0.2))),
        first_stage = fit_or_error(rd_call(data, model = "first_stage", h = c(0.4, 0.2)))
      ),
      msetwo_no_fe = list(
        price = fit_or_error(rd_call(data, model = "price", use_time_fe = FALSE)),
        first_stage = fit_or_error(rd_call(data, model = "first_stage", use_time_fe = FALSE))
      )
    )

    for (spec_name in names(ests)) {
      price_est <- ests[[spec_name]]$price
      fs_est <- ests[[spec_name]]$first_stage
      error_msg <- c()
      if (inherits(price_est, "error")) error_msg <- c(error_msg, paste0("price: ", price_est$message))
      if (inherits(fs_est, "error")) error_msg <- c(error_msg, paste0("first_stage: ", fs_est$message))

      rows[[paste(sample_id, filter_id, spec_name, sep = "__")]] <- data.frame(
        sample_id = sample_id,
        sample = samples[[sample_id]]$label,
        filter_id = filter_id,
        filter = filters[[filter_id]]$label,
        raw_n = nrow(data),
        spec = spec_name,
        if (inherits(price_est, "error")) empty_extract("price") else extract(price_est, "price"),
        if (inherits(fs_est, "error")) empty_extract("fs") else extract(fs_est, "fs"),
        error = paste(error_msg, collapse = " | "),
        stringsAsFactors = FALSE
      )
    }
  }
}

result <- bind_rows(rows)

write.csv(result, file.path(out_dir, "first_month_review_increment_match_rdrobust300.csv"), row.names = FALSE)

star <- function(p) {
  ifelse(is.na(p), "",
    ifelse(p < 0.01, "***",
      ifelse(p < 0.05, "**",
        ifelse(p < 0.1, "*", ""))))
}

strip0 <- function(x) ifelse(is.na(x), "--", sub("^0", "", sprintf("%.3f", x)))
fmt_coef <- function(x, p) paste0(strip0(x), star(p))
fmt_se <- function(x) ifelse(is.na(x), "--", paste0("(", strip0(x), ")"))

make_tex <- function(value_col, se_col, p_col, file_name) {
  tex_lines <- c(
    "\\begin{tabular}{llcccccc}",
    "\\hline",
    "Sample & Filter & (1) & (2) & (3) & (4) & (5) & (6) \\\\",
    "\\hline"
  )

  for (sample_id in names(samples)) {
    for (filter_id in names(filters)) {
      count_row <- diagnostics[diagnostics$sample_id == sample_id & diagnostics$filter_id == filter_id, ]
      sub <- result[result$sample_id == sample_id & result$filter_id == filter_id, ]
      sub <- sub[match(spec_names, sub$spec), ]
      coefs <- fmt_coef(sub[[value_col]], sub[[p_col]])
      ses <- fmt_se(sub[[se_col]])
      tex_lines <- c(
        tex_lines,
        paste(c(samples[[sample_id]]$label, paste0(filters[[filter_id]]$label, " (N=", count_row$kept_n, ")"), coefs), collapse = " & "),
        "\\\\",
        paste(c("", "", ses), collapse = " & "),
        "\\\\"
      )
    }
  }

  tex_lines <- c(tex_lines, "\\hline", "\\end{tabular}")
  writeLines(tex_lines, file.path(out_dir, file_name))
}

make_tex("price_conv", "price_se_conv", "price_pv_conv", "first_month_review_increment_match_price_rdrobust300.tex")
make_tex("fs_conv", "fs_se_conv", "fs_pv_conv", "first_month_review_increment_match_first_stage_rdrobust300.tex")

cat("rdrobust", as.character(packageVersion("rdrobust")), "\n")
print(diagnostics)
print(result %>% select(sample_id, filter_id, spec, raw_n, price_conv, price_se_conv, price_pv_conv, fs_conv, fs_se_conv, fs_pv_conv, error))
