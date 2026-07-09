local_lib <- normalizePath(file.path("..", "Rlibs"), winslash = "/", mustWork = FALSE)
if (dir.exists(local_lib)) .libPaths(c(local_lib, .libPaths()))

library(dplyr)
library(rdrobust)

codex_setup_path <- file.path("codex", "_paths.R")
if (!file.exists(codex_setup_path)) codex_setup_path <- "_paths.R"
source(codex_setup_path)

load(codex_project_file("Quarterly_dataset1.RData"))

target_conditions <- c("FULL", "Q1Q2", "Q2Q3", "Q3Q4")

condition_fns <- list(
  FULL = function(d) d$ex_q1 == 1 | d$ex_q2 == 1 | d$ex_q3 == 1 | d$ex_q4 == 1,
  Q1Q2 = function(d) d$ex_q1 == 1 | d$ex_q2 == 1,
  Q2Q3 = function(d) d$ex_q2 == 1 | d$ex_q3 == 1,
  Q3Q4 = function(d) d$ex_q3 == 1 | d$ex_q4 == 1
)

as_date_safely <- function(x) {
  if (inherits(x, "Date")) return(x)
  as.Date(as.character(x))
}

latex_escape <- function(x) {
  x <- gsub("\\\\", "\\\\textbackslash{}", x)
  x <- gsub("([_#$%&{}])", "\\\\\\1", x, perl = TRUE)
  x <- gsub("<=", "$\\leq$", x, fixed = TRUE)
  x <- gsub(">=", "$\\geq$", x, fixed = TRUE)
  x
}

fmt_num <- function(x, digits = 3) {
  ifelse(is.na(x), "", formatC(round(x, digits), format = "f", digits = digits))
}

stars <- function(p) {
  ifelse(
    is.na(p), "",
    ifelse(p < 0.01, "***", ifelse(p < 0.05, "**", ifelse(p < 0.1, "*", "")))
  )
}

coef_with_stars <- function(coef, p) {
  ifelse(is.na(coef), "", paste0(fmt_num(coef), stars(p)))
}

paren_num <- function(x) {
  ifelse(is.na(x), "", paste0("(", fmt_num(x), ")"))
}

rd_panel_b <- function(data, h = NULL, use_time_fe = TRUE) {
  covs <- NULL
  if (use_time_fe) {
    dummy_vars <- as.data.frame(model.matrix(~ date3_ym - 1, data = data))
    if (ncol(dummy_vars) > 1) {
      covs <- as.matrix(dummy_vars[, -1, drop = FALSE])
    }
  }

  args <- list(
    y = log(data$avg_price) - log(data$ex_avg),
    x = data$running_scr - 4.75,
    fuzzy = data$host_is_superhost2,
    covs = covs,
    cluster = data$id,
    kernel = "tri",
    bwselect = "msetwo",
    p = 1,
    masspoints = "off",
    bwrestrict = TRUE
  )
  if (!is.null(h)) args$h <- h
  do.call(rdrobust, args)
}

tidy_est <- function(est, filter_id, filter_label, condition_name, n_panel_b) {
  data.frame(
    filter_id = filter_id,
    filter = filter_label,
    condition = condition_name,
    n_panel_b = n_panel_b,
    coef_conv = as.numeric(est[["Estimate"]][1]),
    coef_bc = as.numeric(est[["Estimate"]][2]),
    se_conv = as.numeric(est[["se"]][1]),
    pv_conv = as.numeric(est[["pv"]][1]),
    se_robust = as.numeric(est[["se"]][3]),
    pv_robust = as.numeric(est[["pv"]][3]),
    h_left = as.numeric(est[["bws"]][1, 1]),
    h_right = as.numeric(est[["bws"]][1, 2]),
    stringsAsFactors = FALSE
  )
}

error_row <- function(filter_id, filter_label, condition_name, n_panel_b, error_message) {
  data.frame(
    filter_id = filter_id,
    filter = filter_label,
    condition = condition_name,
    n_panel_b = n_panel_b,
    coef_conv = NA_real_,
    coef_bc = NA_real_,
    se_conv = NA_real_,
    pv_conv = NA_real_,
    se_robust = NA_real_,
    pv_robust = NA_real_,
    h_left = NA_real_,
    h_right = NA_real_,
    error = error_message,
    stringsAsFactors = FALSE
  )
}

write_filter_table <- function(df, filter_id, filter_label, path) {
  rows <- c(
    "\\begin{table}[!htbp]",
    "\\centering",
    paste0("\\caption{Good-result RD estimates: ", latex_escape(filter_label), "}"),
    paste0("\\label{tab:good_result_", filter_id, "}"),
    "\\begin{tabular}{lccccc}",
    "\\hline",
    "Condition & N & Conventional & Bias-corrected & $h_L$ & $h_R$ \\\\",
    "\\hline"
  )

  for (i in seq_len(nrow(df))) {
    r <- df[i, ]
    rows <- c(
      rows,
      paste(
        r$condition,
        r$n_panel_b,
        coef_with_stars(r$coef_conv, r$pv_conv),
        coef_with_stars(r$coef_bc, r$pv_robust),
        fmt_num(r$h_left, 4),
        fmt_num(r$h_right, 4),
        sep = " & "
      ) |> paste0(" \\\\"),
      paste(
        "",
        "",
        paren_num(r$se_conv),
        paren_num(r$se_robust),
        "",
        "",
        sep = " & "
      ) |> paste0(" \\\\")
    )
  }

  rows <- c(
    rows,
    "\\hline",
    "\\end{tabular}",
    "\\begin{flushleft}",
    "\\footnotesize Notes: Panel B sample restricts to $ex\\_super=f$ after applying the good-result filter. Outcome is $\\log(avg\\_price)-\\log(ex\\_avg)$. Running variable is $running\\_scr-4.75$. Regressions use fuzzy RD with triangular kernel, MSE two-sided bandwidth, time fixed effects, and listing-clustered standard errors. Conventional stars use conventional p-values; bias-corrected stars use robust p-values. $^{***}p<0.01$, $^{**}p<0.05$, $^{*}p<0.10$.",
    "\\end{flushleft}",
    "\\end{table}",
    ""
  )

  writeLines(rows, path)
  paste(rows, collapse = "\n")
}

z0 <- bind_rows(Q323, Q423, Q124, Q224, Q324, Q424)
z0$date3 <- factor(z0$Date)
z0$date3_ym <- format(as_date_safely(z0$Date), "%Y-%m")
z0$days_since_last_review <- as.numeric(as_date_safely(z0$Date) - as_date_safely(z0$last_review))
z0$minimum_nights_num <- suppressWarnings(as.numeric(z0$minimum_nights))
z0$ex_quarter_number_of_reviews_num <- suppressWarnings(as.numeric(z0$ex_quarter_number_of_reviews))

z0 <- z0 %>%
  filter(
    as.character(ex_super2) == "t",
    !is.na(running_scr),
    !is.na(host_is_superhost2),
    !is.na(id),
    !is.na(avg_price),
    !is.na(ex_avg),
    avg_price > 0,
    ex_avg > 0
  )

good_filters <- list(
  host_response_rate_not_na = list(
    label = "ex_super2=t & host_response_rate != N/A",
    fn = function(d) !is.na(d$host_response_rate) & as.character(d$host_response_rate) != "N/A"
  ),
  days_since_last_review_60 = list(
    label = "ex_super2=t & 0 <= days_since_last_review <= 60",
    fn = function(d) !is.na(d$days_since_last_review) & d$days_since_last_review >= 0 & d$days_since_last_review <= 60
  ),
  days_since_last_review_90 = list(
    label = "ex_super2=t & 0 <= days_since_last_review <= 90",
    fn = function(d) !is.na(d$days_since_last_review) & d$days_since_last_review >= 0 & d$days_since_last_review <= 90
  ),
  days_since_last_review_120 = list(
    label = "ex_super2=t & 0 <= days_since_last_review <= 120",
    fn = function(d) !is.na(d$days_since_last_review) & d$days_since_last_review >= 0 & d$days_since_last_review <= 120
  ),
  days_since_last_review_150 = list(
    label = "ex_super2=t & 0 <= days_since_last_review <= 150",
    fn = function(d) !is.na(d$days_since_last_review) & d$days_since_last_review >= 0 & d$days_since_last_review <= 150
  ),
  days_since_last_review_180 = list(
    label = "ex_super2=t & 0 <= days_since_last_review <= 180",
    fn = function(d) !is.na(d$days_since_last_review) & d$days_since_last_review >= 0 & d$days_since_last_review <= 180
  ),
  min_nights_30_reviews_10 = list(
    label = "ex_super2=t & minimum_nights >= 30 & ex_quarter_number_of_reviews <= 10",
    fn = function(d) {
      !is.na(d$minimum_nights_num) &
        d$minimum_nights_num >= 30 &
        !is.na(d$ex_quarter_number_of_reviews_num) &
        d$ex_quarter_number_of_reviews_num <= 10
    }
  )
)

all_results <- list()
table_blocks <- list()
tex_dir <- codex_project_file("tex")
dir.create(tex_dir, showWarnings = FALSE, recursive = TRUE)

for (filter_id in names(good_filters)) {
  spec <- good_filters[[filter_id]]
  keep_filter <- spec$fn(z0)
  keep_filter[is.na(keep_filter)] <- FALSE
  zf <- z0[keep_filter, , drop = FALSE]

  filter_results <- list()
  for (condition_name in target_conditions) {
    keep_condition <- condition_fns[[condition_name]](zf)
    keep_condition[is.na(keep_condition)] <- FALSE
    data_b <- zf[keep_condition & as.character(zf$ex_super) == "f", , drop = FALSE]

    est <- tryCatch(
      rd_panel_b(data_b),
      error = function(e) e
    )

    filter_results[[condition_name]] <- if (inherits(est, "error")) {
      error_row(filter_id, spec$label, condition_name, nrow(data_b), est$message)
    } else {
      tidy_est(est, filter_id, spec$label, condition_name, nrow(data_b))
    }
  }

  filter_df <- bind_rows(filter_results)
  all_results[[filter_id]] <- filter_df
  table_path <- file.path(tex_dir, paste0("good_result_", filter_id, ".tex"))
  table_blocks[[filter_id]] <- write_filter_table(filter_df, filter_id, spec$label, table_path)
}

all_results_df <- bind_rows(all_results)
write.csv(
  all_results_df,
  codex_project_file("tex", "good_result_regression_results.csv"),
  row.names = FALSE
)
writeLines(
  unlist(table_blocks, use.names = FALSE),
  codex_project_file("tex", "good_result_all_tables.tex")
)

summary_df <- all_results_df %>%
  group_by(filter_id, filter) %>%
  summarise(
    min_n_panel_b = min(n_panel_b, na.rm = TRUE),
    max_abs_conv = ifelse(all(is.na(coef_conv)), NA_real_, max(abs(coef_conv), na.rm = TRUE)),
    max_abs_bc = ifelse(all(is.na(coef_bc)), NA_real_, max(abs(coef_bc), na.rm = TRUE)),
    all_negative_conv = all(coef_conv < 0, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(max_abs_conv, desc(min_n_panel_b))

print(summary_df)
cat("Wrote good-result LaTeX tables to ", tex_dir, "\n", sep = "")
