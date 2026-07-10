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
load(file.path(project_root, "RData", "Entire.RData"))

if (as.character(packageVersion("rdrobust")) != "3.0.0") {
  stop("Expected rdrobust 3.0.0, got ", packageVersion("rdrobust"))
}

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
    .date = as.Date(Date),
    .year = as.integer(format(.date, "%Y")),
    .month = as.integer(format(.date, "%m"))
  )

availability_sums <- lapply(seq_len(nrow(quarter_map)), function(i) {
  qm <- quarter_map[i, ]
  entire_dates %>%
    filter(.year == qm$target_year, .month %in% c(qm$month_a, qm$month_b)) %>%
    group_by(id) %>%
    summarise(
      quarter = qm$quarter,
      availability_365_sum_q = sum(availability_365, na.rm = TRUE),
      availability_365_months_q = n_distinct(.month[!is.na(availability_365)]),
      availability_365_365_months_q = sum(availability_365 == 365, na.rm = TRUE),
      .groups = "drop"
    )
}) %>%
  bind_rows()

z <- bind_rows(lapply(names(quarter_list), function(qname) {
  quarter_list[[qname]] %>% mutate(quarter = qname)
}))

z <- z %>% left_join(availability_sums, by = c("id", "quarter"))
z$date3 <- factor(z$Date)
z$date3_ym <- format(as.Date(z$date3), "%Y-%m")

full_condition <- z$ex_q1 == 1 | z$ex_q2 == 1 | z$ex_q3 == 1 | z$ex_q4 == 1
full_condition[is.na(full_condition)] <- FALSE

ltm_zero <- (
  (!is.na(z$number_of_reviews_ltm) & z$number_of_reviews_ltm == 0) |
    (!is.na(z$ex_quarter_ltm) & z$ex_quarter_ltm == 0)
)

row_av365365 <- !is.na(z$availability_365) & z$availability_365 == 365
sum_av365730 <- !is.na(z$availability_365_sum_q) & z$availability_365_sum_q == 730

samples <- list(
  panel_b_ex2 = list(
    label = "ex_super=f & ex_super2=t",
    keep = z$ex_super == "f" & z$ex_super2 == "t" & full_condition
  ),
  panel_b_ex2_verified = list(
    label = "ex_super=f & ex_super2=t & host_identity_verified=t",
    keep = z$ex_super == "f" & z$ex_super2 == "t" & z$host_identity_verified == "t" & full_condition
  )
)

filters <- list(
  baseline = list(
    label = "baseline",
    fn = rep(TRUE, nrow(z))
  ),
  drop_row_av365365_ltm0_or_exltm0 = list(
    label = "drop row av365=365 & (ltm=0 or ex_q_ltm=0)",
    fn = !(row_av365365 & ltm_zero)
  ),
  drop_sum_av365730_ltm0_or_exltm0 = list(
    label = "drop quarter-sum av365=730 & (ltm=0 or ex_q_ltm=0)",
    fn = !(sum_av365730 & ltm_zero)
  )
)

rd_call <- function(data, h = NULL, use_time_fe = TRUE) {
  args <- list(
    y = log(data$avg_price) - log(data$ex_avg),
    x = data$running_scr - 4.75,
    fuzzy = data$host_is_superhost2,
    cluster = data$id,
    kernel = "tri",
    bwselect = "msetwo",
    p = 1,
    masspoints = "off",
    bwrestrict = TRUE,
    all = TRUE
  )
  if (!is.null(h)) args$h <- h
  if (use_time_fe) {
    dummy_vars <- as.data.frame(model.matrix(~ date3_ym - 1, data = data))
    dummy_vars <- dummy_vars[, -1, drop = FALSE]
    if (ncol(dummy_vars) > 0) args$covs <- cbind(dummy_vars)
  }
  do.call(rdrobust, args)
}

extract <- function(est) {
  data.frame(
    coef_conv = as.numeric(est[["Estimate"]][1]),
    se_conv = as.numeric(est[["se"]][1]),
    pv_conv = as.numeric(est[["pv"]][1]),
    coef_bc = as.numeric(est[["Estimate"]][2]),
    se_robust = as.numeric(est[["se"]][3]),
    pv_robust = as.numeric(est[["pv"]][3]),
    h_left = as.numeric(est[["bws"]][1, 1]),
    h_right = as.numeric(est[["bws"]][1, 2]),
    obs_h = sum(est[["N_h"]]),
    stringsAsFactors = FALSE
  )
}

spec_names <- c("msetwo_fe", "twomse_fe", "h020_010_fe", "h030_015_fe", "h040_020_fe", "msetwo_no_fe")
rows <- list()
counts <- list()

for (sample_id in names(samples)) {
  sample_keep <- samples[[sample_id]]$keep
  sample_keep[is.na(sample_keep)] <- FALSE

  for (filter_id in names(filters)) {
    filter_keep <- filters[[filter_id]]$fn
    filter_keep[is.na(filter_keep)] <- FALSE
    keep <- sample_keep & filter_keep
    data <- z[keep, , drop = FALSE]
    dropped <- sample_keep & !filter_keep

    counts[[paste(sample_id, filter_id, sep = "__")]] <- data.frame(
      sample_id = sample_id,
      sample = samples[[sample_id]]$label,
      filter_id = filter_id,
      filter = filters[[filter_id]]$label,
      raw_n = sum(sample_keep),
      kept_n = nrow(data),
      dropped_n = sum(dropped),
      dropped_ltm0 = sum(dropped & !is.na(z$number_of_reviews_ltm) & z$number_of_reviews_ltm == 0),
      dropped_ex_quarter_ltm0 = sum(dropped & !is.na(z$ex_quarter_ltm) & z$ex_quarter_ltm == 0),
      dropped_row_av365365 = sum(dropped & row_av365365),
      dropped_sum_av365730 = sum(dropped & sum_av365730),
      dropped_mean_number_reviews_ltm = mean(z$number_of_reviews_ltm[dropped], na.rm = TRUE),
      dropped_mean_ex_quarter_ltm = mean(z$ex_quarter_ltm[dropped], na.rm = TRUE),
      stringsAsFactors = FALSE
    )

    ests <- tryCatch({
      est1 <- rd_call(data)
      list(
        msetwo_fe = est1,
        twomse_fe = rd_call(data, h = c(2 * est1[["bws"]][1, 1], 2 * est1[["bws"]][1, 2])),
        h020_010_fe = rd_call(data, h = c(0.2, 0.1)),
        h030_015_fe = rd_call(data, h = c(0.3, 0.15)),
        h040_020_fe = rd_call(data, h = c(0.4, 0.2)),
        msetwo_no_fe = rd_call(data, use_time_fe = FALSE)
      )
    }, error = function(e) e)

    if (inherits(ests, "error")) {
      for (spec_name in spec_names) {
        rows[[paste(sample_id, filter_id, spec_name, sep = "__")]] <- data.frame(
          sample_id = sample_id,
          sample = samples[[sample_id]]$label,
          filter_id = filter_id,
          filter = filters[[filter_id]]$label,
          raw_n_panel_b = nrow(data),
          spec = spec_name,
          coef_conv = NA_real_,
          se_conv = NA_real_,
          pv_conv = NA_real_,
          coef_bc = NA_real_,
          se_robust = NA_real_,
          pv_robust = NA_real_,
          h_left = NA_real_,
          h_right = NA_real_,
          obs_h = NA_integer_,
          error = ests$message,
          stringsAsFactors = FALSE
        )
      }
      next
    }

    for (spec_name in names(ests)) {
      rows[[paste(sample_id, filter_id, spec_name, sep = "__")]] <- data.frame(
        sample_id = sample_id,
        sample = samples[[sample_id]]$label,
        filter_id = filter_id,
        filter = filters[[filter_id]]$label,
        raw_n_panel_b = nrow(data),
        spec = spec_name,
        extract(ests[[spec_name]]),
        error = "",
        stringsAsFactors = FALSE
      )
    }
  }
}

result <- bind_rows(rows)
count_result <- bind_rows(counts)

write.csv(result, file.path(out_dir, "av365_365_zero_ltm_filter_rdrobust300.csv"), row.names = FALSE)
write.csv(count_result, file.path(out_dir, "av365_365_zero_ltm_filter_counts.csv"), row.names = FALSE)

star <- function(p) {
  ifelse(is.na(p), "",
    ifelse(p < 0.01, "***",
      ifelse(p < 0.05, "**",
        ifelse(p < 0.1, "*", ""))))
}
strip0 <- function(x) ifelse(is.na(x), "", sub("^0", "", sprintf("%.3f", x)))
tex_escape <- function(x) {
  x <- gsub("&", "\\&", x, fixed = TRUE)
  x <- gsub("_", "\\_", x, fixed = TRUE)
  x
}

tex_lines <- c(
  "\\begin{tabular}{llcccccc}",
  "\\hline",
  "Sample & Filter & (1) & (2) & (3) & (4) & (5) & (6) \\\\",
  "\\hline"
)

for (sample_id in names(samples)) {
  for (filter_id in names(filters)) {
    sub <- result[result$sample_id == sample_id & result$filter_id == filter_id, ]
    sub <- sub[match(spec_names, sub$spec), ]
    count_row <- count_result[count_result$sample_id == sample_id & count_result$filter_id == filter_id, ]
    coefs <- paste0(strip0(sub$coef_conv), star(sub$pv_conv))
    ses <- paste0("(", strip0(sub$se_conv), ")")
    tex_lines <- c(
      tex_lines,
      paste(c(tex_escape(sample_id), paste0(tex_escape(filters[[filter_id]]$label), " (N=", count_row$kept_n, ", drop=", count_row$dropped_n, ")"), coefs), collapse = " & "),
      "\\\\",
      paste(c("", "", ses), collapse = " & "),
      "\\\\"
    )
  }
}

tex_lines <- c(tex_lines, "\\hline", "\\end{tabular}")
writeLines(tex_lines, file.path(out_dir, "av365_365_zero_ltm_filter_rdrobust300.tex"))

cat("rdrobust", as.character(packageVersion("rdrobust")), "\n")
cat("\nCounts:\n")
print(count_result)
cat("\nResults:\n")
print(result[, c("sample_id", "filter_id", "raw_n_panel_b", "spec", "coef_conv", "se_conv", "pv_conv", "coef_bc", "pv_robust", "obs_h", "error")], row.names = FALSE)
