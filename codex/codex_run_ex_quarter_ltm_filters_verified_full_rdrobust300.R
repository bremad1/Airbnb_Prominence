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

z <- rbind(Q323, Q423, Q124, Q224, Q324, Q424)
z$date3 <- factor(z$Date)
z$date3_ym <- format(as.Date(z$date3), "%Y-%m")

if (as.character(packageVersion("rdrobust")) != "3.0.0") {
  stop("Expected rdrobust 3.0.0, got ", packageVersion("rdrobust"))
}

x_ltm <- z$ex_quarter_ltm
base_keep <- z$ex_super2 == "t" & z$ex_super == "f" & z$host_identity_verified == "t"
base_keep[is.na(base_keep)] <- FALSE
full_condition <- z$ex_q1 == 1 | z$ex_q2 == 1 | z$ex_q3 == 1 | z$ex_q4 == 1
full_condition[is.na(full_condition)] <- FALSE

filters <- list(
  baseline_verified = list(
    label = "host_identity_verified=t only",
    family = "baseline",
    cutoff = NA_real_,
    fn = rep(TRUE, nrow(z))
  )
)

for (k in c(0, 1, 2, 3, 5, 10, 15, 20, 30)) {
  filters[[paste0("exqltm_ge", k)]] <- list(
    label = paste0("ex_quarter_ltm >= ", k),
    family = "lower_bound",
    cutoff = k,
    fn = !is.na(x_ltm) & x_ltm >= k
  )
}

for (k in c(0, 1, 2, 3, 5, 10, 15, 20, 30, 50)) {
  filters[[paste0("exqltm_le", k)]] <- list(
    label = paste0("ex_quarter_ltm <= ", k),
    family = "upper_bound",
    cutoff = k,
    fn = !is.na(x_ltm) & x_ltm <= k
  )
}

range_filters <- list(
  exqltm_1_2 = c(1, 2),
  exqltm_1_3 = c(1, 3),
  exqltm_1_5 = c(1, 5),
  exqltm_1_10 = c(1, 10),
  exqltm_2_3 = c(2, 3),
  exqltm_2_5 = c(2, 5),
  exqltm_2_10 = c(2, 10),
  exqltm_2_20 = c(2, 20),
  exqltm_3_20 = c(3, 20),
  exqltm_5_20 = c(5, 20),
  exqltm_5_30 = c(5, 30),
  exqltm_10_30 = c(10, 30)
)

for (id in names(range_filters)) {
  bounds <- range_filters[[id]]
  filters[[id]] <- list(
    label = paste0(bounds[1], " <= ex_quarter_ltm <= ", bounds[2]),
    family = "range",
    cutoff = paste(bounds, collapse = "-"),
    fn = !is.na(x_ltm) & x_ltm >= bounds[1] & x_ltm <= bounds[2]
  )
}

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
count_rows <- list()
price_gap <- log(z$avg_price) - log(z$ex_avg)

for (filter_id in names(filters)) {
  fkeep <- filters[[filter_id]]$fn
  fkeep[is.na(fkeep)] <- FALSE
  keep <- base_keep & full_condition & fkeep
  keep[is.na(keep)] <- FALSE
  dropped <- base_keep & full_condition & !fkeep
  dropped[is.na(dropped)] <- FALSE
  data <- z[keep, , drop = FALSE]

  count_rows[[filter_id]] <- data.frame(
    filter_id = filter_id,
    filter = filters[[filter_id]]$label,
    family = filters[[filter_id]]$family,
    cutoff = as.character(filters[[filter_id]]$cutoff),
    raw_verified_full = sum(base_keep & full_condition),
    kept_verified_full = nrow(data),
    dropped_verified_full = sum(dropped),
    kept_ex_quarter_ltm_mean = mean(x_ltm[keep], na.rm = TRUE),
    kept_ex_quarter_ltm_median = median(x_ltm[keep], na.rm = TRUE),
    kept_mean_log_price_gap = mean(price_gap[keep], na.rm = TRUE),
    dropped_mean_log_price_gap = mean(price_gap[dropped], na.rm = TRUE),
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
      rows[[paste(filter_id, spec_name, sep = "__")]] <- data.frame(
        filter_id = filter_id,
        filter = filters[[filter_id]]$label,
        family = filters[[filter_id]]$family,
        cutoff = as.character(filters[[filter_id]]$cutoff),
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
    rows[[paste(filter_id, spec_name, sep = "__")]] <- data.frame(
      filter_id = filter_id,
      filter = filters[[filter_id]]$label,
      family = filters[[filter_id]]$family,
      cutoff = as.character(filters[[filter_id]]$cutoff),
      raw_n_panel_b = nrow(data),
      spec = spec_name,
      extract(ests[[spec_name]]),
      error = "",
      stringsAsFactors = FALSE
    )
  }
}

result <- bind_rows(rows)
counts <- bind_rows(count_rows)

write.csv(result, file.path(out_dir, "ex_quarter_ltm_verified_full_filters_rdrobust300.csv"), row.names = FALSE)
write.csv(counts, file.path(out_dir, "ex_quarter_ltm_verified_full_counts.csv"), row.names = FALSE)

hits <- result %>%
  filter(error == "", coef_conv < 0, abs(coef_conv) <= 0.1, pv_conv < 0.1) %>%
  arrange(pv_conv, abs(coef_conv))
write.csv(hits, file.path(out_dir, "ex_quarter_ltm_verified_full_hits.csv"), row.names = FALSE)

best_by_filter <- result %>%
  filter(error == "", coef_conv < 0) %>%
  group_by(filter_id, filter, family, cutoff) %>%
  arrange(pv_conv, .by_group = TRUE) %>%
  slice(1) %>%
  ungroup()
write.csv(best_by_filter, file.path(out_dir, "ex_quarter_ltm_verified_full_best_by_filter.csv"), row.names = FALSE)

all_negative <- result %>%
  filter(error == "") %>%
  group_by(filter_id, filter, family, cutoff, raw_n_panel_b) %>%
  summarise(
    n_negative = sum(coef_conv < 0, na.rm = TRUE),
    max_coef = max(coef_conv, na.rm = TRUE),
    min_p = min(pv_conv, na.rm = TRUE),
    best_coef = coef_conv[which.min(pv_conv)],
    best_spec = spec[which.min(pv_conv)],
    .groups = "drop"
  ) %>%
  arrange(desc(n_negative), max_coef, min_p)
write.csv(all_negative, file.path(out_dir, "ex_quarter_ltm_verified_full_shape_rank.csv"), row.names = FALSE)

cat("rdrobust", as.character(packageVersion("rdrobust")), "\n")
cat("\nHits:\n")
print(hits %>% select(filter, family, cutoff, raw_n_panel_b, spec, coef_conv, se_conv, pv_conv, coef_bc, pv_robust, obs_h))
cat("\nBest negative spec by filter:\n")
print(best_by_filter %>% select(filter, family, cutoff, raw_n_panel_b, spec, coef_conv, se_conv, pv_conv, coef_bc, pv_robust, obs_h))
cat("\nShape rank:\n")
print(all_negative)
