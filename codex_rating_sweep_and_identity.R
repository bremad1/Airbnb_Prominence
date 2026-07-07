library(dplyr)
library(rdrobust)

setwd("C:/Users/brema/iCloudDrive/4-1/Airbnb/Test")
load("Quarterly_dataset1.RData")

target_conditions <- c("FULL", "Q1Q2", "Q2Q3", "Q3Q4")
rating_cuts <- c(4.0, 4.2, 4.3, 4.4, 4.5, 4.6, 4.7, 4.8, 4.9)

z_all <- rbind(Q323, Q423, Q124, Q224, Q324, Q424)
z_all$date3 <- factor(z_all$Date)
z_all$date3_ym <- format(as.Date(z_all$date3), "%Y-%m")

condition_fns <- list(
  FULL = function(d) d$ex_q1 == 1 | d$ex_q2 == 1 | d$ex_q3 == 1 | d$ex_q4 == 1,
  Q1Q2 = function(d) d$ex_q1 == 1 | d$ex_q2 == 1,
  Q2Q3 = function(d) d$ex_q2 == 1 | d$ex_q3 == 1,
  Q3Q4 = function(d) d$ex_q3 == 1 | d$ex_q4 == 1
)

rd_panel_b_col1 <- function(data) {
  dummy_vars <- as.data.frame(model.matrix(~ date3_ym - 1, data = data))
  covs <- NULL
  if (ncol(dummy_vars) > 1) {
    covs <- as.matrix(dummy_vars[, -1, drop = FALSE])
  }

  rdrobust(
    y = log(data$avg_price) - log(data$ex_avg),
    x = data$running_scr - 4.75,
    fuzzy = data$host_is_superhost2,
    covs = covs,
    all = TRUE,
    cluster = data$id,
    kernel = "tri",
    bwselect = "msetwo",
    p = 1,
    masspoints = "off",
    bwrestrict = TRUE
  )
}

z_base <- z_all %>%
  filter(
    ex_super2 == "t",
    !is.na(ex_quarter_rating),
    !is.na(running_scr),
    !is.na(host_is_superhost2),
    !is.na(id),
    !is.na(avg_price),
    !is.na(ex_avg),
    avg_price > 0,
    ex_avg > 0
  )

rating_results <- list()
for (rating_cut in rating_cuts) {
  zf <- z_base %>% filter(ex_quarter_rating >= rating_cut)

  for (condition_name in target_conditions) {
    data_b <- zf[
      condition_fns[[condition_name]](zf) & zf$ex_super == "f",
      ,
      drop = FALSE
    ]

    est <- tryCatch(rd_panel_b_col1(data_b), error = function(e) NULL)
    if (is.null(est)) next

    rating_results[[length(rating_results) + 1]] <- data.frame(
      rating_cut = rating_cut,
      condition = condition_name,
      n_panel_b = nrow(data_b),
      coef_conv = as.numeric(est[["Estimate"]][1]),
      se_conv = as.numeric(est[["se"]][1]),
      pv_conv = as.numeric(est[["pv"]][1]),
      coef_bc = as.numeric(est[["Estimate"]][2]),
      pv_robust = as.numeric(est[["pv"]][3]),
      h_left = as.numeric(est[["bws"]][1, 1]),
      h_right = as.numeric(est[["bws"]][1, 2]),
      stringsAsFactors = FALSE
    )
  }
}

rating_results <- bind_rows(rating_results)
rating_summary <- rating_results %>%
  group_by(rating_cut) %>%
  summarise(
    max_abs_conv = max(abs(coef_conv), na.rm = TRUE),
    max_pv_conv = max(pv_conv, na.rm = TRUE),
    min_pv_conv = min(pv_conv, na.rm = TRUE),
    all_negative_conv = all(coef_conv < 0),
    ok_conv = all(coef_conv < 0 & abs(coef_conv) < 0.1),
    any_sig_10 = any(pv_conv < 0.1),
    any_sig_5 = any(pv_conv < 0.05),
    min_n_panel_b = min(n_panel_b, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(rating_cut)

identity_by_ex_super <- z_all %>%
  filter(!is.na(ex_super), !is.na(host_identity_verified)) %>%
  group_by(ex_super) %>%
  summarise(
    n = n(),
    host_identity_t = sum(host_identity_verified == "t"),
    prob_host_identity_t = mean(host_identity_verified == "t"),
    .groups = "drop"
  )

identity_by_ex_super2 <- z_all %>%
  filter(!is.na(ex_super2), !is.na(host_identity_verified)) %>%
  group_by(ex_super2) %>%
  summarise(
    n = n(),
    host_identity_t = sum(host_identity_verified == "t"),
    prob_host_identity_t = mean(host_identity_verified == "t"),
    .groups = "drop"
  )

identity_by_current_superhost <- z_all %>%
  filter(!is.na(host_is_superhost2), !is.na(host_identity_verified)) %>%
  group_by(host_is_superhost2) %>%
  summarise(
    n = n(),
    host_identity_t = sum(host_identity_verified == "t"),
    prob_host_identity_t = mean(host_identity_verified == "t"),
    .groups = "drop"
  )

stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
dir.create("results", showWarnings = FALSE, recursive = TRUE)
rating_all_path <- file.path("results", paste0("codex_rating_sweep_all_", stamp, ".csv"))
rating_summary_path <- file.path("results", paste0("codex_rating_sweep_summary_", stamp, ".csv"))
identity_ex_super_path <- file.path("results", paste0("codex_identity_by_ex_super_", stamp, ".csv"))
identity_ex_super2_path <- file.path("results", paste0("codex_identity_by_ex_super2_", stamp, ".csv"))
identity_current_path <- file.path("results", paste0("codex_identity_by_host_is_superhost2_", stamp, ".csv"))

write.csv(rating_results, rating_all_path, row.names = FALSE)
write.csv(rating_summary, rating_summary_path, row.names = FALSE)
write.csv(identity_by_ex_super, identity_ex_super_path, row.names = FALSE)
write.csv(identity_by_ex_super2, identity_ex_super2_path, row.names = FALSE)
write.csv(identity_by_current_superhost, identity_current_path, row.names = FALSE)

cat("\nRATING SUMMARY\n")
print(rating_summary)
cat("\nRATING DETAILS\n")
print(rating_results %>% arrange(rating_cut, match(condition, target_conditions)))
cat("\nIDENTITY BY ex_super\n")
print(identity_by_ex_super)
cat("\nIDENTITY BY ex_super2\n")
print(identity_by_ex_super2)
cat("\nIDENTITY BY host_is_superhost2\n")
print(identity_by_current_superhost)

cat("\nWROTE\n")
cat(rating_all_path, "\n")
cat(rating_summary_path, "\n")
cat(identity_ex_super_path, "\n")
cat(identity_ex_super2_path, "\n")
cat(identity_current_path, "\n")
