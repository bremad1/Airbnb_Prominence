library(dplyr)
library(rdrobust)

codex_setup_path <- file.path("codex", "_paths.R")
if (!file.exists(codex_setup_path)) codex_setup_path <- "_paths.R"
source(codex_setup_path)

load(codex_project_file("Quarterly_dataset1.RData"))

target_conditions <- c("FULL", "Q1Q2", "Q2Q3", "Q3Q4")
ltm_cuts <- 1:5

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

star <- function(p) {
  ifelse(p < 0.01, "***", ifelse(p < 0.05, "**", ifelse(p < 0.1, "*", "")))
}

z_base <- z_all %>%
  filter(
    ex_super2 == "t",
    !is.na(ex_quarter_ltm),
    !is.na(running_scr),
    !is.na(host_is_superhost2),
    !is.na(id),
    !is.na(avg_price),
    !is.na(ex_avg),
    avg_price > 0,
    ex_avg > 0
  )

results <- list()
for (ltm_cut in ltm_cuts) {
  zf <- z_base %>% filter(ex_quarter_ltm >= ltm_cut)

  for (condition_name in target_conditions) {
    data_b <- zf[
      condition_fns[[condition_name]](zf) & zf$ex_super == "f",
      ,
      drop = FALSE
    ]
    est <- rd_panel_b_col1(data_b)

    results[[length(results) + 1]] <- data.frame(
      ltm_cut = ltm_cut,
      condition = condition_name,
      n_panel_b = nrow(data_b),
      coef_conv = as.numeric(est[["Estimate"]][1]),
      se_conv = as.numeric(est[["se"]][1]),
      pv_conv = as.numeric(est[["pv"]][1]),
      coef_text = paste0(sprintf("%.3f", as.numeric(est[["Estimate"]][1])), star(as.numeric(est[["pv"]][1]))),
      se_text = paste0("(", sprintf("%.3f", as.numeric(est[["se"]][1])), ")"),
      stringsAsFactors = FALSE
    )
  }
}

results <- bind_rows(results)

cat("\nLTM SWEEP RESULTS\n")
print(results %>% arrange(ltm_cut, match(condition, target_conditions)))
