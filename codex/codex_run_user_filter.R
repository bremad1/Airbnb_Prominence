library(dplyr)
library(rdrobust)

codex_setup_path <- file.path("codex", "_paths.R")
if (!file.exists(codex_setup_path)) codex_setup_path <- "_paths.R"
source(codex_setup_path)

load(codex_project_file("Quarterly_dataset1.RData"))

target_conditions <- c("FULL", "Q1Q2", "Q2Q3", "Q3Q4")

z <- rbind(Q323, Q423, Q124, Q224, Q324, Q424)
z$date3 <- factor(z$Date)
z$date3_ym <- format(as.Date(z$date3), "%Y-%m")

z <- z %>%
  filter(
    ex_super2 == "t",
    ex_quarter_rating >= 4.7,
    host_identity_verified == "t",
    !is.na(running_scr),
    !is.na(host_is_superhost2),
    !is.na(id),
    !is.na(avg_price),
    !is.na(ex_avg),
    avg_price > 0,
    ex_avg > 0
  )

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

results <- lapply(target_conditions, function(condition_name) {
  data_b <- z[
    condition_fns[[condition_name]](z) & z$ex_super == "f",
    ,
    drop = FALSE
  ]
  est <- rd_panel_b_col1(data_b)

  data.frame(
    condition = condition_name,
    n_panel_b = nrow(data_b),
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
})

results <- bind_rows(results)
print(results)
cat("max_abs_conv=", max(abs(results$coef_conv)), "\n")
cat("max_abs_bc=", max(abs(results$coef_bc)), "\n")
