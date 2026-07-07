library(dplyr)
library(rdrobust)

codex_setup_path <- file.path("codex", "_paths.R")
if (!file.exists(codex_setup_path)) codex_setup_path <- "_paths.R"
source(codex_setup_path)

load(codex_project_file("Quarterly_dataset1.RData"))

target_conditions <- c("FULL", "Q1Q2", "Q2Q3", "Q3Q4")
min_panel_b_n <- 800

add_ex_quarter_host_ltm <- function(data) {
  host_ltm <- data %>%
    distinct(Date, host_id, id, .keep_all = TRUE) %>%
    group_by(Date, host_id) %>%
    summarise(
      ex_quarter_host_ltm = ifelse(all(is.na(ex_quarter_ltm)), NA_real_, sum(ex_quarter_ltm, na.rm = TRUE)),
      .groups = "drop"
    )

  data %>%
    select(-any_of("ex_quarter_host_ltm")) %>%
    left_join(host_ltm, by = c("Date", "host_id"))
}

z0 <- rbind(Q323, Q423, Q124, Q224, Q324, Q424)
z0 <- add_ex_quarter_host_ltm(z0)
z0$date3 <- factor(z0$Date)
z0$date3_ym <- format(as.Date(z0$date3), "%Y-%m")
z0 <- z0 %>%
  filter(
    !is.na(running_scr),
    !is.na(host_is_superhost2),
    !is.na(id),
    !is.na(avg_price),
    !is.na(ex_avg),
    !is.na(ex_quarter_host_ltm),
    avg_price > 0,
    ex_avg > 0
  )

condition_fns <- list(
  FULL = function(d) d$ex_q1 == 1 | d$ex_q2 == 1 | d$ex_q3 == 1 | d$ex_q4 == 1,
  Q1Q2 = function(d) d$ex_q1 == 1 | d$ex_q2 == 1,
  Q2Q3 = function(d) d$ex_q2 == 1 | d$ex_q3 == 1,
  Q3Q4 = function(d) d$ex_q3 == 1 | d$ex_q4 == 1
)

base <- function(d) d$ex_quarter_host_ltm >= 5 & d$ex_quarter_number_of_reviews >= 20

atoms <- list(
  "none" = function(d) TRUE,
  "ex_rating>=4" = function(d) !is.na(d$ex_quarter_rating) & d$ex_quarter_rating >= 4,
  "ex_rating>=4.3" = function(d) !is.na(d$ex_quarter_rating) & d$ex_quarter_rating >= 4.3,
  "ex_rating>=4.5" = function(d) !is.na(d$ex_quarter_rating) & d$ex_quarter_rating >= 4.5,
  "score_rating>=4.5" = function(d) !is.na(d$review_scores_rating) & d$review_scores_rating >= 4.5,
  "score_clean>=4.5" = function(d) !is.na(d$review_scores_cleanliness) & d$review_scores_cleanliness >= 4.5,
  "score_value>=4.5" = function(d) !is.na(d$review_scores_value) & d$review_scores_value >= 4.5,
  "min_nights<=30" = function(d) d$minimum_nights <= 30,
  "availability_365>0" = function(d) d$availability_365 > 0,
  "availability_365<=325" = function(d) d$availability_365 <= 325,
  "availability_90>0" = function(d) d$availability_90 > 0,
  "accommodates<=6" = function(d) d$accommodates <= 6,
  "accommodates<=4" = function(d) d$accommodates <= 4,
  "bedrooms<=3" = function(d) !is.na(d$bedrooms) & d$bedrooms <= 3,
  "beds<=3" = function(d) !is.na(d$beds) & d$beds <= 3,
  "avg_price_100_400" = function(d) d$avg_price >= 100 & d$avg_price <= 400,
  "avg_price_125_400" = function(d) d$avg_price >= 125 & d$avg_price <= 400,
  "instant_bookable_f" = function(d) d$instant_bookable == "f",
  "rental_unit" = function(d) d$property_type == "Entire rental unit"
)

filters <- list("host_ltm>=5 & ex_reviews>=20" = base)

atom_names <- setdiff(names(atoms), "none")
for (a in atom_names) {
  filters[[paste("host_ltm>=5 & ex_reviews>=20", a, sep = " & ")]] <- local({
    fa <- atoms[[a]]
    function(d) base(d) & fa(d)
  })
}

quality <- c("ex_rating>=4", "ex_rating>=4.3", "ex_rating>=4.5", "score_rating>=4.5", "score_clean>=4.5", "score_value>=4.5")
stabilizers <- setdiff(atom_names, quality)
for (q in quality) {
  for (s in stabilizers) {
    filters[[paste("host_ltm>=5 & ex_reviews>=20", q, s, sep = " & ")]] <- local({
      fq <- atoms[[q]]
      fs <- atoms[[s]]
      function(d) base(d) & fq(d) & fs(d)
    })
  }
}

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

tidy_est <- function(est, filter_label, condition_name, n_panel_b) {
  data.frame(
    filter = filter_label,
    condition = condition_name,
    n_panel_b = n_panel_b,
    coef_conv = as.numeric(est[["Estimate"]][1]),
    coef_bc = as.numeric(est[["Estimate"]][2]),
    se_robust = as.numeric(est[["se"]][3]),
    pv_robust = as.numeric(est[["pv"]][3]),
    h_left = as.numeric(est[["bws"]][1, 1]),
    h_right = as.numeric(est[["bws"]][1, 2]),
    stringsAsFactors = FALSE
  )
}

results <- list()
summary_rows <- list()
message("Testing ", length(filters), " refined host_ltm filters")

for (filter_label in names(filters)) {
  zf <- z0[filters[[filter_label]](z0), , drop = FALSE]
  out <- list()
  skip <- FALSE

  for (condition_name in target_conditions) {
    data_b <- zf[condition_fns[[condition_name]](zf) & zf$ex_super == "f", , drop = FALSE]
    if (nrow(data_b) < min_panel_b_n) {
      skip <- TRUE
      break
    }

    est <- tryCatch(rd_panel_b_col1(data_b), error = function(e) NULL)
    if (is.null(est)) {
      skip <- TRUE
      break
    }
    out[[condition_name]] <- tidy_est(est, filter_label, condition_name, nrow(data_b))
  }

  if (skip) next

  df <- bind_rows(out)
  results[[length(results) + 1]] <- df
  summary_rows[[length(summary_rows) + 1]] <- data.frame(
    filter = filter_label,
    max_abs_conv = max(abs(df$coef_conv)),
    max_abs_bc = max(abs(df$coef_bc)),
    min_n_panel_b = min(df$n_panel_b),
    max_pv_robust = max(df$pv_robust),
    ok_conv = all(df$coef_conv < 0 & abs(df$coef_conv) < 0.1),
    ok_bc = all(df$coef_bc < 0 & abs(df$coef_bc) < 0.1),
    stringsAsFactors = FALSE
  )
}

all_results <- bind_rows(results)
summary_df <- bind_rows(summary_rows) %>%
  arrange(desc(ok_conv), desc(ok_bc), max_abs_conv, max_abs_bc, desc(min_n_panel_b))
pass_df <- summary_df %>% filter(ok_conv)
strict_df <- summary_df %>% filter(ok_conv, ok_bc)

print(head(summary_df, 20))

if (nrow(pass_df) > 0) {
  best_filter <- pass_df$filter[1]
  print(all_results %>% filter(filter == best_filter) %>% arrange(match(condition, target_conditions)))
}
