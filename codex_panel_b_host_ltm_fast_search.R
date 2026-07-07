library(dplyr)
library(rdrobust)

setwd("C:/Users/brema/iCloudDrive/4-1/Airbnb/Test")
load("Quarterly_dataset1.RData")

target_conditions <- c("FULL", "Q1Q2", "Q2Q3", "Q3Q4")
min_panel_b_n <- 1000

add_ex_quarter_host_ltm <- function(data) {
  host_ltm <- data %>%
    distinct(Date, host_id, id, .keep_all = TRUE) %>%
    group_by(Date, host_id) %>%
    summarise(
      ex_quarter_host_ltm = ifelse(
        all(is.na(ex_quarter_ltm)),
        NA_real_,
        sum(ex_quarter_ltm, na.rm = TRUE)
      ),
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

filter_specs <- list()
add_filter <- function(label, fn) {
  filter_specs[[label]] <<- fn
}

host_base <- function(d) d$ex_quarter_host_ltm >= 3
add_filter("host_ltm>=3", host_base)

rating_cuts <- c(4, 4.3, 4.5, 4.6)
for (x in rating_cuts) {
  add_filter(
    paste0("host_ltm>=3 & ex_rating>=", x),
    local({ xx <- x; function(d) host_base(d) & !is.na(d$ex_quarter_rating) & d$ex_quarter_rating >= xx })
  )
}

for (x in c(4.5, 4.6, 4.7)) {
  add_filter(
    paste0("host_ltm>=3 & score_rating>=", x),
    local({ xx <- x; function(d) host_base(d) & !is.na(d$review_scores_rating) & d$review_scores_rating >= xx })
  )
  add_filter(
    paste0("host_ltm>=3 & score_clean>=", x),
    local({ xx <- x; function(d) host_base(d) & !is.na(d$review_scores_cleanliness) & d$review_scores_cleanliness >= xx })
  )
  add_filter(
    paste0("host_ltm>=3 & score_value>=", x),
    local({ xx <- x; function(d) host_base(d) & !is.na(d$review_scores_value) & d$review_scores_value >= xx })
  )
}

for (x in c(5, 10, 20, 30)) {
  add_filter(
    paste0("host_ltm>=3 & ex_reviews>=", x),
    local({ xx <- x; function(d) host_base(d) & d$ex_quarter_number_of_reviews >= xx })
  )
}

for (x in c(10, 20, 50)) {
  add_filter(
    paste0("host_ltm>=3 & total_reviews>=", x),
    local({ xx <- x; function(d) host_base(d) & d$number_of_reviews >= xx })
  )
}

for (x in c(1, 5, 10)) {
  add_filter(
    paste0("host_ltm>=3 & reviews_ltm>=", x),
    local({ xx <- x; function(d) host_base(d) & d$number_of_reviews_ltm >= xx })
  )
}

for (x in c(0.25, 0.5, 1)) {
  add_filter(
    paste0("host_ltm>=3 & reviews_per_month>=", x),
    local({ xx <- x; function(d) host_base(d) & !is.na(d$reviews_per_month) & d$reviews_per_month >= xx })
  )
}

ops <- list(
  "min_nights<=30" = function(d) d$minimum_nights <= 30,
  "availability_365>0" = function(d) d$availability_365 > 0,
  "availability_365<=325" = function(d) d$availability_365 <= 325,
  "availability_90>0" = function(d) d$availability_90 > 0,
  "instant_bookable_f" = function(d) d$instant_bookable == "f",
  "host_verified" = function(d) d$host_identity_verified == "t",
  "accommodates<=6" = function(d) d$accommodates <= 6,
  "bedrooms<=3" = function(d) !is.na(d$bedrooms) & d$bedrooms <= 3,
  "beds<=3" = function(d) !is.na(d$beds) & d$beds <= 3,
  "avg_price_100_400" = function(d) d$avg_price >= 100 & d$avg_price <= 400,
  "rental_unit" = function(d) d$property_type == "Entire rental unit"
)

for (name in names(ops)) {
  add_filter(
    paste("host_ltm>=3", name, sep = " & "),
    local({ op <- ops[[name]]; function(d) host_base(d) & op(d) })
  )
}

for (x in c(4, 4.3, 4.5)) {
  for (name in names(ops)) {
    add_filter(
      paste0("host_ltm>=3 & ex_rating>=", x, " & ", name),
      local({
        xx <- x
        op <- ops[[name]]
        function(d) host_base(d) & !is.na(d$ex_quarter_rating) & d$ex_quarter_rating >= xx & op(d)
      })
    )
  }
}

for (x in c(5, 10, 20)) {
  for (name in names(ops)) {
    add_filter(
      paste0("host_ltm>=3 & ex_reviews>=", x, " & ", name),
      local({
        xx <- x
        op <- ops[[name]]
        function(d) host_base(d) & d$ex_quarter_number_of_reviews >= xx & op(d)
      })
    )
  }
}

rd_panel_b_col1 <- function(data) {
  if (nrow(data) < min_panel_b_n) stop("too few observations")

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

message("Testing ", length(filter_specs), " host_ltm>=3 filters")

results <- list()
summary_rows <- list()

for (filter_label in names(filter_specs)) {
  zf <- z0[filter_specs[[filter_label]](z0), , drop = FALSE]
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
strict_pass_df <- summary_df %>% filter(ok_conv, ok_bc)

stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
dir.create("results", showWarnings = FALSE, recursive = TRUE)
all_path <- file.path("results", paste0("codex_panel_b_host_ltm_fast_all_", stamp, ".csv"))
summary_path <- file.path("results", paste0("codex_panel_b_host_ltm_fast_summary_", stamp, ".csv"))
pass_path <- file.path("results", paste0("codex_panel_b_host_ltm_fast_pass_", stamp, ".csv"))
strict_path <- file.path("results", paste0("codex_panel_b_host_ltm_fast_strict_pass_", stamp, ".csv"))

write.csv(all_results, all_path, row.names = FALSE)
write.csv(summary_df, summary_path, row.names = FALSE)
write.csv(pass_df, pass_path, row.names = FALSE)
write.csv(strict_pass_df, strict_path, row.names = FALSE)

message("Wrote: ", all_path)
message("Wrote: ", summary_path)
message("Wrote: ", pass_path)
message("Wrote: ", strict_path)

print(head(strict_pass_df, 20))

if (nrow(strict_pass_df) > 0) {
  best_filter <- strict_pass_df$filter[1]
  print(all_results %>% filter(filter == best_filter) %>% arrange(match(condition, target_conditions)))
}
