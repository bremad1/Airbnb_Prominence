library(dplyr)
library(rdrobust)

setwd("C:/Users/brema/iCloudDrive/4-1/Airbnb/Test")
load("Quarterly_dataset1.RData")

target_conditions <- c("FULL", "Q1Q2", "Q2Q3", "Q3Q4")

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

num_var <- function(d, name) suppressWarnings(as.numeric(d[[name]]))

z0 <- rbind(Q323, Q423, Q124, Q224, Q324, Q424)
z0 <- add_ex_quarter_host_ltm(z0)
z0$date3 <- factor(z0$Date)
z0$date3_ym <- format(as.Date(z0$date3), "%Y-%m")

z0 <- z0 %>%
  filter(
    as.character(ex_super2) == "t",
    !is.na(running_scr),
    !is.na(host_is_superhost2),
    !is.na(id),
    !is.na(avg_price),
    !is.na(ex_avg),
    !is.na(ex_quarter_ltm),
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

filters <- list()
add_filter <- function(label, fn) {
  filters[[label]] <<- fn
}

base_filters <- list(
  "ex_super2=t & host_ltm>=3" = function(d) d$ex_quarter_host_ltm >= 3,
  "ex_super2=t & host_ltm>=5" = function(d) d$ex_quarter_host_ltm >= 5,
  "ex_super2=t & host_ltm>=10" = function(d) d$ex_quarter_host_ltm >= 10,
  "ex_super2=t & ltm>=3" = function(d) d$ex_quarter_ltm >= 3,
  "ex_super2=t & ltm>=5" = function(d) d$ex_quarter_ltm >= 5,
  "ex_super2=t & ltm>=10" = function(d) d$ex_quarter_ltm >= 10
)

stabilizers <- list(
  "ex_reviews>=5" = function(d) !is.na(d$ex_quarter_number_of_reviews) & d$ex_quarter_number_of_reviews >= 5,
  "ex_reviews>=10" = function(d) !is.na(d$ex_quarter_number_of_reviews) & d$ex_quarter_number_of_reviews >= 10,
  "ex_reviews>=20" = function(d) !is.na(d$ex_quarter_number_of_reviews) & d$ex_quarter_number_of_reviews >= 20,
  "ex_rating>=4.3" = function(d) !is.na(d$ex_quarter_rating) & d$ex_quarter_rating >= 4.3,
  "ex_rating>=4.5" = function(d) !is.na(d$ex_quarter_rating) & d$ex_quarter_rating >= 4.5,
  "ex_rating>=4.7" = function(d) !is.na(d$ex_quarter_rating) & d$ex_quarter_rating >= 4.7,
  "min_nights<=7" = function(d) !is.na(d$minimum_nights) & d$minimum_nights <= 7,
  "min_nights<=14" = function(d) !is.na(d$minimum_nights) & d$minimum_nights <= 14,
  "min_nights<=30" = function(d) !is.na(d$minimum_nights) & d$minimum_nights <= 30,
  "accommodates<=2" = function(d) !is.na(d$accommodates) & d$accommodates <= 2,
  "accommodates<=4" = function(d) !is.na(d$accommodates) & d$accommodates <= 4,
  "accommodates<=6" = function(d) !is.na(d$accommodates) & d$accommodates <= 6
)

if ("beds" %in% names(z0)) {
  stabilizers[["beds<=2"]] <- function(d) !is.na(num_var(d, "beds")) & num_var(d, "beds") <= 2
  stabilizers[["beds<=3"]] <- function(d) !is.na(num_var(d, "beds")) & num_var(d, "beds") <= 3
}
if ("bedrooms" %in% names(z0)) {
  stabilizers[["bedrooms<=1"]] <- function(d) !is.na(num_var(d, "bedrooms")) & num_var(d, "bedrooms") <= 1
  stabilizers[["bedrooms<=2"]] <- function(d) !is.na(num_var(d, "bedrooms")) & num_var(d, "bedrooms") <= 2
}
if ("bathrooms" %in% names(z0)) {
  stabilizers[["bathrooms<=1"]] <- function(d) !is.na(num_var(d, "bathrooms")) & num_var(d, "bathrooms") <= 1
  stabilizers[["bathrooms<=2"]] <- function(d) !is.na(num_var(d, "bathrooms")) & num_var(d, "bathrooms") <= 2
}
if ("room_type" %in% names(z0)) {
  stabilizers[["room_type=Entire home/apt"]] <- function(d) as.character(d$room_type) == "Entire home/apt"
  stabilizers[["room_type=Private room"]] <- function(d) as.character(d$room_type) == "Private room"
}
if ("instant_bookable" %in% names(z0)) {
  stabilizers[["instant_bookable=t"]] <- function(d) as.character(d$instant_bookable) == "t"
  stabilizers[["instant_bookable=f"]] <- function(d) as.character(d$instant_bookable) == "f"
}
if ("host_identity_verified" %in% names(z0)) {
  stabilizers[["host_identity_verified=t"]] <- function(d) as.character(d$host_identity_verified) == "t"
  stabilizers[["host_identity_verified=f"]] <- function(d) as.character(d$host_identity_verified) == "f"
}
if ("host_response_time" %in% names(z0)) {
  stabilizers[["host_response_time=within an hour"]] <- function(d) as.character(d$host_response_time) == "within an hour"
  stabilizers[["host_response_time=within a few hours"]] <- function(d) as.character(d$host_response_time) == "within a few hours"
}
if ("review_scores_cleanliness" %in% names(z0)) {
  stabilizers[["cleanliness>=4.5"]] <- function(d) !is.na(d$review_scores_cleanliness) & d$review_scores_cleanliness >= 4.5
}
if ("review_scores_value" %in% names(z0)) {
  stabilizers[["value>=4.5"]] <- function(d) !is.na(d$review_scores_value) & d$review_scores_value >= 4.5
}
if ("review_scores_location" %in% names(z0)) {
  stabilizers[["location>=4.5"]] <- function(d) !is.na(d$review_scores_location) & d$review_scores_location >= 4.5
}
if ("property_type" %in% names(z0)) {
  property_levels <- names(sort(table(z0$property_type), decreasing = TRUE))[1:min(4, length(table(z0$property_type)))]
  for (property_level in property_levels) {
    stabilizers[[paste0("property_type=", property_level)]] <- local({
      level <- property_level
      function(d) as.character(d$property_type) == level
    })
  }
}

make_filter <- function(base_fn, stab_names) {
  local({
    base_local <- base_fn
    stabs_local <- stabilizers[stab_names]
    function(d) {
      keep <- base_local(d)
      for (stab_fn in stabs_local) {
        keep <- keep & stab_fn(d)
      }
      keep
    }
  })
}

for (base_name in names(base_filters)) {
  add_filter(base_name, base_filters[[base_name]])
  for (stab_name in names(stabilizers)) {
    add_filter(paste(base_name, stab_name, sep = " & "), make_filter(base_filters[[base_name]], stab_name))
  }
}

rating_names <- grep("^ex_rating", names(stabilizers), value = TRUE)
review_names <- grep("^ex_reviews", names(stabilizers), value = TRUE)
shape_names <- grep("^(min_nights|accommodates|beds|bedrooms|bathrooms|room_type|instant_bookable|host_identity_verified|host_response_time|property_type|cleanliness|value|location)", names(stabilizers), value = TRUE)

pair_names <- list()
for (a in rating_names) {
  for (b in c(review_names, shape_names)) {
    pair_names[[length(pair_names) + 1]] <- c(a, b)
  }
}
for (a in review_names) {
  for (b in shape_names) {
    pair_names[[length(pair_names) + 1]] <- c(a, b)
  }
}

for (base_name in names(base_filters)) {
  for (pair in pair_names) {
    add_filter(
      paste(base_name, pair[1], pair[2], sep = " & "),
      make_filter(base_filters[[base_name]], pair)
    )
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

message("Testing ", length(filters), " single-ltm/no-availability filters")

results <- list()
summary_rows <- list()

for (filter_label in names(filters)) {
  keep <- filters[[filter_label]](z0)
  keep[is.na(keep)] <- FALSE
  zf <- z0[keep, , drop = FALSE]
  out <- list()
  skip <- FALSE

  for (condition_name in target_conditions) {
    data_b <- zf[condition_fns[[condition_name]](zf) & zf$ex_super == "f", , drop = FALSE]
    if (nrow(data_b) == 0) {
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
    ok_conv = all(df$coef_conv < 0 & abs(df$coef_conv) < 0.1),
    ok_bc = all(df$coef_bc < 0 & abs(df$coef_bc) < 0.1),
    all_negative_conv = all(df$coef_conv < 0),
    stringsAsFactors = FALSE
  )
}

all_results <- bind_rows(results)
summary_df <- bind_rows(summary_rows)
if (nrow(summary_df) > 0) {
  summary_df <- summary_df %>%
    arrange(desc(ok_conv), desc(ok_bc), max_abs_conv, max_abs_bc, desc(min_n_panel_b))
  pass_df <- summary_df %>% filter(ok_conv)
  strict_df <- summary_df %>% filter(ok_conv, ok_bc)
  near_df <- summary_df %>% filter(all_negative_conv, max_abs_conv < 0.15)
} else {
  pass_df <- summary_df
  strict_df <- summary_df
  near_df <- summary_df
}

stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
dir.create("results", showWarnings = FALSE, recursive = TRUE)
all_path <- file.path("results", paste0("codex_panel_b_single_ltm_noavail_all_", stamp, ".csv"))
summary_path <- file.path("results", paste0("codex_panel_b_single_ltm_noavail_summary_", stamp, ".csv"))
pass_path <- file.path("results", paste0("codex_panel_b_single_ltm_noavail_pass_", stamp, ".csv"))
strict_path <- file.path("results", paste0("codex_panel_b_single_ltm_noavail_strict_pass_", stamp, ".csv"))
near_path <- file.path("results", paste0("codex_panel_b_single_ltm_noavail_near_", stamp, ".csv"))

write.csv(all_results, all_path, row.names = FALSE)
write.csv(summary_df, summary_path, row.names = FALSE)
write.csv(pass_df, pass_path, row.names = FALSE)
write.csv(strict_df, strict_path, row.names = FALSE)
write.csv(near_df, near_path, row.names = FALSE)

message("Wrote: ", all_path)
message("Wrote: ", summary_path)
message("Wrote: ", pass_path)
message("Wrote: ", strict_path)
message("Wrote: ", near_path)

print(head(summary_df, 30))
if (nrow(pass_df) > 0) {
  best_filter <- pass_df$filter[1]
  print(all_results %>% filter(filter == best_filter) %>% arrange(match(condition, target_conditions)))
}
