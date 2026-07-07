library(dplyr)
library(rdrobust)

setwd("C:/Users/brema/iCloudDrive/4-1/Airbnb/Test")
load("Quarterly_dataset1.RData")

min_panel_b_n <- 1000
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

base_filter <- list(
  label = "host_ltm>=3",
  fn = function(d) d$ex_quarter_host_ltm >= 3
)

atoms <- list()
add_atom <- function(label, fn, group) {
  atoms[[label]] <<- list(fn = fn, group = group)
}

for (x in c(4, 4.3, 4.5, 4.6, 4.7, 4.8)) {
  add_atom(
    paste0("ex_rating>=", x),
    local({ xx <- x; function(d) !is.na(d$ex_quarter_rating) & d$ex_quarter_rating >= xx }),
    "quality"
  )
}

for (x in c(4.5, 4.6, 4.7, 4.8, 4.9)) {
  add_atom(
    paste0("score_rating>=", x),
    local({ xx <- x; function(d) !is.na(d$review_scores_rating) & d$review_scores_rating >= xx }),
    "quality"
  )
  add_atom(
    paste0("score_clean>=", x),
    local({ xx <- x; function(d) !is.na(d$review_scores_cleanliness) & d$review_scores_cleanliness >= xx }),
    "quality"
  )
  add_atom(
    paste0("score_value>=", x),
    local({ xx <- x; function(d) !is.na(d$review_scores_value) & d$review_scores_value >= xx }),
    "quality"
  )
}

for (x in c(5, 10, 20, 30, 50)) {
  add_atom(
    paste0("ex_reviews>=", x),
    local({ xx <- x; function(d) d$ex_quarter_number_of_reviews >= xx }),
    "review_depth"
  )
}

for (x in c(10, 20, 50, 100)) {
  add_atom(
    paste0("total_reviews>=", x),
    local({ xx <- x; function(d) d$number_of_reviews >= xx }),
    "review_depth"
  )
}

for (x in c(1, 5, 10, 20)) {
  add_atom(
    paste0("reviews_ltm>=", x),
    local({ xx <- x; function(d) d$number_of_reviews_ltm >= xx }),
    "review_depth"
  )
}

for (x in c(0.25, 0.5, 1, 2)) {
  add_atom(
    paste0("reviews_per_month>=", x),
    local({ xx <- x; function(d) !is.na(d$reviews_per_month) & d$reviews_per_month >= xx }),
    "review_depth"
  )
}

add_atom("min_nights<=30", function(d) d$minimum_nights <= 30, "operations")
add_atom("min_nights==30", function(d) d$minimum_nights == 30, "operations")
add_atom("availability_365>0", function(d) d$availability_365 > 0, "operations")
add_atom("availability_365>=120", function(d) d$availability_365 >= 120, "operations")
add_atom("availability_365<=325", function(d) d$availability_365 <= 325, "operations")
add_atom("availability_90>0", function(d) d$availability_90 > 0, "operations")
add_atom("instant_bookable_f", function(d) d$instant_bookable == "f", "operations")
add_atom("instant_bookable_t", function(d) d$instant_bookable == "t", "operations")
add_atom("host_verified", function(d) d$host_identity_verified == "t", "operations")
add_atom("response_hour", function(d) d$host_response_time == "within an hour", "operations")
add_atom("response_hour_or_few", function(d) d$host_response_time %in% c("within an hour", "within a few hours"), "operations")

for (x in c(2, 4, 6)) {
  add_atom(
    paste0("accommodates<=", x),
    local({ xx <- x; function(d) d$accommodates <= xx }),
    "listing"
  )
}

for (x in c(1, 2, 3)) {
  add_atom(
    paste0("bedrooms<=", x),
    local({ xx <- x; function(d) !is.na(d$bedrooms) & d$bedrooms <= xx }),
    "listing"
  )
  add_atom(
    paste0("beds<=", x),
    local({ xx <- x; function(d) !is.na(d$beds) & d$beds <= xx }),
    "listing"
  )
}

add_atom("rental_unit", function(d) d$property_type == "Entire rental unit", "listing")
add_atom("home_or_townhouse", function(d) d$property_type %in% c("Entire home", "Entire townhouse"), "listing")
add_atom("condo", function(d) d$property_type == "Entire condo", "listing")
add_atom("avg_price_100_400", function(d) d$avg_price >= 100 & d$avg_price <= 400, "price")
add_atom("avg_price_125_400", function(d) d$avg_price >= 125 & d$avg_price <= 400, "price")
add_atom("ex_avg_100_400", function(d) d$ex_avg >= 100 & d$ex_avg <= 400, "price")

make_filters <- function() {
  filters <- list()
  filters[[base_filter$label]] <- base_filter$fn

  for (name in names(atoms)) {
    filters[[paste(base_filter$label, name, sep = " & ")]] <- local({
      atom <- atoms[[name]]$fn
      function(d) base_filter$fn(d) & atom(d)
    })
  }

  quality_names <- names(atoms)[vapply(atoms, function(x) x$group %in% c("quality", "review_depth"), logical(1))]
  stabilizer_names <- names(atoms)[vapply(atoms, function(x) x$group %in% c("operations", "listing", "price"), logical(1))]

  for (q in quality_names) {
    for (s in stabilizer_names) {
      filters[[paste(base_filter$label, q, s, sep = " & ")]] <- local({
        fq <- atoms[[q]]$fn
        fs <- atoms[[s]]$fn
        function(d) base_filter$fn(d) & fq(d) & fs(d)
      })
    }
  }

  filters
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
    se_conv = as.numeric(est[["se"]][1]),
    se_robust = as.numeric(est[["se"]][3]),
    pv_conv = as.numeric(est[["pv"]][1]),
    pv_robust = as.numeric(est[["pv"]][3]),
    h_left = as.numeric(est[["bws"]][1, 1]),
    h_right = as.numeric(est[["bws"]][1, 2]),
    stringsAsFactors = FALSE
  )
}

filters <- make_filters()
message("Testing ", length(filters), " candidate filters")

results <- list()
passes <- list()
near_misses <- list()

for (filter_label in names(filters)) {
  zf <- z0[filters[[filter_label]](z0), , drop = FALSE]
  filter_results <- list()
  skip_filter <- FALSE

  for (condition_name in target_conditions) {
    keep <- condition_fns[[condition_name]](zf) & zf$ex_super == "f"
    data_b <- zf[keep, , drop = FALSE]
    n_b <- nrow(data_b)

    if (n_b < min_panel_b_n) {
      skip_filter <- TRUE
      break
    }

    est <- tryCatch(rd_panel_b_col1(data_b), error = function(e) NULL)
    if (is.null(est)) {
      skip_filter <- TRUE
      break
    }

    filter_results[[condition_name]] <- tidy_est(est, filter_label, condition_name, n_b)
  }

  if (skip_filter) next

  df <- bind_rows(filter_results)
  results[[length(results) + 1]] <- df

  ok_conv <- all(df$coef_conv < 0 & abs(df$coef_conv) < 0.1)
  ok_bc <- all(df$coef_bc < 0 & abs(df$coef_bc) < 0.1)
  max_abs_conv <- max(abs(df$coef_conv))
  max_abs_bc <- max(abs(df$coef_bc))

  summary_row <- data.frame(
    filter = filter_label,
    max_abs_conv = max_abs_conv,
    max_abs_bc = max_abs_bc,
    min_n_panel_b = min(df$n_panel_b),
    max_pv_robust = max(df$pv_robust),
    ok_conv = ok_conv,
    ok_bc = ok_bc,
    stringsAsFactors = FALSE
  )

  if (ok_conv) {
    passes[[length(passes) + 1]] <- summary_row
  } else if (all(df$coef_conv < 0) && max_abs_conv < 0.15) {
    near_misses[[length(near_misses) + 1]] <- summary_row
  }
}

all_results <- bind_rows(results)
pass_summary <- bind_rows(passes) %>%
  arrange(desc(ok_bc), max_abs_conv, max_abs_bc, desc(min_n_panel_b))
near_summary <- bind_rows(near_misses) %>%
  arrange(max_abs_conv, desc(min_n_panel_b))

stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
dir.create("results", showWarnings = FALSE, recursive = TRUE)
all_path <- file.path("results", paste0("codex_panel_b_host_ltm_all_", stamp, ".csv"))
pass_path <- file.path("results", paste0("codex_panel_b_host_ltm_pass_", stamp, ".csv"))
near_path <- file.path("results", paste0("codex_panel_b_host_ltm_near_", stamp, ".csv"))

write.csv(all_results, all_path, row.names = FALSE)
write.csv(pass_summary, pass_path, row.names = FALSE)
write.csv(near_summary, near_path, row.names = FALSE)

message("Wrote: ", all_path)
message("Wrote: ", pass_path)
message("Wrote: ", near_path)

print(head(pass_summary, 30))

if (nrow(pass_summary) > 0) {
  best_filter <- pass_summary$filter[1]
  print(all_results %>% filter(filter == best_filter) %>% arrange(match(condition, target_conditions)))
}
