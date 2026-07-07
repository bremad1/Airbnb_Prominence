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

add_stabilizer <- function(stabilizers, label, fn) {
  stabilizers[[label]] <- fn
  stabilizers
}

base_filters <- list(
  "ex_super2=t & host_ltm>=3" = function(d) d$ex_quarter_host_ltm >= 3,
  "ex_super2=t & host_ltm>=4" = function(d) d$ex_quarter_host_ltm >= 4,
  "ex_super2=t & host_ltm>=5" = function(d) d$ex_quarter_host_ltm >= 5,
  "ex_super2=t & host_ltm>=3 & listing_ltm>=2" = function(d) d$ex_quarter_host_ltm >= 3 & d$ex_quarter_ltm >= 2,
  "ex_super2=t & host_ltm>=3 & listing_ltm>=3" = function(d) d$ex_quarter_host_ltm >= 3 & d$ex_quarter_ltm >= 3,
  "ex_super2=t & host_ltm>=5 & listing_ltm>=3" = function(d) d$ex_quarter_host_ltm >= 5 & d$ex_quarter_ltm >= 3,
  "ex_super2=t & listing_ltm>=3" = function(d) d$ex_quarter_ltm >= 3
)

stabilizers <- list()
stabilizers <- add_stabilizer(stabilizers, "ex_reviews>=5", function(d) !is.na(d$ex_quarter_number_of_reviews) & d$ex_quarter_number_of_reviews >= 5)
stabilizers <- add_stabilizer(stabilizers, "ex_reviews>=10", function(d) !is.na(d$ex_quarter_number_of_reviews) & d$ex_quarter_number_of_reviews >= 10)
stabilizers <- add_stabilizer(stabilizers, "ex_reviews>=20", function(d) !is.na(d$ex_quarter_number_of_reviews) & d$ex_quarter_number_of_reviews >= 20)
stabilizers <- add_stabilizer(stabilizers, "ex_rating>=4.3", function(d) !is.na(d$ex_quarter_rating) & d$ex_quarter_rating >= 4.3)
stabilizers <- add_stabilizer(stabilizers, "ex_rating>=4.5", function(d) !is.na(d$ex_quarter_rating) & d$ex_quarter_rating >= 4.5)
stabilizers <- add_stabilizer(stabilizers, "min_nights<=7", function(d) !is.na(d$minimum_nights) & d$minimum_nights <= 7)
stabilizers <- add_stabilizer(stabilizers, "min_nights<=14", function(d) !is.na(d$minimum_nights) & d$minimum_nights <= 14)
stabilizers <- add_stabilizer(stabilizers, "min_nights<=30", function(d) !is.na(d$minimum_nights) & d$minimum_nights <= 30)
stabilizers <- add_stabilizer(stabilizers, "availability_365<=275", function(d) !is.na(d$availability_365) & d$availability_365 <= 275)
stabilizers <- add_stabilizer(stabilizers, "availability_365<=300", function(d) !is.na(d$availability_365) & d$availability_365 <= 300)
stabilizers <- add_stabilizer(stabilizers, "availability_365<=325", function(d) !is.na(d$availability_365) & d$availability_365 <= 325)
stabilizers <- add_stabilizer(stabilizers, "availability_365<=350", function(d) !is.na(d$availability_365) & d$availability_365 <= 350)
stabilizers <- add_stabilizer(stabilizers, "availability_90>0", function(d) !is.na(d$availability_90) & d$availability_90 > 0)
stabilizers <- add_stabilizer(stabilizers, "availability_90<=75", function(d) !is.na(d$availability_90) & d$availability_90 <= 75)
stabilizers <- add_stabilizer(stabilizers, "accommodates<=4", function(d) !is.na(d$accommodates) & d$accommodates <= 4)
stabilizers <- add_stabilizer(stabilizers, "accommodates<=6", function(d) !is.na(d$accommodates) & d$accommodates <= 6)

if ("room_type" %in% names(z0)) {
  stabilizers <- add_stabilizer(stabilizers, "room_type=Entire home/apt", function(d) as.character(d$room_type) == "Entire home/apt")
  stabilizers <- add_stabilizer(stabilizers, "room_type=Private room", function(d) as.character(d$room_type) == "Private room")
}
if ("instant_bookable" %in% names(z0)) {
  stabilizers <- add_stabilizer(stabilizers, "instant_bookable=t", function(d) as.character(d$instant_bookable) == "t")
  stabilizers <- add_stabilizer(stabilizers, "instant_bookable=f", function(d) as.character(d$instant_bookable) == "f")
}
if ("host_identity_verified" %in% names(z0)) {
  stabilizers <- add_stabilizer(stabilizers, "host_identity_verified=t", function(d) as.character(d$host_identity_verified) == "t")
}

property_levels <- character()
if ("property_type" %in% names(z0)) {
  property_levels <- names(sort(table(z0$property_type), decreasing = TRUE))[1:min(3, length(table(z0$property_type)))]
  for (property_level in property_levels) {
    stabilizers <- add_stabilizer(
      stabilizers,
      paste0("property_type=", property_level),
      local({
        level <- property_level
        function(d) as.character(d$property_type) == level
      })
    )
  }
}

for (base_name in names(base_filters)) {
  add_filter(base_name, base_filters[[base_name]])
  for (stab_name in names(stabilizers)) {
    add_filter(
      paste(base_name, stab_name, sep = " & "),
      local({
        base_fn <- base_filters[[base_name]]
        stab_fn <- stabilizers[[stab_name]]
        function(d) base_fn(d) & stab_fn(d)
      })
    )
  }
}

availability_stabs <- grep("^availability_365", names(stabilizers), value = TRUE)
pair_second_stabs <- setdiff(
  names(stabilizers),
  c(availability_stabs, "availability_90>0", "availability_90<=75")
)

for (base_name in names(base_filters)) {
  for (avail_name in availability_stabs) {
    for (stab_name in pair_second_stabs) {
      add_filter(
        paste(base_name, avail_name, stab_name, sep = " & "),
        local({
          base_fn <- base_filters[[base_name]]
          avail_fn <- stabilizers[[avail_name]]
          stab_fn <- stabilizers[[stab_name]]
          function(d) base_fn(d) & avail_fn(d) & stab_fn(d)
        })
      )
    }
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

message("Testing ", length(filters), " no-avg filters")

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
all_path <- file.path("results", paste0("codex_panel_b_noavg_all_", stamp, ".csv"))
summary_path <- file.path("results", paste0("codex_panel_b_noavg_summary_", stamp, ".csv"))
pass_path <- file.path("results", paste0("codex_panel_b_noavg_pass_", stamp, ".csv"))
strict_path <- file.path("results", paste0("codex_panel_b_noavg_strict_pass_", stamp, ".csv"))
near_path <- file.path("results", paste0("codex_panel_b_noavg_near_", stamp, ".csv"))

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
