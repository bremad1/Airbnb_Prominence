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
      .groups = "drop"
    )
}) %>%
  bind_rows()

z <- bind_rows(lapply(names(quarter_list), function(qname) {
  quarter_list[[qname]] %>% mutate(quarter = qname)
})) %>%
  left_join(availability_sums, by = c("id", "quarter"))

z$date3 <- factor(z$Date)
z$date3_ym <- format(as.Date(z$date3), "%Y-%m")

conditions <- list(
  FULL = z$ex_q1 == 1 | z$ex_q2 == 1 | z$ex_q3 == 1 | z$ex_q4 == 1,
  Q1Q2 = z$ex_q1 == 1 | z$ex_q2 == 1,
  Q2Q3 = z$ex_q2 == 1 | z$ex_q3 == 1,
  Q3Q4 = z$ex_q3 == 1 | z$ex_q4 == 1
)
conditions <- lapply(conditions, function(x) {
  x[is.na(x)] <- FALSE
  x
})

match_host_listing_count <- !is.na(z$host_listings_count) &
  !is.na(z$calculated_host_listings_count) &
  z$host_listings_count == z$calculated_host_listings_count

match_host_total_listing_count <- !is.na(z$host_total_listings_count) &
  !is.na(z$calculated_host_listings_count) &
  z$host_total_listings_count == z$calculated_host_listings_count

filters <- list(
  baseline = list(
    label = "baseline",
    keep = rep(TRUE, nrow(z))
  ),
  drop_av365_sum0 = list(
    label = "drop quarter-sum availability_365=0",
    keep = !(z$availability_365_sum_q == 0)
  ),
  host_listings_eq_calculated = list(
    label = "host_listings_count = calculated_host_listings_count",
    keep = match_host_listing_count
  ),
  host_total_listings_eq_calculated = list(
    label = "host_total_listings_count = calculated_host_listings_count",
    keep = match_host_total_listing_count
  )
)

rd_call <- function(data, h = NULL, use_time_fe = TRUE) {
  args <- list(
    y = as.numeric(data$host_is_superhost2),
    x = data$running_scr - 4.75,
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
    fs_conv = as.numeric(est[["Estimate"]][1]),
    se_conv = as.numeric(est[["se"]][1]),
    pv_conv = as.numeric(est[["pv"]][1]),
    fs_bc = as.numeric(est[["Estimate"]][2]),
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

for (panel_type in c("t", "f")) {
  panel_id <- ifelse(panel_type == "t", "panel_a", "panel_b")
  panel_keep <- z$ex_super == panel_type & z$ex_super2 == "t"
  panel_keep[is.na(panel_keep)] <- FALSE

  for (condition_name in names(conditions)) {
    condition_keep <- conditions[[condition_name]]

    for (filter_id in names(filters)) {
      filter_keep <- filters[[filter_id]]$keep
      filter_keep[is.na(filter_keep)] <- FALSE

      sample_keep <- panel_keep & condition_keep
      keep <- sample_keep & filter_keep
      data <- z[keep, , drop = FALSE]

      count_key <- paste(panel_id, condition_name, filter_id, sep = "__")
      counts[[count_key]] <- data.frame(
        panel = panel_id,
        ex_super = panel_type,
        condition = condition_name,
        filter_id = filter_id,
        filter = filters[[filter_id]]$label,
        raw_n = sum(sample_keep),
        kept_n = nrow(data),
        dropped_n = sum(sample_keep & !filter_keep),
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
          row_key <- paste(panel_id, condition_name, filter_id, spec_name, sep = "__")
          rows[[row_key]] <- data.frame(
            panel = panel_id,
            ex_super = panel_type,
            condition = condition_name,
            filter_id = filter_id,
            filter = filters[[filter_id]]$label,
            raw_n = nrow(data),
            spec = spec_name,
            fs_conv = NA_real_,
            se_conv = NA_real_,
            pv_conv = NA_real_,
            fs_bc = NA_real_,
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
        row_key <- paste(panel_id, condition_name, filter_id, spec_name, sep = "__")
        rows[[row_key]] <- data.frame(
          panel = panel_id,
          ex_super = panel_type,
          condition = condition_name,
          filter_id = filter_id,
          filter = filters[[filter_id]]$label,
          raw_n = nrow(data),
          spec = spec_name,
          extract(ests[[spec_name]]),
          error = "",
          stringsAsFactors = FALSE
        )
      }
    }
  }
}

result <- bind_rows(rows)
count_result <- bind_rows(counts)

write.csv(result, file.path(out_dir, "first_stage_rdrobust300.csv"), row.names = FALSE)
write.csv(count_result, file.path(out_dir, "first_stage_counts.csv"), row.names = FALSE)

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
  "\\begin{tabular}{lllcccccc}",
  "\\hline",
  "Panel & Condition & Filter & (1) & (2) & (3) & (4) & (5) & (6) \\\\",
  "\\hline"
)

for (panel_id in c("panel_a", "panel_b")) {
  for (condition_name in names(conditions)) {
    for (filter_id in names(filters)) {
      sub <- result[result$panel == panel_id & result$condition == condition_name & result$filter_id == filter_id, ]
      sub <- sub[match(spec_names, sub$spec), ]
      count_row <- count_result[count_result$panel == panel_id & count_result$condition == condition_name & count_result$filter_id == filter_id, ]
      vals <- paste0(strip0(sub$fs_conv), star(sub$pv_conv))
      ses <- paste0("(", strip0(sub$se_conv), ")")
      tex_lines <- c(
        tex_lines,
        paste(c(tex_escape(panel_id), condition_name, paste0(tex_escape(filters[[filter_id]]$label), " (N=", count_row$kept_n, ")"), vals), collapse = " & "),
        "\\\\",
        paste(c("", "", "", ses), collapse = " & "),
        "\\\\"
      )
    }
  }
}

tex_lines <- c(tex_lines, "\\hline", "\\end{tabular}")
writeLines(tex_lines, file.path(out_dir, "first_stage_rdrobust300.tex"))

cat("rdrobust", as.character(packageVersion("rdrobust")), "\n")
cat("\nCounts:\n")
print(count_result)
cat("\nResults:\n")
print(result[, c("panel", "condition", "filter_id", "raw_n", "spec", "fs_conv", "se_conv", "pv_conv", "fs_bc", "pv_robust", "obs_h", "error")], row.names = FALSE)
