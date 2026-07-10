base_dir <- "C:/Users/admin/Documents/Codex/2026-07-09/s"
project_root <- "C:/Users/admin/Documents/Codex/2026-07-07/rlt/work/Airbnb_Prominence"
lib_300 <- file.path(base_dir, "work", "rdrobust_versions", "rdrobust_3_0_0")
deps_lib <- "C:/Users/admin/Documents/Codex/2026-07-07/rlt/work/Rlibs"
out_dir <- file.path(base_dir, "outputs")

.libPaths(c(lib_300, deps_lib, .libPaths()))
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages(library(rdrobust))

load(file.path(project_root, "Quarterly_dataset1.RData"))

if (as.character(packageVersion("rdrobust")) != "3.0.0") {
  stop("Expected rdrobust 3.0.0, got ", packageVersion("rdrobust"))
}

quarters <- list(
  Q323 = Q323,
  Q423 = Q423,
  Q124 = Q124,
  Q224 = Q224,
  Q324 = Q324,
  Q424 = Q424
)

rd_call <- function(data, h = NULL) {
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

specs_from_primary <- function(data, est1) {
  list(
    msetwo = est1,
    twomse = rd_call(data, h = c(2 * est1[["bws"]][1, 1], 2 * est1[["bws"]][1, 2])),
    h020_010 = rd_call(data, h = c(0.2, 0.1)),
    h030_015 = rd_call(data, h = c(0.3, 0.15)),
    h040_020 = rd_call(data, h = c(0.4, 0.2))
  )
}

rows <- list()
count_rows <- list()

for (quarter_name in names(quarters)) {
  q <- quarters[[quarter_name]]
  keep <- q$ex_super == "f"
  keep[is.na(keep)] <- FALSE
  data <- q[keep, , drop = FALSE]

  count_rows[[quarter_name]] <- data.frame(
    quarter = quarter_name,
    raw_n = nrow(q),
    panel_b_n_ex_super_f = nrow(data),
    ex_super2_t_n = sum(data$ex_super2 == "t", na.rm = TRUE),
    ex_super2_not_t_n = sum(!(data$ex_super2 == "t") & !is.na(data$ex_super2), na.rm = TRUE),
    avg_price_missing = sum(is.na(data$avg_price)),
    ex_avg_missing = sum(is.na(data$ex_avg)),
    stringsAsFactors = FALSE
  )

  ests <- tryCatch({
    est1 <- rd_call(data)
    specs_from_primary(data, est1)
  }, error = function(e) e)

  if (inherits(ests, "error")) {
    for (spec_name in c("msetwo", "twomse", "h020_010", "h030_015", "h040_020")) {
      rows[[paste(quarter_name, spec_name, sep = "__")]] <- data.frame(
        quarter = quarter_name,
        spec = spec_name,
        raw_n_panel_b = nrow(data),
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
    rows[[paste(quarter_name, spec_name, sep = "__")]] <- data.frame(
      quarter = quarter_name,
      spec = spec_name,
      raw_n_panel_b = nrow(data),
      extract(ests[[spec_name]]),
      error = "",
      stringsAsFactors = FALSE
    )
  }
}

result <- do.call(rbind, rows)
counts <- do.call(rbind, count_rows)

write.csv(result, file.path(out_dir, "panel_b_quarter_rdrobust_no_exsuper2_all_specs.csv"), row.names = FALSE)
write.csv(counts, file.path(out_dir, "panel_b_quarter_rdrobust_no_exsuper2_counts.csv"), row.names = FALSE)

primary <- result[result$spec == "msetwo", ]
write.csv(primary, file.path(out_dir, "panel_b_quarter_rdrobust_no_exsuper2_msetwo.csv"), row.names = FALSE)

star <- function(p) {
  ifelse(is.na(p), "",
    ifelse(p < 0.01, "***",
      ifelse(p < 0.05, "**",
        ifelse(p < 0.1, "*", ""))))
}
strip0 <- function(x) ifelse(is.na(x), "", sub("^0", "", sprintf("%.3f", x)))

tex_lines <- c(
  "\\begin{tabular}{lccccc}",
  "\\hline",
  "Quarter & N & Conventional & Bias-corrected & $h_L$ & $h_R$ \\\\",
  "\\hline"
)

for (i in seq_len(nrow(primary))) {
  r <- primary[i, ]
  tex_lines <- c(
    tex_lines,
    paste(c(
      r$quarter,
      r$raw_n_panel_b,
      paste0(strip0(r$coef_conv), star(r$pv_conv)),
      paste0(strip0(r$coef_bc), star(r$pv_robust)),
      strip0(r$h_left),
      strip0(r$h_right)
    ), collapse = " & "),
    "\\\\",
    paste(c("", "", paste0("(", strip0(r$se_conv), ")"), paste0("(", strip0(r$se_robust), ")"), "", ""), collapse = " & "),
    "\\\\"
  )
}

tex_lines <- c(tex_lines, "\\hline", "\\end{tabular}")
writeLines(tex_lines, file.path(out_dir, "panel_b_quarter_rdrobust_no_exsuper2_msetwo.tex"))

cat("rdrobust", as.character(packageVersion("rdrobust")), "\n")
cat("\nCounts:\n")
print(counts)
cat("\nPrimary msetwo quarter results:\n")
print(primary[, c("quarter", "raw_n_panel_b", "coef_conv", "se_conv", "pv_conv", "coef_bc", "se_robust", "pv_robust", "h_left", "h_right", "obs_h", "error")], row.names = FALSE)
