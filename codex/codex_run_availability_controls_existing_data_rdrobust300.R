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

z <- rbind(Q323, Q423, Q124, Q224, Q324, Q424)
z$date3 <- factor(z$Date)
z$date3_ym <- format(as.Date(z$date3), "%Y-%m")

if (as.character(packageVersion("rdrobust")) != "3.0.0") {
  stop("Expected rdrobust 3.0.0, got ", packageVersion("rdrobust"))
}

full_condition <- z$ex_q1 == 1 | z$ex_q2 == 1 | z$ex_q3 == 1 | z$ex_q4 == 1
full_condition[is.na(full_condition)] <- FALSE

samples <- list(
  panel_b_ex2 = list(
    label = "Panel B: ex_super=f & ex_super2=t",
    keep = z$ex_super == "f" & z$ex_super2 == "t" & full_condition
  ),
  panel_b_ex2_verified = list(
    label = "Panel B: ex_super=f & ex_super2=t & host_identity_verified=t",
    keep = z$ex_super == "f" & z$ex_super2 == "t" & z$host_identity_verified == "t" & full_condition
  )
)

cov_builders <- list(
  time_fe = function(d) {
    mm <- as.data.frame(model.matrix(~ date3_ym - 1, data = d))
    mm[, -1, drop = FALSE]
  },
  time_fe_av365 = function(d) {
    cbind(cov_builders$time_fe(d), availability_365 = d$availability_365)
  },
  time_fe_av90 = function(d) {
    cbind(cov_builders$time_fe(d), availability_90 = d$availability_90)
  },
  time_fe_av90_av365 = function(d) {
    cbind(
      cov_builders$time_fe(d),
      availability_90 = d$availability_90,
      availability_365 = d$availability_365
    )
  },
  time_fe_av_extremes = function(d) {
    cbind(
      cov_builders$time_fe(d),
      av90_zero = as.integer(d$availability_90 == 0),
      av90_full = as.integer(d$availability_90 == 90),
      av365_zero = as.integer(d$availability_365 == 0),
      av365_full = as.integer(d$availability_365 == 365)
    )
  },
  time_fe_av90_av365_extremes = function(d) {
    cbind(
      cov_builders$time_fe(d),
      availability_90 = d$availability_90,
      availability_365 = d$availability_365,
      av90_zero = as.integer(d$availability_90 == 0),
      av90_full = as.integer(d$availability_90 == 90),
      av365_zero = as.integer(d$availability_365 == 0),
      av365_full = as.integer(d$availability_365 == 365)
    )
  }
)

rd_call <- function(data, covs = NULL) {
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
  if (!is.null(covs) && ncol(covs) > 0) args$covs <- cbind(covs)
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

rows <- list()

for (sample_id in names(samples)) {
  keep <- samples[[sample_id]]$keep
  keep[is.na(keep)] <- FALSE
  data0 <- z[keep, , drop = FALSE]

  for (cov_id in names(cov_builders)) {
    covs0 <- cov_builders[[cov_id]](data0)
    complete <- complete.cases(covs0)
    data <- data0[complete, , drop = FALSE]
    covs <- covs0[complete, , drop = FALSE]

    est <- tryCatch(rd_call(data, covs), error = function(e) e)

    if (inherits(est, "error")) {
      rows[[paste(sample_id, cov_id, sep = "__")]] <- data.frame(
        sample_id = sample_id,
        sample = samples[[sample_id]]$label,
        covariates = cov_id,
        raw_n = nrow(data0),
        effective_n = nrow(data),
        coef_conv = NA_real_,
        se_conv = NA_real_,
        pv_conv = NA_real_,
        coef_bc = NA_real_,
        se_robust = NA_real_,
        pv_robust = NA_real_,
        h_left = NA_real_,
        h_right = NA_real_,
        obs_h = NA_integer_,
        error = est$message,
        stringsAsFactors = FALSE
      )
    } else {
      rows[[paste(sample_id, cov_id, sep = "__")]] <- data.frame(
        sample_id = sample_id,
        sample = samples[[sample_id]]$label,
        covariates = cov_id,
        raw_n = nrow(data0),
        effective_n = nrow(data),
        extract(est),
        error = "",
        stringsAsFactors = FALSE
      )
    }
  }
}

result <- bind_rows(rows)
write.csv(result, file.path(out_dir, "availability_controls_existing_data_full_rdrobust300.csv"), row.names = FALSE)

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
  "\\begin{tabular}{llcccc}",
  "\\hline",
  "Sample & Covariates & N & Conventional & Bias-corrected & Bandwidth \\\\",
  "\\hline"
)

for (i in seq_len(nrow(result))) {
  r <- result[i, ]
  tex_lines <- c(
    tex_lines,
    paste(c(
      tex_escape(r$sample_id),
      tex_escape(r$covariates),
      r$effective_n,
      paste0(strip0(r$coef_conv), star(r$pv_conv)),
      paste0(strip0(r$coef_bc), star(r$pv_robust)),
      paste0("[", strip0(r$h_left), ", ", strip0(r$h_right), "]")
    ), collapse = " & "),
    "\\\\",
    paste(c("", "", "", paste0("(", strip0(r$se_conv), ")"), paste0("(", strip0(r$se_robust), ")"), ""), collapse = " & "),
    "\\\\"
  )
}

tex_lines <- c(tex_lines, "\\hline", "\\end{tabular}")
writeLines(tex_lines, file.path(out_dir, "availability_controls_existing_data_full_rdrobust300.tex"))

cat("rdrobust", as.character(packageVersion("rdrobust")), "\n")
print(result[, c("sample_id", "covariates", "effective_n", "coef_conv", "se_conv", "pv_conv", "coef_bc", "pv_robust", "h_left", "h_right", "obs_h", "error")], row.names = FALSE)
