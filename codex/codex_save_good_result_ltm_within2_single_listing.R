base_dir <- "C:/Users/admin/Documents/Codex/2026-07-09/s"
out_dir <- file.path(base_dir, "outputs")
good_dir <- file.path(out_dir, "GOOD_RESULT")
dir.create(good_dir, showWarnings = FALSE, recursive = TRUE)

result <- read.csv(file.path(out_dir, "ltm_scr_current_ltm_within2_rdrobust300.csv"), stringsAsFactors = FALSE)
counts <- read.csv(file.path(out_dir, "ltm_scr_current_ltm_within2_counts.csv"), stringsAsFactors = FALSE)

spec_names <- c("msetwo_fe", "twomse_fe", "h020_010_fe", "h030_015_fe", "h040_020_fe", "msetwo_no_fe")
spec_labels <- c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)")

good <- result[
  result$sample_id == "panel_b_no_ex2" &
    result$filter_id == "single_listing_current_ltm_within2",
]
good <- good[match(spec_names, good$spec), ]

good_count <- counts[
  counts$sample_id == "panel_b_no_ex2" &
    counts$filter_id == "single_listing_current_ltm_within2",
]

base_name <- "good_result_ltm_scr_current_ltm_within2_single_listing_no_exsuper2"

write.csv(good, file.path(good_dir, paste0(base_name, ".csv")), row.names = FALSE)

star <- function(p) {
  ifelse(is.na(p), "",
    ifelse(p < 0.01, "***",
      ifelse(p < 0.05, "**",
        ifelse(p < 0.1, "*", ""))))
}

strip0 <- function(x) ifelse(is.na(x), "--", sub("^0", "", sprintf("%.3f", x)))
fmt_coef <- function(x, p) paste0(strip0(x), star(p))
fmt_se <- function(x) ifelse(is.na(x), "--", paste0("(", strip0(x), ")"))

tex_lines <- c(
  "\\begin{tabular}{lcccccc}",
  "\\hline",
  paste(c("Outcome", spec_labels), collapse = " & "),
  "\\\\ \\hline",
  "\\multicolumn{7}{l}{\\textbf{Panel B: $Superhost_{t-1}=0$, no $ex\\_super2$ filter}} \\\\",
  paste0("\\multicolumn{7}{l}{Single-listing hosts with $|ltm\\_scr-number\\_of\\_reviews\\_ltm|\\leq 2$ (N=", good_count$kept_n, ")} \\\\"),
  "\\\\",
  paste(c("Price RD", fmt_coef(good$price_conv, good$price_pv_conv)), collapse = " & "),
  "\\\\",
  paste(c("", fmt_se(good$price_se_conv)), collapse = " & "),
  "\\\\",
  paste(c("First stage", fmt_coef(good$fs_conv, good$fs_pv_conv)), collapse = " & "),
  "\\\\",
  paste(c("", fmt_se(good$fs_se_conv)), collapse = " & "),
  "\\\\",
  "\\hline",
  "\\end{tabular}"
)

writeLines(tex_lines, file.path(good_dir, paste0(base_name, ".tex")))

summary_lines <- c(
  "# GOOD RESULT: LTM review-count matched Panel B",
  "",
  "Filter:",
  "`ex_super=f`; no `ex_super2` filter; keep single-listing host-quarter observations with `abs(ltm_scr - number_of_reviews_ltm) <= 2`.",
  "",
  paste0("Panel B FULL raw N: ", good_count$raw_n, "."),
  paste0("Kept N: ", good_count$kept_n, "."),
  paste0("Usable N: ", good_count$usable_n, "."),
  paste0("Unique hosts: ", good_count$unique_hosts, "."),
  "",
  "Conventional FULL estimates:",
  "",
  "| Spec | Price coef | Price SE | Price p | First stage | First-stage SE | First-stage p |",
  "|---|---:|---:|---:|---:|---:|---:|"
)

for (i in seq_along(spec_names)) {
  row_text <- paste(
    c(
      spec_labels[i],
      strip0(good$price_conv[i]),
      strip0(good$price_se_conv[i]),
      strip0(good$price_pv_conv[i]),
      strip0(good$fs_conv[i]),
      strip0(good$fs_se_conv[i]),
      strip0(good$fs_pv_conv[i])
    ),
    collapse = " | "
  )
  summary_lines <- c(summary_lines, paste0("| ", row_text, " |"))
}

summary_lines <- c(
  summary_lines,
  "",
  "Rationale:",
  "This keeps hosts for which the scraped host-level LTM review count is close to the Inside Airbnb LTM review count. The single-listing restriction makes the host-level scraped count equivalent to the listing-level count, avoiding multi-listing host coverage mismatch."
)

writeLines(summary_lines, file.path(good_dir, paste0(base_name, ".md")))

cat("Saved", base_name, "to", good_dir, "\n")
