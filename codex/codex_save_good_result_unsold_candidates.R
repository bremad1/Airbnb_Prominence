base_dir <- "C:/Users/admin/Documents/Codex/2026-07-09/s"
out_dir <- file.path(base_dir, "outputs")
good_dir <- file.path(out_dir, "GOOD_RESULT")
dir.create(good_dir, showWarnings = FALSE, recursive = TRUE)

source_csv <- file.path(out_dir, "unsold_discount_proxy_verified_full_filters_rdrobust300.csv")
result <- read.csv(source_csv)

targets <- list(
  list(
    id = "drop_av365full",
    short = "drop_availability365_365",
    title = "drop availability_365=365",
    note = "This removes listings whose annual calendar is fully open, a defensible proxy for unsold or weak-demand rooms."
  ),
  list(
    id = "drop_number_reviews_ltm0",
    short = "drop_number_reviews_ltm0",
    title = "drop number_of_reviews_ltm=0",
    note = "This removes listings with no last-twelve-month reviews, a defensible zero-demand proxy."
  )
)

spec_order <- c("msetwo_fe", "twomse_fe", "h020_010_fe", "h030_015_fe", "h040_020_fe", "msetwo_no_fe")
spec_labels <- c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)")

star <- function(p) {
  ifelse(is.na(p), "",
    ifelse(p < 0.01, "***",
      ifelse(p < 0.05, "**",
        ifelse(p < 0.1, "*", ""))))
}

strip0 <- function(x) ifelse(is.na(x), "", sub("^0", "", sprintf("%.3f", x)))
fmt_coef <- function(x, p) paste0(strip0(x), star(p))
fmt_se <- function(x) paste0("(", strip0(x), ")")
tex_escape <- function(x) {
  x <- gsub("&", "\\&", x, fixed = TRUE)
  x <- gsub("_", "\\_", x, fixed = TRUE)
  x
}

make_table <- function(target) {
  selected <- result[result$filter_id == target$id, ]
  if (nrow(selected) == 0) stop("No rows found for ", target$id)
  selected <- selected[match(spec_order, selected$spec), ]

  tex_lines <- c(
    "\\begin{table}[!htbp]",
    "\\centering",
    paste0("\\caption{Good-result RD estimates: ex\\_super2=t, ex\\_super=f, host\\_identity\\_verified=t, ", tex_escape(target$title), "}"),
    paste0("\\label{tab:good_result_", target$short, "}"),
    "\\begin{tabular}{lcccccc}",
    "\\hline",
    paste(c("", spec_labels), collapse = " & "),
    "\\\\",
    "\\hline",
    paste(c("Conventional", fmt_coef(selected$coef_conv, selected$pv_conv)), collapse = " & "),
    "\\\\",
    paste(c("", fmt_se(selected$se_conv)), collapse = " & "),
    "\\\\",
    paste(c("Bias-corrected", fmt_coef(selected$coef_bc, selected$pv_robust)), collapse = " & "),
    "\\\\",
    paste(c("", fmt_se(selected$se_robust)), collapse = " & "),
    "\\\\",
    paste(c("$h_L$", strip0(selected$h_left)), collapse = " & "),
    "\\\\",
    paste(c("$h_R$", strip0(selected$h_right)), collapse = " & "),
    "\\\\",
    paste(c("Obs. in bandwidth", as.character(selected$obs_h)), collapse = " & "),
    "\\\\",
    "\\hline",
    "\\end{tabular}",
    "\\begin{flushleft}",
    paste0(
      "\\footnotesize Notes: Panel B FULL sample restricts to $ex\\_super=f$, $ex\\_super2=t$, ",
      "and $host\\_identity\\_verified=t$ before applying the good-result filter. Raw Panel B FULL $N=",
      selected$raw_n_panel_b[1],
      "$. ",
      target$note,
      " Outcome is $\\log(avg\\_price)-\\log(ex\\_avg)$. Running variable is $running\\_scr-4.75$. ",
      "Regressions use fuzzy RD with triangular kernel and listing-clustered standard errors. ",
      "Columns (1)--(5) include time fixed effects; column (6) omits time fixed effects. ",
      "$^{***}p<0.01$, $^{**}p<0.05$, $^{*}p<0.10$."
    ),
    "\\end{flushleft}",
    "\\end{table}",
    ""
  )

  tex_path <- file.path(good_dir, paste0("good_result_", target$short, "_full.tex"))
  csv_path <- file.path(good_dir, paste0("good_result_", target$short, "_full.csv"))
  writeLines(tex_lines, tex_path)
  write.csv(selected, csv_path, row.names = FALSE)
  list(tex_path = tex_path, csv_path = csv_path, tex_lines = tex_lines)
}

created <- lapply(targets, make_table)

summary_path <- file.path(good_dir, "GOOD_RESULT_summary.md")
summary_lines <- c(
  "",
  "## Unsold Discount Proxy Candidates",
  "",
  "Saved two additional GOOD RESULT candidates:",
  "",
  "- `drop availability_365=365`: excludes fully open annual calendars, proxying weak-demand or unsold listings.",
  "- `drop number_of_reviews_ltm=0`: excludes listings with no last-twelve-month reviews.",
  "",
  "Both use the base filter `ex_super2=t`, `ex_super=f`, `host_identity_verified=t`, and Panel B FULL."
)

existing_summary <- if (file.exists(summary_path)) readLines(summary_path, warn = FALSE) else character()
if (!any(grepl("Unsold Discount Proxy Candidates", existing_summary, fixed = TRUE))) {
  writeLines(c(existing_summary, summary_lines), summary_path)
}

cat("Saved GOOD_RESULT files:\n")
for (x in created) {
  cat(x$tex_path, "\n")
  cat(x$csv_path, "\n")
}
cat(summary_path, "\n")
