x <- read.csv("outputs/first_stage_no_exsuper2_rdrobust300.csv")
specs <- c("msetwo_fe", "twomse_fe", "h020_010_fe", "h030_015_fe", "h040_020_fe", "msetwo_no_fe")
star <- function(p) {
  ifelse(is.na(p), "",
    ifelse(p < .01, "***",
      ifelse(p < .05, "**",
        ifelse(p < .1, "*", ""))))
}
fmt <- function(v, p) paste0(sprintf("%.3f", v), star(p))
sub <- subset(x, condition == "FULL")

for (pan in c("panel_a", "panel_b")) {
  cat("\n", pan, "\n", sep = "")
  for (fid in c("baseline", "drop_av365_sum0", "host_listings_eq_calculated", "host_total_listings_eq_calculated")) {
    r <- sub[sub$panel == pan & sub$filter_id == fid, ]
    r <- r[match(specs, r$spec), ]
    cat(fid, " N=", unique(r$raw_n), ": ", paste(fmt(r$fs_conv, r$pv_conv), collapse = " & "), "\n", sep = "")
    cat("se: ", paste(sprintf("(%.3f)", r$se_conv), collapse = " & "), "\n", sep = "")
  }
}
