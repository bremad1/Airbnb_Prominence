runrd <- function(data, level) {
  conditions <- list(
    "FULL" = (z$ex_q1 == 1 | z$ex_q2 == 1 | z$ex_q3 == 1 | z$ex_q4 == 1),
    "Q1Q2" = (z$ex_q1 == 1 | z$ex_q2 == 1),
    "Q2Q3" = (z$ex_q3 == 1 | z$ex_q2 == 1),
    "Q3Q4" = (z$ex_q3 == 1 | z$ex_q4 == 1),
    "Q1" = (z$ex_q1 == 1),
    "Q2" = (z$ex_q2 == 1),
    "Q3" = (z$ex_q3 == 1),
    "Q4" = (z$ex_q4 == 1)
  )
  z$date3_ym <- format(as.Date(z$date3), "%Y-%m")

  for (super_type in c("f", "t")) {
    for (condition_name in names(conditions)) {
      condition <- conditions[[condition_name]]

      filtered_data <- z %>%
        filter(ex_super == super_type & condition)

      if (nrow(filtered_data) > 0) {
        dummy_vars <- as.data.frame(model.matrix(~ date3_ym - 1, data = filtered_data))
        covs <- NULL
        if (ncol(dummy_vars) > 1) {
          covs <- as.matrix(dummy_vars[, -1, drop = FALSE])
        }

        margin <- filtered_data$running_scr - 4.75
        result_name <- paste(pct, super_type, condition_name, sep = "_")
        if (level == TRUE) {
          y <- log(filtered_data$avg_price)
        } else {
          y <- log(filtered_data$avg_price) - log(filtered_data$ex_avg)
        }

        tryCatch({
          est1 <- rdrobust(
            y = y,
            x = margin,
            fuzzy = filtered_data$host_is_superhost2,
            covs = covs,
            all = TRUE,
            cluster = filtered_data$id,
            kernel = "tri",
            bwselect = "msetwo",
            p = 1,
            masspoints = "off",
            bwrestrict = TRUE
          )

          if (level == TRUE) {
            results1_level[[result_name]] <<- est1
          } else {
            results1[[result_name]] <<- est1
          }
        }, error = function(e) {
          message(sprintf("Error in %s: %s", result_name, e$message))
        })
      }
    }
  }
}
