PROJECT_DIR <- if (file.exists(file.path(
  getwd(), "func", "placebo.R"
))) {
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
} else if (file.exists(file.path(
  getwd(), "Final2", "func", "placebo.R"
))) {
  normalizePath(file.path(getwd(), "Final2"), winslash = "/", mustWork = TRUE)
} else {
  stop("Run this script from the project root or the Final2 directory.")
}

source(file.path(PROJECT_DIR, "func", "balanced_3month_data.R"))
source(file.path(PROJECT_DIR, "func", "mccrary.R"))
source(file.path(PROJECT_DIR, "func", "placebo.R"))
source(file.path(PROJECT_DIR, "func", "run_regression.R"))

make_3month_data()
mccrary()
placebo()
run_regression()
