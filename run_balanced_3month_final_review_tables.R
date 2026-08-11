PROJECT_DIR <- if (file.exists(file.path(
  getwd(), "func", "run_regression.R"
))) {
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
} else if (file.exists(file.path(
  getwd(), "Final2", "func", "run_regression.R"
))) {
  normalizePath(file.path(getwd(), "Final2"), winslash = "/", mustWork = TRUE)
} else {
  stop("Run this script from the project root or Final2 directory.")
}

source(file.path(PROJECT_DIR, "func", "run_regression.R"))
if (sys.nframe() == 0L) run_regression()
