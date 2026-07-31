# Load the three simple Final2 entry points:
#   make_3month_data()
#   placebo()
#   mccrary()
#
# Example:
#   source("Final2/func/functions.R")
#   make_3month_data()  # creates Entire and quarterly_panel globally
#   placebo()

FINAL2_FUNCTIONS_DIR <- if (file.exists(file.path(
  getwd(), "Final2", "func", "functions.R"
))) {
  normalizePath(file.path(getwd(), "Final2"), winslash = "/", mustWork = TRUE)
} else if (file.exists(file.path(getwd(), "func", "functions.R"))) {
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
} else {
  normalizePath(file.path(getwd(), ".."), winslash = "/", mustWork = TRUE)
}

source(file.path(
  FINAL2_FUNCTIONS_DIR,
  "func",
  "balanced_3month_data.R"
))
source(file.path(FINAL2_FUNCTIONS_DIR, "func", "placebo.R"))
source(file.path(
  FINAL2_FUNCTIONS_DIR,
  "func",
  "mccrary.R"
))

# Keep experimental overrides isolated so baseline placebo() remains unchanged.
PLACEBO_ADAPTIVE_ENV <- new.env(parent = .GlobalEnv)
sys.source(
  file.path(
    FINAL2_FUNCTIONS_DIR,
    "func",
    "placebo_adaptive_bandwidth.R"
  ),
  envir = PLACEBO_ADAPTIVE_ENV
)
placebo_adaptive <- PLACEBO_ADAPTIVE_ENV$placebo_adaptive

invisible(c(
  "make_3month_data",
  "placebo",
  "placebo_adaptive",
  "mccrary"
))
