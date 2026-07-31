FINAL2_DIR <- if (file.exists(file.path(
  getwd(), "Final2", "func", "functions.R"
))) {
  file.path(getwd(), "Final2")
} else {
  getwd()
}

source(file.path(FINAL2_DIR, "func", "functions.R"))

make_3month_data()
mccrary()

placebo()
placebo_adaptive()
