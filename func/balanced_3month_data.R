# Shared data loader for the diagnostic scripts.
# Entire and quarterly_panel are stored as ordinary global objects. The small
# private environment below retains helper function definitions only.
if (
  !exists(
    ".balanced_3month_data_cache",
    envir = .GlobalEnv,
    inherits = FALSE
  )
) {
  assign(
    ".balanced_3month_data_cache",
    new.env(parent = emptyenv()),
    envir = .GlobalEnv
  )
}

balanced_3month_data_cache <- function() {
  cache <- get(
    ".balanced_3month_data_cache",
    envir = .GlobalEnv,
    inherits = FALSE
  )
  if (!is.environment(cache)) {
    stop(".balanced_3month_data_cache must be an environment.")
  }
  cache
}

balanced_3month_project_root <- function() {
  candidates <- c(
    getwd(),
    file.path(getwd(), "Final2"),
    dirname(getwd()),
    dirname(dirname(getwd()))
  )
  matches <- candidates[
    file.exists(file.path(
      candidates,
      "sub",
      "balanced-3month.R"
    ))
  ]
  if (length(matches) == 0L) {
    stop(
      "Cannot locate sub/balanced-3month.R from: ",
      getwd()
    )
  }
  normalizePath(matches[[1L]], winslash = "/", mustWork = TRUE)
}

load_balanced_3month_helpers <- function(force = FALSE) {
  cache <- balanced_3month_data_cache()
  if (!force && exists("helpers", envir = cache, inherits = FALSE)) {
    return(cache$helpers)
  }

  project_root <- balanced_3month_project_root()
  helper_file <- file.path(
    project_root,
    "sub",
    "balanced-3month.R"
  )
  helper_env <- new.env(parent = .GlobalEnv)

  # The legacy helper script imports Entire.RData at top level. Suppress only
  # that import while sourcing its function definitions; the data are loaded
  # lazily and explicitly by load_balanced_3month_monthly_data().
  helper_env$load <- function(...) invisible(character())
  old_wd <- getwd()
  setwd(project_root)
  tryCatch(
    sys.source(helper_file, envir = helper_env),
    finally = setwd(old_wd)
  )
  rm("load", envir = helper_env)

  cache$helpers <- helper_env
  cache$project_root <- project_root
  helper_env
}

load_balanced_3month_monthly_data <- function(force = FALSE) {
  cache <- balanced_3month_data_cache()
  if (
    !force &&
      exists("Entire", envir = .GlobalEnv, inherits = FALSE)
  ) {
    return(get("Entire", envir = .GlobalEnv, inherits = FALSE))
  }

  if (!exists("project_root", envir = cache, inherits = FALSE)) {
    load_balanced_3month_helpers()
  }
  project_root <- cache$project_root
  data_file <- file.path(project_root, "RData", "Entire.RData")
  loaded_names <- base::load(data_file, envir = .GlobalEnv)
  if (!"Entire" %in% loaded_names) {
    stop("Entire.RData does not contain an object named Entire.")
  }

  cache$data_file <- normalizePath(
    data_file,
    winslash = "/",
    mustWork = TRUE
  )
  message(
    "[balanced_3month_data] loaded Entire into .GlobalEnv: ",
    format(nrow(Entire), big.mark = ","),
    " rows"
  )
  get("Entire", envir = .GlobalEnv, inherits = FALSE)
}

get_balanced_3month_quarter_panel <- function(force = FALSE) {
  if (
    !force &&
      exists("quarterly_panel", envir = .GlobalEnv, inherits = FALSE)
  ) {
    return(get(
      "quarterly_panel",
      envir = .GlobalEnv,
      inherits = FALSE
    ))
  }

  helpers <- load_balanced_3month_helpers(force = force)
  monthly_data <- load_balanced_3month_monthly_data(force = force)
  quarterly_panel_value <- helpers$build_quarter_panel(monthly_data)
  assign(
    "quarterly_panel",
    quarterly_panel_value,
    envir = .GlobalEnv
  )
  message(
    "[balanced_3month_data] created quarterly_panel in .GlobalEnv: ",
    format(nrow(quarterly_panel_value), big.mark = ","),
    " rows"
  )
  quarterly_panel_value
}

# User-facing entry point. It creates ordinary Entire and quarterly_panel
# objects in the global environment.
make_3month_data <- function(refresh_data = FALSE) {
  get_balanced_3month_quarter_panel(force = refresh_data)
}

get_balanced_3month_derived_data <- function(
    key,
    build,
    force = FALSE
) {
  if (!is.character(key) || length(key) != 1L || !nzchar(key)) {
    stop("key must be one nonempty character value.")
  }
  if (!is.function(build)) stop("build must be a function.")

  value <- build(get_balanced_3month_quarter_panel(force = force))
  value
}

clear_balanced_3month_data_cache <- function() {
  cache <- balanced_3month_data_cache()
  rm(list = ls(envir = cache, all.names = TRUE), envir = cache)
  global_data <- intersect(
    c("Entire", "quarterly_panel"),
    ls(envir = .GlobalEnv, all.names = TRUE)
  )
  if (length(global_data) > 0L) {
    rm(list = global_data, envir = .GlobalEnv)
  }
  invisible(TRUE)
}
