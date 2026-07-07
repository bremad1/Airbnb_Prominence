find_codex_project_root <- function() {
  candidates <- unique(normalizePath(c(".", ".."), winslash = "/", mustWork = FALSE))
  matches <- candidates[file.exists(file.path(candidates, "Quarterly_dataset1.RData"))]

  if (length(matches) == 0) {
    stop(
      "Quarterly_dataset1.RData not found. Run this script from the project root or codex/ folder.",
      call. = FALSE
    )
  }

  matches[[1]]
}

codex_project_root <- find_codex_project_root()
codex_project_file <- function(...) file.path(codex_project_root, ...)
