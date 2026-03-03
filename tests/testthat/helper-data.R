# Helper to load weai_sample for tests
load_weai_sample <- function() {
  # Try installed package first
  pkg_path <- system.file("data", "weai_sample.rda", package = "weaiR")
  if (nzchar(pkg_path) && file.exists(pkg_path)) {
    e <- new.env()
    load(pkg_path, envir = e)
    return(e$weai_sample)
  }
  # Try relative path from testthat directory
  rel_path <- file.path(testthat::test_path("..", "..", "data", "weai_sample.rda"))
  if (file.exists(rel_path)) {
    e <- new.env()
    load(rel_path, envir = e)
    return(e$weai_sample)
  }
  # Try from working directory
  for (p in c("weaiR/data/weai_sample.rda", "data/weai_sample.rda")) {
    if (file.exists(p)) {
      e <- new.env()
      load(p, envir = e)
      return(e$weai_sample)
    }
  }
  # Last resort: use the global variable
  if (exists("weai_sample", envir = .GlobalEnv)) {
    return(get("weai_sample", envir = .GlobalEnv))
  }
  stop("Cannot find weai_sample.rda")
}
