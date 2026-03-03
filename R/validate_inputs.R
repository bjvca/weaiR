#' Validate inputs to the weai function
#'
#' @param data A data.frame or NULL if design is provided.
#' @param domains Named list of character vectors.
#' @param weights Named list of numeric vectors or NULL.
#' @param sex Character, column name for sex variable.
#' @param female Value identifying female respondents.
#' @param hhid Character, column name for household ID.
#' @param cutoff Numeric between 0 and 1.
#' @param by Optional column name for subgroup decomposition.
#' @param design Optional survey.design object.
#' @return The working data.frame (extracted from design if needed).
#' @keywords internal
validate_inputs <- function(data, domains, weights, sex, female, hhid,
                            cutoff, by, design) {

  # Determine working data

  if (!is.null(design)) {
    if (!inherits(design, "survey.design")) {
      stop("'design' must be a survey.design object from the survey package.",
           call. = FALSE)
    }
    df <- design$variables
  } else if (!is.null(data)) {
    if (!is.data.frame(data)) {
      stop("'data' must be a data.frame.", call. = FALSE)
    }
    df <- data
  } else {
    stop("Either 'data' or 'design' must be provided.", call. = FALSE)
  }

  # Validate domains

  if (!is.list(domains) || length(domains) < 3) {
    stop("'domains' must be a named list with at least 3 elements.", call. = FALSE)
  }
  if (is.null(names(domains)) || any(names(domains) == "")) {
    stop("All elements of 'domains' must be named.", call. = FALSE)
  }
  all_indicators <- unlist(domains, use.names = FALSE)
  if (any(duplicated(all_indicators))) {
    stop("Indicator names must be unique across domains.", call. = FALSE)
  }

  # Check indicator columns exist and are binary
  missing_cols <- setdiff(all_indicators, names(df))
  if (length(missing_cols) > 0) {
    stop("Indicator columns not found in data: ",
         paste(missing_cols, collapse = ", "), call. = FALSE)
  }
  for (ind in all_indicators) {
    vals <- df[[ind]]
    vals <- vals[!is.na(vals)]
    if (!all(vals %in% c(0, 1))) {
      stop("Indicator '", ind, "' must contain only 0, 1, or NA. ",
           "Use 1 = adequate, 0 = inadequate.", call. = FALSE)
    }
  }

  # Validate weights if supplied
  if (!is.null(weights)) {
    if (!is.list(weights)) {
      stop("'weights' must be a named list of numeric vectors.", call. = FALSE)
    }
    if (length(weights) != length(domains)) {
      stop("'weights' must have the same number of elements as 'domains'.",
           call. = FALSE)
    }
    for (i in seq_along(domains)) {
      if (length(weights[[i]]) != length(domains[[i]])) {
        stop("Weight vector for domain '", names(domains)[i],
             "' must have the same length as the indicator list (",
             length(domains[[i]]), ").", call. = FALSE)
      }
    }
    total_w <- sum(unlist(weights))
    if (abs(total_w - 1) > 0.01) {
      stop("Indicator weights must sum to 1. Current sum: ",
           round(total_w, 4), call. = FALSE)
    }
  }

  # Validate sex column
  if (!is.character(sex) || length(sex) != 1) {
    stop("'sex' must be a single column name (character).", call. = FALSE)
  }
  if (!(sex %in% names(df))) {
    stop("Column '", sex, "' not found in data.", call. = FALSE)
  }
  sex_vals <- unique(df[[sex]][!is.na(df[[sex]])])
  if (length(sex_vals) != 2) {
    stop("The sex variable '", sex, "' must have exactly 2 unique non-NA values.",
         call. = FALSE)
  }
  if (!(female %in% sex_vals)) {
    stop("Value '", female, "' not found in column '", sex, "'.", call. = FALSE)
  }

  # Validate hhid
  if (!is.character(hhid) || length(hhid) != 1) {
    stop("'hhid' must be a single column name (character).", call. = FALSE)
  }
  if (!(hhid %in% names(df))) {
    stop("Column '", hhid, "' not found in data.", call. = FALSE)
  }

  # Validate cutoff
  if (!is.numeric(cutoff) || length(cutoff) != 1 || cutoff <= 0 || cutoff > 1) {
    stop("'cutoff' must be a single number between 0 (exclusive) and 1 (inclusive).",
         call. = FALSE)
  }

  # Validate by
  if (!is.null(by)) {
    if (!is.character(by) || length(by) != 1) {
      stop("'by' must be a single column name (character).", call. = FALSE)
    }
    if (!(by %in% names(df))) {
      stop("Column '", by, "' not found in data.", call. = FALSE)
    }
  }

  df
}
