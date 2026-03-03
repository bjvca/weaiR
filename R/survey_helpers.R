#' Compute a survey-weighted mean or plain mean
#'
#' @param x Numeric vector.
#' @param design A survey.design object or NULL.
#' @param subset_expr Logical vector for subsetting (same length as design data).
#' @return A list with elements: estimate, se.
#' @keywords internal
svy_mean <- function(x, design = NULL, subset_expr = NULL) {
  if (!is.null(design)) {
    if (!is.null(subset_expr)) {
      sub_design <- subset(design, subset_expr)
    } else {
      sub_design <- design
    }
    # Add x to the design's variables
    sub_design$variables$.tmp_var <- x[if (!is.null(subset_expr)) subset_expr else TRUE]
    result <- survey::svymean(~ .tmp_var, sub_design, na.rm = TRUE)
    list(estimate = as.numeric(coef(result)),
         se = as.numeric(survey::SE(result)))
  } else {
    if (!is.null(subset_expr)) {
      x <- x[subset_expr]
    }
    list(estimate = mean(x, na.rm = TRUE), se = NA_real_)
  }
}

#' Compute a survey-weighted ratio or plain ratio
#'
#' @param numerator Numeric vector.
#' @param denominator Numeric vector.
#' @param design A survey.design object or NULL.
#' @param subset_expr Logical vector for subsetting.
#' @return A list with elements: estimate, se.
#' @keywords internal
svy_ratio <- function(numerator, denominator, design = NULL, subset_expr = NULL) {
  if (!is.null(design)) {
    if (!is.null(subset_expr)) {
      sub_design <- subset(design, subset_expr)
    } else {
      sub_design <- design
    }
    sub_design$variables$.tmp_num <- numerator[if (!is.null(subset_expr)) subset_expr else TRUE]
    sub_design$variables$.tmp_den <- denominator[if (!is.null(subset_expr)) subset_expr else TRUE]
    result <- survey::svyratio(~ .tmp_num, ~ .tmp_den, sub_design, na.rm = TRUE)
    list(estimate = as.numeric(coef(result)),
         se = as.numeric(survey::SE(result)))
  } else {
    if (!is.null(subset_expr)) {
      numerator <- numerator[subset_expr]
      denominator <- denominator[subset_expr]
    }
    list(estimate = sum(numerator, na.rm = TRUE) / sum(denominator, na.rm = TRUE),
         se = NA_real_)
  }
}
