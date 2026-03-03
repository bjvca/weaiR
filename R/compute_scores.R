#' Compute individual empowerment scores
#'
#' For each individual, computes the deprivation score (weighted sum of
#' inadequacies), empowerment score (1 - dscore), and empowerment status.
#'
#' @param df Data.frame with binary indicators (1 = adequate, 0 = inadequate).
#' @param indicators Character vector of indicator column names.
#' @param weights Named numeric vector of per-indicator weights.
#' @param cutoff Numeric empowerment threshold.
#' @return A data.frame with columns: dscore, emp_score, empowered.
#'   Rows correspond to rows of df. Rows with any NA indicator get NA scores.
#' @keywords internal
compute_scores <- function(df, indicators, weights, cutoff) {
  # Build matrix of inadequacy (1 = inadequate)
  inad_mat <- as.matrix(df[, indicators, drop = FALSE])
  inad_mat <- 1 - inad_mat  # flip: now 1 = inadequate

  w <- weights[indicators]

  # Deprivation score = sum of (weight * inadequacy)
  dscore <- as.numeric(inad_mat %*% w)

  # Handle rows with any missing indicator
  has_na <- rowSums(is.na(df[, indicators, drop = FALSE])) > 0
  dscore[has_na] <- NA_real_

  # Round very small scores to 0 (matching Stata behavior)
  dscore[!is.na(dscore) & dscore < 0.0006] <- 0

  emp_score <- 1 - dscore

  # Empowered if emp_score >= cutoff (with small tolerance matching Stata)
  empowered <- ifelse(is.na(emp_score), NA_integer_,
                       as.integer(emp_score >= cutoff - 1e-4))

  data.frame(dscore = dscore, emp_score = emp_score, empowered = empowered)
}
