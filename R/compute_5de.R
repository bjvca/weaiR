#' Compute the 5DE/3DE sub-index
#'
#' Given individual deprivation scores and empowerment status, computes:
#' - H: disempowerment headcount ratio (proportion not empowered)
#' - A: average deprivation score among the disempowered
#' - M0: adjusted headcount (H * A)
#' - DE: 1 - M0
#'
#' @param dscore Numeric vector of deprivation scores.
#' @param empowered Integer vector (1 = empowered, 0 = not).
#' @param design Optional survey.design object.
#' @param subset_expr Optional logical vector for subsetting.
#' @return A list with elements: H, A, M0, DE, N, and corresponding se_ values.
#' @keywords internal
compute_5de <- function(dscore, empowered, design = NULL, subset_expr = NULL) {

  if (!is.null(subset_expr)) {
    dscore_sub <- dscore[subset_expr]
    empowered_sub <- empowered[subset_expr]
  } else {
    dscore_sub <- dscore
    empowered_sub <- empowered
  }

  # Remove NAs

  valid <- !is.na(dscore_sub) & !is.na(empowered_sub)
  dscore_v <- dscore_sub[valid]
  empowered_v <- empowered_sub[valid]
  N <- length(dscore_v)

  if (N == 0) {
    return(list(H = NA_real_, A = NA_real_, M0 = NA_real_, DE = NA_real_,
                N = 0L, se_H = NA_real_, se_A = NA_real_, se_M0 = NA_real_))
  }

  # H = proportion disempowered
  disemp <- 1 - empowered_v
  # M0 = mean of censored deprivation (dscore for disempowered, 0 for empowered)
  censored_dscore <- ifelse(empowered_v == 0, dscore_v, 0)

  if (!is.null(design)) {
    # Use survey-weighted computation
    h_res <- svy_mean(disemp, design, subset_expr)
    m0_res <- svy_mean(censored_dscore, design, subset_expr)

    H <- h_res$estimate
    M0 <- m0_res$estimate
    A <- if (H > 0) M0 / H else NA_real_

    # For A, use svyratio
    if (H > 0) {
      a_res <- svy_ratio(censored_dscore, disemp, design, subset_expr)
      se_A <- a_res$se
    } else {
      se_A <- NA_real_
    }

    list(H = H, A = A, M0 = M0, DE = 1 - M0, N = N,
         se_H = h_res$se, se_A = se_A, se_M0 = m0_res$se)
  } else {
    H <- mean(disemp)
    M0 <- mean(censored_dscore)
    A <- if (H > 0) M0 / H else NA_real_
    DE <- 1 - M0

    list(H = H, A = A, M0 = M0, DE = DE, N = as.integer(N),
         se_H = NA_real_, se_A = NA_real_, se_M0 = NA_real_)
  }
}
