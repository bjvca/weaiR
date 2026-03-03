#' Resolve indicator weights
#'
#' If weights are NULL, assigns default weights based on the number of domains:
#' - 5 domains: 1/5 per domain, split equally among indicators within each domain
#' - 3 domains (pro-WEAI): 1/N per indicator (equal weight for every indicator)
#' - Other: 1/N per indicator
#'
#' If weights are provided but don't sum to exactly 1, rescales them.
#'
#' @param domains Named list of character vectors.
#' @param weights Named list of numeric vectors, or NULL.
#' @return A named numeric vector of per-indicator weights (names = indicator names).
#' @keywords internal
resolve_weights <- function(domains, weights = NULL) {
  ndom <- length(domains)
  all_indicators <- unlist(domains, use.names = FALSE)
  ni <- length(all_indicators)

  if (is.null(weights)) {
    if (ndom == 5) {
      # WEAI / A-WEAI: 1/5 per domain, equal within domain
      w <- numeric(0)
      for (i in seq_along(domains)) {
        n_ind <- length(domains[[i]])
        w <- c(w, rep(1 / (5 * n_ind), n_ind))
      }
    } else {
      # pro-WEAI (3 domains) or other: equal weight per indicator
      w <- rep(1 / ni, ni)
    }
  } else {
    w <- unlist(weights, use.names = FALSE)
  }

  # Rescale if close but not exactly 1
  total <- sum(w)
  if (abs(total - 1) > 1e-10 && abs(total - 1) <= 0.01) {
    w <- w / total
  }

  names(w) <- all_indicators
  w
}
