#' Flatten domain structure into a data.frame
#'
#' @param domains Named list of character vectors.
#' @param weights Named numeric vector of per-indicator weights.
#' @return A data.frame with columns: indicator, domain, weight.
#' @keywords internal
flatten_domain_structure <- function(domains, weights) {
  rows <- list()
  for (dom_name in names(domains)) {
    for (ind in domains[[dom_name]]) {
      rows[[length(rows) + 1]] <- data.frame(
        indicator = ind,
        domain = dom_name,
        weight = weights[[ind]],
        stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, rows)
}

#' Format a number for display
#'
#' @param x Numeric value.
#' @param digits Number of decimal places.
#' @return Character string.
#' @keywords internal
fmt <- function(x, digits = 3) {
  ifelse(is.na(x), "NA", formatC(x, format = "f", digits = digits))
}
