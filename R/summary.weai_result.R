#' Summary method for weai_result
#'
#' Provides a detailed summary including decomposition tables.
#'
#' @param object A \code{weai_result} object.
#' @param ... Additional arguments (ignored).
#' @return An object of class \code{"summary.weai_result"}, invisibly.
#' @export
summary.weai_result <- function(object, ...) {
  structure(object, class = c("summary.weai_result", "weai_result"))
}

#' Print summary of weai_result
#'
#' @param x A \code{summary.weai_result} object.
#' @param ... Additional arguments (ignored).
#' @return Invisibly returns x.
#' @export
print.summary.weai_result <- function(x, ...) {
  # Print the main table first
  print.weai_result(x)

  cfg <- x$config

  # Uncensored headcount ratios
  cat("\n\nUncensored inadequacy headcount ratios (%)\n")
  cat(paste(rep("-", 65), collapse = ""), "\n")
  cat(sprintf("%-40s %12s %12s\n", "", "Women", "Men"))
  cat(paste(rep("-", 65), collapse = ""), "\n")

  decomp_f <- x$decomposition$female
  decomp_m <- x$decomposition$male

  for (i in seq_len(nrow(decomp_f))) {
    label <- paste0(decomp_f$domain[i], ": ", decomp_f$indicator[i])
    cat(sprintf("%-40s %12s %12s\n", label,
                fmt(100 * decomp_f$uncensored_hc[i], 1),
                fmt(100 * decomp_m$uncensored_hc[i], 1)))
  }
  cat(paste(rep("-", 65), collapse = ""), "\n")

  # Censored headcount ratios
  cat("\n\nCensored inadequacy headcount ratios (%)\n")
  cat(paste(rep("-", 65), collapse = ""), "\n")
  cat(sprintf("%-40s %12s %12s\n", "", "Women", "Men"))
  cat(paste(rep("-", 65), collapse = ""), "\n")

  for (i in seq_len(nrow(decomp_f))) {
    label <- paste0(decomp_f$domain[i], ": ", decomp_f$indicator[i])
    cat(sprintf("%-40s %12s %12s\n", label,
                fmt(100 * decomp_f$censored_hc[i], 1),
                fmt(100 * decomp_m$censored_hc[i], 1)))
  }
  cat(paste(rep("-", 65), collapse = ""), "\n")

  # Relative contributions
  cat("\n\nRelative contribution of each indicator to disempowerment (%)\n")
  cat(paste(rep("-", 75), collapse = ""), "\n")
  cat(sprintf("%-35s %8s %14s %14s\n", "", "Weight", "Women", "Men"))
  cat(paste(rep("-", 75), collapse = ""), "\n")

  for (i in seq_len(nrow(decomp_f))) {
    label <- paste0(decomp_f$domain[i], ": ", decomp_f$indicator[i])
    cat(sprintf("%-35s %8s %14s %14s\n", label,
                fmt(decomp_f$weight[i], 2),
                fmt(decomp_f$rel_contribution[i], 2),
                fmt(decomp_m$rel_contribution[i], 2)))
  }
  cat(paste(rep("-", 75), collapse = ""), "\n")
  cat(sprintf("Note: %d indicators calculated.\n", cfg$n_indicators))

  # Subgroup results
  if (!is.null(x$subgroup)) {
    cat("\n\nDecomposition of empowerment results by subgroup\n")
    cat(paste(rep("=", 75), collapse = ""), "\n")

    for (lvl_name in names(x$subgroup)) {
      sg <- x$subgroup[[lvl_name]]
      cat(sprintf("\n--- Subgroup: %s ---\n", lvl_name))
      cat(sprintf("%-40s %12s %12s\n", "", "Women", "Men"))
      cat(paste(rep("-", 65), collapse = ""), "\n")

      weai_val <- sg$weai
      cat(sprintf("%-40s %12s\n", "WEAI", fmt(weai_val)))
      cat(sprintf("%-40s %12s %12s\n", "DE Index",
                  fmt(sg$de_female$DE), fmt(sg$de_male$DE)))
      cat(sprintf("%-40s %12s\n", "GPI", fmt(sg$gpi$GPI)))
      cat(sprintf("%-40s %12s %12s\n", "% Not achieving empowerment (H)",
                  fmt(100 * sg$de_female$H), fmt(100 * sg$de_male$H)))
      cat(sprintf("%-40s %12s %12s\n", "Mean disempowerment score (A)*",
                  fmt(sg$de_female$A), fmt(sg$de_male$A)))
      cat(sprintf("%-40s %12s\n", "% Without gender parity (HGPI)",
                  fmt(100 * sg$gpi$HGPI)))
      cat(sprintf("%-40s %12s\n", "Mean empowerment gap (IGPI)",
                  fmt(sg$gpi$IGPI)))
      cat(sprintf("%-40s %12s\n", "Number of dual households",
                  fmt(sg$gpi$N_dual, 0)))
      cat(sprintf("%-40s %12d %12d\n", "Number of observations",
                  sg$de_female$N, sg$de_male$N))
      cat(paste(rep("-", 65), collapse = ""), "\n")
    }
  }

  invisible(x)
}
