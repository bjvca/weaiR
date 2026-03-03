#' Print a weai_result object
#'
#' Displays the main empowerment results table, matching the Stata output format.
#'
#' @param x A \code{weai_result} object.
#' @param ... Additional arguments (ignored).
#' @return Invisibly returns x.
#' @export
print.weai_result <- function(x, ...) {
  agg <- x$aggregate
  cfg <- x$config

  # Determine index name
  idx_name <- if (cfg$n_domains == 3) "pro-WEAI" else
    if (cfg$n_domains == 5 && cfg$n_indicators == 6) "A-WEAI" else
      if (cfg$n_domains == 5) "WEAI" else "WEAI"

  de_name <- if (cfg$n_domains == 5) "5DE" else "3DE"

  cat("\nEmpowerment results\n")
  cat(paste(rep("-", 65), collapse = ""), "\n")

  # Header
  cat(sprintf("%-40s %12s %12s\n", "", "Women", "Men"))
  cat(paste(rep("-", 65), collapse = ""), "\n")

  # WEAI
  cat(sprintf("%-40s %12s %12s\n", paste0(idx_name),
              fmt(agg$overall$WEAI), ""))

  # DE
  cat(sprintf("%-40s %12s %12s\n", paste0(de_name, " Index"),
              fmt(agg$de_female$DE), fmt(agg$de_male$DE)))

  # GPI
  cat(sprintf("%-40s %12s %12s\n", "Gender Parity Index (GPI)",
              fmt(agg$gpi$GPI), ""))

  cat(paste(rep("-", 65), collapse = ""), "\n")

  # H
  cat(sprintf("%-40s %12s %12s\n", "% Not achieving empowerment (H)",
              fmt(100 * agg$de_female$H), fmt(100 * agg$de_male$H)))

  # A
  cat(sprintf("%-40s %12s %12s\n", "Mean disempowerment score (A)*",
              fmt(agg$de_female$A), fmt(agg$de_male$A)))

  cat(paste(rep("-", 65), collapse = ""), "\n")

  # HGPI
  cat(sprintf("%-40s %12s %12s\n", "% Without gender parity (HGPI)",
              fmt(100 * agg$gpi$HGPI), ""))

  # IGPI
  cat(sprintf("%-40s %12s %12s\n", "Mean empowerment gap (IGPI)",
              fmt(agg$gpi$IGPI), ""))

  cat(paste(rep("-", 65), collapse = ""), "\n")

  # N
  cat(sprintf("%-40s %12s %12s\n", "Number of dual households",
              fmt(agg$gpi$N_dual, 0), ""))
  cat(sprintf("%-40s %12d %12d\n", "Number of observations",
              agg$de_female$N, agg$de_male$N))

  cat(paste(rep("=", 65), collapse = ""), "\n")
  cat(sprintf("Note: %d indicators calculated.\n", cfg$n_indicators))
  cat("* Refers to the mean disempowerment score among only\n")
  cat("  women/men who are disempowered.\n")
  cat(sprintf("  %s = 1 - (H*A); GPI = 1 - (HGPI*IGPI)\n", de_name))

  invisible(x)
}
