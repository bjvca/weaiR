#' Compute indicator-level decomposition of disempowerment
#'
#' For each indicator, computes:
#' - Uncensored headcount: proportion inadequate (all individuals)
#' - Censored headcount: proportion both disempowered AND inadequate
#' - Absolute contribution: censored_headcount * weight
#' - Relative contribution: abs_contribution / M0 * 100 (sums to 100%)
#'
#' @param df Data.frame with indicator columns plus empowered column.
#' @param indicators Character vector of indicator column names.
#' @param weights Named numeric vector of per-indicator weights.
#' @param domains Named list of indicator groups (for domain labels).
#' @param empowered Integer vector (1 = empowered, 0 = not).
#' @param M0 The adjusted headcount ratio for normalization.
#' @param design Optional survey.design object.
#' @param subset_expr Optional logical vector for subsetting.
#' @return A data.frame with one row per indicator and columns:
#'   indicator, domain, weight, uncensored_hc, censored_hc,
#'   abs_contribution, rel_contribution.
#' @keywords internal
compute_decomposition <- function(df, indicators, weights, domains,
                                  empowered, M0,
                                  design = NULL, subset_expr = NULL) {
  if (!is.null(subset_expr)) {
    df_sub <- df[subset_expr, , drop = FALSE]
    empowered_sub <- empowered[subset_expr]
  } else {
    df_sub <- df
    empowered_sub <- empowered
  }

  # Build domain lookup
  domain_lookup <- character(0)
  for (dom_name in names(domains)) {
    for (ind in domains[[dom_name]]) {
      domain_lookup[ind] <- dom_name
    }
  }

  results <- vector("list", length(indicators))

  for (i in seq_along(indicators)) {
    ind <- indicators[i]
    w <- weights[ind]
    dom <- domain_lookup[ind]

    # Inadequacy: 1 - indicator value
    inadequate <- 1 - df_sub[[ind]]
    valid <- !is.na(inadequate) & !is.na(empowered_sub)
    inad_v <- inadequate[valid]
    emp_v <- empowered_sub[valid]

    n <- sum(valid)
    if (n == 0) {
      results[[i]] <- data.frame(
        indicator = ind, domain = dom, weight = w,
        uncensored_hc = NA_real_, censored_hc = NA_real_,
        abs_contribution = NA_real_, rel_contribution = NA_real_,
        stringsAsFactors = FALSE
      )
      next
    }

    # Uncensored headcount: proportion inadequate (all individuals)
    uncensored_hc <- mean(inad_v)

    # Censored headcount: proportion both disempowered AND inadequate
    censored_inad <- as.numeric(inad_v == 1 & emp_v == 0)
    censored_hc <- mean(censored_inad)

    # Absolute contribution = censored_hc * weight
    abs_contribution <- censored_hc * w

    # Relative contribution = abs_contribution / M0 * 100
    if (!is.na(M0) && M0 > 0) {
      rel_contribution <- abs_contribution / M0 * 100
    } else {
      rel_contribution <- NA_real_
    }

    results[[i]] <- data.frame(
      indicator = ind, domain = dom, weight = w,
      uncensored_hc = uncensored_hc, censored_hc = censored_hc,
      abs_contribution = abs_contribution, rel_contribution = rel_contribution,
      stringsAsFactors = FALSE
    )
  }

  do.call(rbind, results)
}
