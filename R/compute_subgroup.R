#' Compute WEAI results for subgroups
#'
#' @param df Data.frame with all required columns.
#' @param by Column name for subgroup variable.
#' @param indicators Character vector of indicator column names.
#' @param weights Named numeric vector of per-indicator weights.
#' @param domains Named list of character vectors.
#' @param sex Column name for sex variable.
#' @param female Value identifying female respondents.
#' @param hhid Column name for household ID.
#' @param cutoff Numeric empowerment threshold.
#' @param design Optional survey.design object.
#' @return A named list where each element corresponds to a subgroup level
#'   and contains $de_female, $de_male, $gpi, $decomposition, $weai.
#' @keywords internal
compute_subgroup <- function(df, by, indicators, weights, domains,
                             sex, female, hhid, cutoff, design = NULL) {
  by_col <- df[[by]]
  levels <- sort(unique(by_col[!is.na(by_col)]))
  male_val <- setdiff(unique(df[[sex]][!is.na(df[[sex]])]), female)

  result <- list()

  for (lvl in levels) {
    lvl_mask <- !is.na(by_col) & by_col == lvl
    df_sub <- df[lvl_mask, , drop = FALSE]

    scores <- compute_scores(df_sub, indicators, weights, cutoff)

    # Female 5DE
    f_mask <- df_sub[[sex]] == female
    de_f <- compute_5de(scores$dscore, scores$empowered, subset_expr = f_mask)

    # Male 5DE
    m_mask <- df_sub[[sex]] == male_val
    de_m <- compute_5de(scores$dscore, scores$empowered, subset_expr = m_mask)

    # GPI on the subgroup
    df_sub$empowered <- scores$empowered
    df_sub$dscore <- scores$dscore
    df_sub$emp_score <- scores$emp_score
    gpi_res <- compute_gpi(df_sub, sex, female, hhid, cutoff,
                           indicators, weights)

    # WEAI
    weai_val <- if (!is.na(de_f$DE) && !is.na(gpi_res$GPI)) {
      0.9 * de_f$DE + 0.1 * gpi_res$GPI
    } else {
      NA_real_
    }

    # Decomposition
    decomp_f <- compute_decomposition(
      df_sub, indicators, weights, domains,
      scores$empowered, de_f$M0, subset_expr = f_mask
    )

    decomp_m <- compute_decomposition(
      df_sub, indicators, weights, domains,
      scores$empowered, de_m$M0, subset_expr = m_mask
    )

    lvl_name <- as.character(lvl)
    result[[lvl_name]] <- list(
      de_female = de_f,
      de_male = de_m,
      gpi = list(
        HGPI = gpi_res$HGPI, IGPI = gpi_res$IGPI, GPI = gpi_res$GPI,
        N_dual = gpi_res$N_dual
      ),
      decomposition = list(female = decomp_f, male = decomp_m),
      weai = weai_val
    )
  }

  result
}
