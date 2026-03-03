#' Compute the Women's Empowerment in Agriculture Index (WEAI)
#'
#' Computes the WEAI family of indices (WEAI, A-WEAI, pro-WEAI) from
#' pre-computed binary adequacy indicators. The function implements the
#' Alkire-Foster methodology including the Five/Three Domains of Empowerment
#' (5DE/3DE) sub-index, Gender Parity Index (GPI), indicator-level
#' decomposition, and optional subgroup analysis.
#'
#' @param data A data.frame with binary indicators (1 = adequate, 0 = inadequate).
#'   Required unless \code{design} is provided.
#' @param domains A named list of character vectors grouping indicator column
#'   names by domain. At least 3 domains must be specified.
#' @param weights Optional named list of numeric vectors specifying per-indicator
#'   weights. Must have the same structure as \code{domains} and sum to 1.
#'   If NULL, defaults are used: for 5 domains, 1/5 per domain split equally
#'   within; for 3 domains (pro-WEAI), 1/N equal weight per indicator.
#' @param sex Column name (character) for the gender variable.
#' @param female Value identifying female respondents in the sex column.
#' @param hhid Column name (character) for the household ID variable.
#' @param cutoff Empowerment threshold (default 0.80). A person is empowered if
#'   their empowerment score >= cutoff.
#' @param by Optional column name (character) for subgroup decomposition.
#' @param design Optional \code{survey.design} object from the survey package.
#'   If provided, overrides \code{data} and all aggregate computations use
#'   survey-weighted estimators.
#'
#' @return An object of class \code{"weai_result"}, a list with:
#'   \describe{
#'     \item{individual}{Data.frame with per-person emp_score, empowered,
#'       gender_parity, hh_ineq, emp_index, gpi, weai columns.}
#'     \item{aggregate}{List with overall, de_female, de_male, gpi sub-lists
#'       each containing H, A, M0, DE, HGPI, IGPI, GPI, WEAI as appropriate.}
#'     \item{decomposition}{List with female and male data.frames showing
#'       per-indicator uncensored/censored headcounts and contributions.}
#'     \item{subgroup}{Named list of per-subgroup results, or NULL.}
#'     \item{call}{The matched call.}
#'     \item{config}{List of parameters used (cutoff, domains, weights, etc.)}
#'   }
#'
#' @examples
#' \dontrun{
#' data(weai_sample)
#' res <- weai(
#'   weai_sample,
#'   domains = list(
#'     intrinsic = c("autonomy_inc", "selfeff", "never_violence"),
#'     instrumental = c("feelinputdecagr", "assetownership", "credit_accdec",
#'                      "incomecontrol", "work_balance", "mobility"),
#'     collective = c("groupmember")
#'   ),
#'   sex = "sex", female = 2, hhid = "hhid"
#' )
#' print(res)
#' summary(res)
#' }
#'
#' @export
weai <- function(data = NULL, domains, weights = NULL, sex, female, hhid,
                 cutoff = 0.80, by = NULL, design = NULL) {
  cl <- match.call()

  # --- Validation ---
  df <- validate_inputs(data, domains, weights, sex, female, hhid,
                        cutoff, by, design)

  # --- Resolve weights ---
  w <- resolve_weights(domains, weights)
  all_indicators <- names(w)

  # --- Determine male value ---
  sex_vals <- unique(df[[sex]][!is.na(df[[sex]])])
  male_val <- setdiff(sex_vals, female)

  # --- Drop rows with missing indicators ---
  indicator_complete <- complete.cases(df[, all_indicators, drop = FALSE])
  if (!is.null(by)) {
    indicator_complete <- indicator_complete & !is.na(df[[by]])
  }
  n_dropped <- sum(!indicator_complete)
  if (n_dropped > 0) {
    message("Note: ", n_dropped, " rows with missing indicator values excluded.")
  }

  df_clean <- df[indicator_complete, , drop = FALSE]
  if (nrow(df_clean) == 0) {
    stop("No complete observations remaining after removing missing values.",
         call. = FALSE)
  }

  # --- Compute individual scores ---
  scores <- compute_scores(df_clean, all_indicators, w, cutoff)
  df_clean$dscore <- scores$dscore
  df_clean$emp_score <- scores$emp_score
  df_clean$empowered <- scores$empowered

  # --- Compute 5DE/3DE by sex ---
  f_mask <- df_clean[[sex]] == female
  m_mask <- df_clean[[sex]] == male_val

  de_female <- compute_5de(scores$dscore, scores$empowered, design, f_mask)
  de_male <- compute_5de(scores$dscore, scores$empowered, design, m_mask)

  # --- Compute GPI ---
  gpi_res <- compute_gpi(df_clean, sex, female, hhid, cutoff,
                         all_indicators, w, design)

  # --- Compute WEAI ---
  weai_val <- if (!is.na(de_female$DE) && !is.na(gpi_res$GPI)) {
    0.9 * de_female$DE + 0.1 * gpi_res$GPI
  } else {
    NA_real_
  }

  # --- Individual-level composite indices ---
  # emp_index = 1 - (dscore * (1 - empowered))
  emp_index <- 1 - (scores$dscore * (1 - scores$empowered))

  # individual weai (for women only)
  indiv_weai <- rep(NA_real_, nrow(df_clean))
  indiv_weai[f_mask] <- 0.9 * emp_index[f_mask] +
    0.1 * ifelse(is.na(gpi_res$gpi_individual[f_mask]), 1,
                 gpi_res$gpi_individual[f_mask])

  # --- Build individual data.frame ---
  individual <- data.frame(
    row.names = NULL,
    emp_score = scores$emp_score,
    empowered = scores$empowered,
    gender_parity = gpi_res$gender_parity,
    hh_ineq = gpi_res$hh_ineq,
    emp_index = emp_index,
    gpi = gpi_res$gpi_individual,
    weai = indiv_weai
  )
  # Attach original identifiers
  individual[[hhid]] <- df_clean[[hhid]]
  individual[[sex]] <- df_clean[[sex]]

  # --- Decomposition ---
  decomp_female <- compute_decomposition(
    df_clean, all_indicators, w, domains,
    scores$empowered, de_female$M0, design, f_mask
  )

  decomp_male <- compute_decomposition(
    df_clean, all_indicators, w, domains,
    scores$empowered, de_male$M0, design, m_mask
  )

  # --- Aggregate results ---
  aggregate <- list(
    overall = list(WEAI = weai_val, n_indicators = length(all_indicators)),
    de_female = de_female,
    de_male = de_male,
    gpi = list(
      HGPI = gpi_res$HGPI,
      IGPI = gpi_res$IGPI,
      GPI = gpi_res$GPI,
      N_dual = gpi_res$N_dual,
      se_HGPI = gpi_res$se_HGPI,
      se_IGPI = gpi_res$se_IGPI,
      se_GPI = gpi_res$se_GPI
    )
  )

  decomposition <- list(female = decomp_female, male = decomp_male)

  # --- Subgroup decomposition ---
  subgroup <- NULL
  if (!is.null(by)) {
    subgroup <- compute_subgroup(df_clean, by, all_indicators, w, domains,
                                sex, female, hhid, cutoff, design)
  }

  # --- Config ---
  config <- list(
    cutoff = cutoff,
    domains = domains,
    weights = w,
    sex = sex,
    female = female,
    male = male_val,
    hhid = hhid,
    by = by,
    n_indicators = length(all_indicators),
    n_domains = length(domains),
    has_design = !is.null(design)
  )

  new_weai_result(individual, aggregate, decomposition, subgroup, cl, config)
}
