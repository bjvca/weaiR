#' Compute the Gender Parity Index (GPI)
#'
#' Implements the GPI sub-index following the Stata weai.ado methodology:
#' 1. Keep only dual-adult households (one male + one female).
#' 2. Compute censored inadequacy scores.
#' 3. Determine gender parity status per household.
#' 4. Compute HGPI, IGPI, and GPI.
#'
#' @param df Data.frame with at minimum the sex and hhid columns, plus
#'   dscore, emp_score, and empowered columns already computed.
#' @param sex Character, column name for sex variable.
#' @param female Value identifying female respondents.
#' @param hhid Character, column name for household ID.
#' @param cutoff Numeric empowerment threshold.
#' @param indicators Character vector of indicator column names.
#' @param weights Named numeric vector of per-indicator weights.
#' @param design Optional survey.design object.
#' @return A list with elements: HGPI, IGPI, GPI, N_dual,
#'   and individual-level vectors: gender_parity, hh_ineq, gpi_individual.
#' @keywords internal
compute_gpi <- function(df, sex, female, hhid, cutoff,
                        indicators, weights, design = NULL) {
  sex_vals <- unique(df[[sex]][!is.na(df[[sex]])])
  male <- setdiff(sex_vals, female)
  if (length(male) != 1) {
    stop("Could not determine male value from sex variable.", call. = FALSE)
  }

  # Compute per-person inadequacy count (ci) as weighted sum of inadequacies
  inad_mat <- 1 - as.matrix(df[, indicators, drop = FALSE])
  w_vec <- weights[indicators]
  ci <- as.numeric(inad_mat %*% w_vec)
  ci <- round(ci, 4)

  # Initialize individual-level results (full dataset length)
  n <- nrow(df)
  gender_parity <- rep(NA_integer_, n)
  hh_ineq <- rep(NA_real_, n)
  gpi_individual <- rep(NA_real_, n)

  hh_ids <- df[[hhid]]
  sex_col <- df[[sex]]

  # --- Vectorized dual-household identification ---
  # Count males and females per household using tapply
  is_female <- sex_col == female
  is_male <- sex_col == male

  hh_factor <- factor(hh_ids)
  n_female <- tapply(is_female, hh_factor, sum)
  n_male <- tapply(is_male, hh_factor, sum)

  # Valid dual: exactly 1 female and 1 male
  valid_hh_names <- names(n_female)[n_female == 1 & n_male == 1]

  if (length(valid_hh_names) == 0) {
    return(list(
      HGPI = NA_real_, IGPI = NA_real_, GPI = NA_real_, N_dual = 0L,
      gender_parity = gender_parity, hh_ineq = hh_ineq,
      gpi_individual = gpi_individual,
      se_HGPI = NA_real_, se_IGPI = NA_real_, se_GPI = NA_real_
    ))
  }

  # Mask for rows in valid dual households
  in_valid <- hh_ids %in% valid_hh_names

  # Split into female and male rows within valid dual HHs
  f_mask <- in_valid & is_female
  m_mask <- in_valid & is_male

  # Build household-level vectors using match
  # Sort female and male rows by hhid so they align
  f_idx <- which(f_mask)
  m_idx <- which(m_mask)
  f_hh <- hh_ids[f_idx]
  m_hh <- hh_ids[m_idx]

  # Align: for each female row's household, find the corresponding male row
  m_order <- match(f_hh, m_hh)
  m_idx_aligned <- m_idx[m_order]

  N_dual <- length(f_idx)

  # Extract household-level vectors (one per dual HH, indexed by female row)
  W_ci <- ci[f_idx]
  M_ci <- ci[m_idx_aligned]
  W_empowered <- df$empowered[f_idx]

  # Censored inadequacy: max(ci, 1 - cutoff)
  floor_val <- 1 - cutoff
  W_cen_ci <- pmax(W_ci, floor_val)
  M_cen_ci <- pmax(M_ci, floor_val)

  # Intrahousehold inequality
  hh_ineq_dual <- W_ci - M_ci

  # Gender parity determination
  ci_above <- as.integer(W_cen_ci > M_cen_ci)
  gp <- 1L - ci_above
  gp[W_empowered == 1L] <- 1L
  ci_above[gp == 1L] <- 0L

  # HGPI
  n_without_parity <- sum(ci_above == 1)
  HGPI <- n_without_parity / N_dual

  # IGPI
  without_parity <- ci_above == 1
  if (any(without_parity)) {
    gap <- rep(NA_real_, N_dual)
    gap[without_parity] <- (W_cen_ci[without_parity] - M_cen_ci[without_parity]) /
      (1 - M_cen_ci[without_parity])
    gap[without_parity & M_cen_ci == 1] <- NA_real_
    IGPI <- mean(gap[without_parity], na.rm = TRUE)
  } else {
    gap <- rep(0, N_dual)
    IGPI <- 0
  }

  # GPI = 1 - (HGPI * IGPI)
  GPI <- 1 - (HGPI * IGPI)

  # Individual-level GPI (for women only)
  ind_P1 <- ifelse(without_parity, ci_above * gap, 0)
  ind_P1[is.na(ind_P1)] <- 0
  gpi_ind_dual <- 1 - ind_P1

  # --- Vectorized map back to full dataset ---
  gender_parity[f_idx] <- gp
  gender_parity[m_idx_aligned] <- gp
  hh_ineq[f_idx] <- hh_ineq_dual
  hh_ineq[m_idx_aligned] <- -hh_ineq_dual
  gpi_individual[f_idx] <- gpi_ind_dual

  list(
    HGPI = HGPI, IGPI = IGPI, GPI = GPI, N_dual = as.integer(N_dual),
    gender_parity = gender_parity, hh_ineq = hh_ineq,
    gpi_individual = gpi_individual,
    se_HGPI = NA_real_, se_IGPI = NA_real_, se_GPI = NA_real_
  )
}
