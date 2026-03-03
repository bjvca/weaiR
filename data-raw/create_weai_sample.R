# Script to generate synthetic WEAI example dataset
# 300 households × 2 individuals (male + female) = 600 rows

set.seed(42)
n_hh <- 300

# Create household-level data
hhid <- rep(seq_len(n_hh), each = 2)
sex <- rep(c(2, 1), times = n_hh)  # 2 = female, 1 = male
group <- rep(sample(1:3, n_hh, replace = TRUE), each = 2)

# Pro-WEAI indicators (10 indicators across 3 domains)
# Domain 1 (Intrinsic): autonomy_inc, selfeff, never_violence
# Domain 2 (Instrumental): feelinputdecagr, assetownership, credit_accdec,
#                           incomecontrol, work_balance, mobility
# Domain 3 (Collective): groupmember

generate_indicator <- function(n, prob_adequate) {
  rbinom(n, 1, prob_adequate)
}

n_total <- n_hh * 2

# Generate indicators with varying adequacy rates
# Women tend to have lower adequacy on some indicators
probs_f <- c(
  autonomy_inc = 0.55, selfeff = 0.70, never_violence = 0.80,
  feelinputdecagr = 0.60, assetownership = 0.50, credit_accdec = 0.45,
  incomecontrol = 0.55, work_balance = 0.65, mobility = 0.70,
  groupmember = 0.50
)
probs_m <- c(
  autonomy_inc = 0.80, selfeff = 0.85, never_violence = 0.90,
  feelinputdecagr = 0.75, assetownership = 0.80, credit_accdec = 0.70,
  incomecontrol = 0.80, work_balance = 0.75, mobility = 0.85,
  groupmember = 0.65
)

indicator_names <- names(probs_f)
indicators <- data.frame(matrix(NA, nrow = n_total, ncol = length(indicator_names)))
names(indicators) <- indicator_names

for (ind in indicator_names) {
  for (i in seq_len(n_total)) {
    if (sex[i] == 2) {
      indicators[i, ind] <- rbinom(1, 1, probs_f[ind])
    } else {
      indicators[i, ind] <- rbinom(1, 1, probs_m[ind])
    }
  }
}

# Introduce a few NAs (< 1%)
set.seed(123)
na_indices <- sample(seq_len(n_total), 3)
indicators$credit_accdec[na_indices[1]] <- NA
indicators$mobility[na_indices[2]] <- NA
indicators$selfeff[na_indices[3]] <- NA

weai_sample <- cbind(
  data.frame(hhid = hhid, sex = sex, group = group),
  indicators
)

# Save
usethis::use_data(weai_sample, overwrite = TRUE)
