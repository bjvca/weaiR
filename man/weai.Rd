\name{weai}
\alias{weai}
\title{Compute the Women's Empowerment in Agriculture Index (WEAI)}
\description{
Computes the WEAI family of indices (WEAI, A-WEAI, pro-WEAI) from
pre-computed binary adequacy indicators. Implements the Alkire-Foster
methodology including the Five/Three Domains of Empowerment (5DE/3DE)
sub-index, Gender Parity Index (GPI), indicator-level decomposition,
and optional subgroup analysis.
}
\usage{
weai(data = NULL, domains, weights = NULL, sex, female, hhid,
     cutoff = 0.80, by = NULL, design = NULL)
}
\arguments{
  \item{data}{A data.frame with binary indicators (1 = adequate, 0 = inadequate).
    Required unless \code{design} is provided.}
  \item{domains}{A named list of character vectors grouping indicator column
    names by domain. At least 3 domains must be specified.}
  \item{weights}{Optional named list of numeric vectors specifying per-indicator
    weights. Must have the same structure as \code{domains} and sum to 1.
    If NULL, defaults are used: for 5 domains, 1/5 per domain split equally
    within; for 3 domains (pro-WEAI), 1/N equal weight per indicator.}
  \item{sex}{Column name (character) for the gender variable.}
  \item{female}{Value identifying female respondents in the sex column.}
  \item{hhid}{Column name (character) for the household ID variable.}
  \item{cutoff}{Empowerment threshold (default 0.80). A person is empowered if
    their empowerment score >= cutoff.}
  \item{by}{Optional column name (character) for subgroup decomposition.}
  \item{design}{Optional \code{survey.design} object from the survey package.
    If provided, overrides \code{data} and all aggregate computations use
    survey-weighted estimators.}
}
\value{
An object of class \code{"weai_result"}, a list with components:
\describe{
  \item{individual}{Data.frame with per-person emp_score, empowered,
    gender_parity, hh_ineq, emp_index, gpi, weai columns.}
  \item{aggregate}{List with overall, de_female, de_male, gpi sub-lists
    each containing H, A, M0, DE, HGPI, IGPI, GPI, WEAI as appropriate.}
  \item{decomposition}{List with female and male data.frames showing
    per-indicator uncensored/censored headcounts and contributions.}
  \item{subgroup}{Named list of per-subgroup results, or NULL.}
  \item{call}{The matched call.}
  \item{config}{List of parameters used (cutoff, domains, weights, etc.)}
}
}
\examples{
data(weai_sample)
res <- weai(
  weai_sample,
  domains = list(
    intrinsic = c("autonomy_inc", "selfeff", "never_violence"),
    instrumental = c("feelinputdecagr", "assetownership", "credit_accdec",
                     "incomecontrol", "work_balance", "mobility"),
    collective = c("groupmember")
  ),
  sex = "sex", female = 2, hhid = "hhid"
)
print(res)
}
