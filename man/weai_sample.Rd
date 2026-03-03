\name{weai_sample}
\alias{weai_sample}
\docType{data}
\title{Synthetic WEAI Example Dataset}
\description{
A simulated dataset of 600 individuals (300 households, each with one
male and one female respondent) with 10 binary adequacy indicators
suitable for computing the pro-WEAI.
}
\usage{data(weai_sample)}
\format{
A data.frame with 600 rows and 13 columns:
\describe{
  \item{hhid}{Household identifier (integer, 1-300)}
  \item{sex}{Gender of respondent (2 = female, 1 = male)}
  \item{group}{Project group (1, 2, or 3) for subgroup analysis}
  \item{autonomy_inc}{Autonomy in income decisions (1 = adequate, 0 = inadequate)}
  \item{selfeff}{Self-efficacy (1 = adequate, 0 = inadequate)}
  \item{never_violence}{Attitudes about intimate partner violence (1 = adequate, 0 = inadequate)}
  \item{feelinputdecagr}{Input in livelihood decisions (1 = adequate, 0 = inadequate)}
  \item{assetownership}{Ownership of land and other assets (1 = adequate, 0 = inadequate)}
  \item{credit_accdec}{Access to and decisions on financial services (1 = adequate, 0 = inadequate)}
  \item{incomecontrol}{Control over use of income (1 = adequate, 0 = inadequate)}
  \item{work_balance}{Work balance (1 = adequate, 0 = inadequate)}
  \item{mobility}{Visiting important locations (1 = adequate, 0 = inadequate)}
  \item{groupmember}{Group membership (1 = adequate, 0 = inadequate)}
}
}
\source{Synthetically generated for package examples.}
\keyword{datasets}
