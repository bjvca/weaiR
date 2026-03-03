\name{summary.weai_result}
\alias{summary.weai_result}
\alias{print.summary.weai_result}
\title{Summary of a weai_result Object}
\description{
Provides a detailed summary including indicator-level decomposition tables.
}
\usage{
\method{summary}{weai_result}(object, \dots)

\method{print}{summary.weai_result}(x, \dots)
}
\arguments{
  \item{object}{A \code{weai_result} object returned by \code{\link{weai}}.}
  \item{x}{A \code{summary.weai_result} object.}
  \item{\dots}{Additional arguments (ignored).}
}
\value{An object of class \code{"summary.weai_result"} (invisibly).}
\seealso{\code{\link{weai}}, \code{\link{print.weai_result}}}
