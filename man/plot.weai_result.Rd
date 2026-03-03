\name{plot.weai_result}
\alias{plot.weai_result}
\title{Plot a weai_result Object}
\description{
Creates a stacked bar chart showing the absolute contribution of each
indicator to disempowerment, for women and men.
}
\usage{
\method{plot}{weai_result}(x, \dots)
}
\arguments{
  \item{x}{A \code{weai_result} object returned by \code{\link{weai}}.}
  \item{\dots}{Additional arguments (ignored).}
}
\value{A ggplot object (invisibly), or NULL for base R fallback.}
\seealso{\code{\link{weai}}}
