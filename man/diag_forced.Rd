% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/diagnostics.R
\name{diag_forced}
\alias{diag_forced}
\title{Forced distribution test.}
\usage{
diag_forced(sorts, grid)
}
\arguments{
\item{grid}{A positive integer vector of a length covering the range of values, specifying maximum allowed counts for each value.
(in Q-parlance, the maximum column heights for the Q-sorts).}
}
\value{
A logical value of length 1, \code{TRUE} if the distribution is forced, else \code{FALSE}.
}
\description{
Test whether all people-variables completely fill out the same grid.
}
\details{
If all people-variables display the same value counts, respondents plausibly faced a \emph{forced} distribution, but this does not strictly speaking follow.
It is unlikely, though conceivable, that respondents all adhered to precisely the same distribution, but \emph{could} have used other available slots.

Whether, or not, a Q dataset should be considered "forced" can therefore only ascertained from a comparison with the physically available grid during the administration of the Q-sort.

In the above, improbable case, the function warns the user of possible misspecification.
}
\examples{
sorts <- civicon_2014$qData$sorts[,,"before"]
diag_forced(sorts = sorts,
            grid = diag_distro_max(sorts))

}
\concept{distribution diagnostic functions}
\keyword{internal}
