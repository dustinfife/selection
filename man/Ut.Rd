% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Ut.R
\name{Ut}
\alias{Ut}
\title{Compute the unrestricted variance of T.}
\usage{
Ut(rxxi, rxxa, ux)
}
\arguments{
\item{rxxi}{the reliability estimate of x in the incumbent (restricted) population}

\item{rxxa}{the reliability estimate of x in the applicant (unrestricted) population}

\item{ux}{The unrestricted variance of X}
}
\value{
a value indicating the unresticted variance of T
}
\description{
Given reliability estimates in the population and in the sample, as well as the variance of X, \code{Ut} estimates
the unrestricted variance of T.
}
\examples{
Ut(.6, .8, 1.5)
}
\seealso{
\code{\link{caseIV}}
}
\author{
Dustin Fife
}
