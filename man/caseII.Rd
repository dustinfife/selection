% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/caseII.R
\name{caseII}
\alias{caseII}
\title{Corrects correlations using Case II}
\usage{
caseII(rxy, sx, sxs)
}
\arguments{
\item{rxy}{the restricted correlation between x (the indirectly selected
variable) and y (the outcome variable).}

\item{sx}{the unrestricted standard deviation of x}

\item{sxs}{the restricted standard deviation of x}
}
\value{
a scalar that is the unbiased estimate of the correlation between X
and Y.
}
\description{
Using Thorndike's Case II correction, \code{caseIII} corrects the xy
correlation for direct restriction on x
}
\details{
The Case II correction is defined as follows insert later The result is an
unbiased estimate of the unattenuated correlation between X and Y
}
\examples{

	# correct assuming direct selection on X
corrected = caseII(rxy=.5, sx=1.5, sxs=.9)	
corrected

}
\references{
Thorndike, R. L. (1949). Personnel selection: Test and
measurement techniques. Oxford, England: Wiley.

Pearson, K. (1903). Mathematical contributions to the theory of evolution.
XI. On the influence of natural selection on the variability and correlation
of organs. Philosophical Transactions of the Royal Society of London. Series
A, Containing Papers of a Mathematical or Physical Character, 200, 1-66.
}
\seealso{
\code{\link{caseIV}}, \code{\link{caseIIIR}}, \code{\link{em}},
\code{\link{rel.correction}}
}
\author{
Dustin Fife
}
