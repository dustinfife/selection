\name{caseIV}
\alias{caseIV}
\title{Correct x-y correlations using Case IV}
\usage{
  caseIV(rtpi, rxxi, rxxa, ux)
}
\arguments{
  \item{rtpi}{the correlation between t and p in the
  incumbent pool.}

  \item{rxxi}{the reliability of x in the incumbent pool.}

  \item{rxxa}{the reliability of x in the applicant pool.}

  \item{ux}{the ratio of selected to unselected variance in
  x.}
}
\value{
  a scalar that is the estimate of the correlation between
  t and p.
}
\description{
  \code{caseIV} uses Hunter and Schmidt's (2004) correction
  procedure to estimate the correlation between t and p,
  assuming direct selection on a latent s.
}
\details{
  I'll do this later.
}
\examples{
# correct an xy correlation for unreliability
	rtpi = rel.correction(rxyi=.3, rxxi=.8, ryyi=.6)

	# estimate Case IV
	caseIV(rtpi=rtpi, rxxi=.6, rxxa=.8, ux=.8)
}
\author{
  Dustin Fife
}
\references{
  Hunter, J. E., & Schmidt, F. L. (2004). Methods of
  meta-analysis: Correcting error and bias in research
  findings. Thousand Oaks, CA: Sage.

  Hunter, J. E., & Schmidt, F. L., & Le, H. (2006).
  Implications of direct and indirect range restriction for
  meta-analysis methods and findings. Journal of Applied
  Psychology, 91(3), 594–612.
  doi:10.1037/0021-9010.91.3.594
}
\seealso{
  See Also \code{\link{rel.correction}} to obtain estimates
  of rtpi from rxyi, \code{\link{caseIII}},
  \code{\link{em}}, \code{\link{caseIIIR}}
}

