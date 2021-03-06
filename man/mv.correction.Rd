% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mv.correction.R
\name{mv.correction}
\alias{mv.correction}
\title{Estimate the unrestricted covariance matrix using the Pearson-Lawley's Multivariate Correction}
\usage{
mv.correction(data, p, v.pp)
}
\arguments{
\item{data}{Either a data matrix containing the restricted dataset, or a variance covariance matrix of the selected covariances. 
The variables used for selected (the Xs) must be in the first \code{p}
columns, while the "incidental" variables are in the last n-p:p columns}

\item{p}{the number of columns used for selection}

\item{v.pp}{The variance-covariance matrix of the population selection variables}
}
\description{
Estimate the unrestricted covariance matrix using the Pearson-Lawley's Multivariate Correction
}
\examples{
# do a simulation to demonstrate unbiasedness of it
#### specify population correlation matrix
cor.mat = matrix(c(
	1, .2, .4, .5,
	.2, 1, .3, .6,
	.4, .3, 1, .4,
	.5, .6, .4, 1), ncol=4)

	#### sample from that matrix
d = data.frame(mvrnorm(100000, mu=rep(0, times=ncol(cor.mat)), Sigma=cor.mat)	)
names(d) = c("X1", "X2", "Y1", "Y2")

	#### restrict the data according to X1 and X2
d.r = d[which((d$X1)>0 & d$X2>0),]

cor.mat.corrected = mv.correction(data=d.r, p=2, v.pp = cor.mat[1:2, 1:2])

# compare the two estimates
cor.mat
round(cor.mat.corrected, digits=4)
# use example I in Birnbaum, Paulson, and Andrews (1950)
## input population correlation matrix
cor = matrix(c(1, .703, .527, .499, .777,
			.703, 1, .404, .555, .679,
			.527, .404, 1, .253, .578,
			.499, .555, .253, 1, .427,
			.777, .679, .578, .427, 1), nrow=5)
v = cor2cov(cor, sd=c(9.93, 9.39, 8.80, 7.19, 8.05))
### change order so selection variables occur first
ord = c(5,1,2,3,4)
v = v[ord, ord]

# input observed correlation matrix
v.star = matrix(c(
	43, 37.65, 37.24, 25.59, 14.46,
	37.65, 72.24, 48.95, 29.04, 23.90,
	37.24, 48.95, 82.60, 20.10, 31.24,
	25.59, 29.04, 20.10, 68.04, 4.71,
	14.46, 23.90, 31.24, 4.71, 48.31), ncol=5)
mv.correction(v.star, p=2, v.pp = v[1:2, 1:2])
}
\references{
Birnbaum, Z. W., Paulson, E., & Andrews, F. C. (1950). On the effect of selection performed on some coordinates of a multi-dimensional population. Psychometrika, 15(2), 191-204.
}
\author{
Dustin Fife
}
