##' Estimate the unrestricted covariance matrix using the Pearson-Lawley's Multivariate Correction
##'	
##' @param data Either a data matrix containing the restricted dataset, or a variance covariance matrix of the selected covariances. 
##' The variables used for selected (the Xs) must be in the first \code{p}
##' columns, while the "incidental" variables are in the last n-p:p columns
##' @param p the number of columns used for selection
##' @param v.pp The variance-covariance matrix of the population selection variables
##' @seealso \code{\link{MASS}}
##' @references
##' @return \item{}{}
##' @author
##' @export
##' @examples
##' # do a simulation to demonstrate unbiasedness of it
##'	#### specify population correlation matrix
##'	cor.mat = matrix(c(
##'		1, .2, .4, .5,
##'		.2, 1, .3, .6,
##'		.4, .3, 1, .4,
##'		.5, .6, .4, 1), ncol=4)
##'	
##'		#### sample from that matrix
##'	d = data.frame(mvrnorm(100000, mu=rep(0, times=ncol(cor.mat)), Sigma=cor.mat)	)
##'	names(d) = c("X1", "X2", "Y1", "Y2")
##'	
##'		#### restrict the data according to X1 and X2
##'	d.r = d[which((d$X1)>0 & d$X2>0),]
##'	
##'	cor.mat.corrected = mv.correction(data=d.r, p=2, v.pp = cor.mat[1:2, 1:2])
##'	
##' # compare the two estimates
##'	cor.mat
##'	round(cor.mat.corrected, digits=4)
mv.correction = function(data, p, v.pp){

	### check if dim(v.pp) = p
	if (dim(v.pp)[1] != p | dim(v.pp)[2] != p){
		stop("The dimensions of v.pp do not equal p")
	}

	### if they supply a data matrix, convert to a variance covariance matrix
	if (ncol(var.covar) != nrow(var.covar)){
		var.covar = cov(var.covar)
	}
	
	### get sufficient estimates
	n = ncol(var.covar)
	vstar.pp = var.covar[1:p, 1:p]
	vstar.np.p = var.covar[(p+1):n, 1:p]
	vstar.np.np = var.covar[(p+1):n, (p+1):n]

	### output actual estimates
	correction.off.diag = vstar.np.p %*% solve(vstar.pp) %*% v.pp
	correction.diag = vstar.np.np - vstar.np.p%*%(solve(vstar.pp) - solve(vstar.pp)%*%v.pp%*%solve(vstar.pp))%*%t(vstar.np.p)
	v.prime = rbind(v.pp, correction.off.diag)
	v.prime2 = rbind(t(correction.off.diag), correction.diag)
	v.prime = cbind(v.prime, v.prime2)
	v.prime
}