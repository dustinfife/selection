##' Estimate Cohen's d Using Case IV
##'
##' Estimate Cohen's d Using Case IV
##'	
##' @param predictor.var The variable the researcher wishes to estimate subgroup differences for
##' @param grouping.var The categorical variable that indicates group membership
##' @param rxxa The reliability at the applicant (population) level
##' @param rxxi The reliability at the incumbent (selected) level
##' @param sd.x.pop The population standard deviation of x
##' @author Dustin Fife
##' @export
##' @examples
##' grouping.var = rep(c(1:2), times=50)
##' predictor.var = 30 + 5*grouping.var + rnorm(100, 0, 5)
##' dc4(predictor.var, grouping.var, rxxa=.8, rxxi=.7, sd.x.pop=7)
dc4 = function(predictor.var, grouping.var, rxxa, rxxi, sd.x.pop){

	#### remove missing values
	r.x = na.omit(predictor.var)
	r.g = na.omit(grouping.var)

	##### estimate d
	d.est = abs(d.computation(y=r.x, group = r.g))	
	dc = d.est/sqrt(rxxi)
		
	##### compute r
	r.est = cor(r.x, as.numeric(r.g))

	#### set up remaining parameters
	P = length(which(r.g==unique(r.g)[1]))/length(r.g)
	Q = 1-P
	ux = sd(r.x)/sd.x.pop
	ut = sqrt((ux^2-(1-rxxa))/rxxa)


	#### apply case IV
	rc4 = (dc/(ut*sqrt((P*Q)^(-1) + dc^2)))/sqrt(((1/ut^2)-1)*(dc/sqrt((P*Q)^(-1)+dc^2))^2 + 1)
	dc4 = rc4/sqrt(P*Q*(1-rc4^2))
	return(dc4)	
}


##' Estimate Cohen's d Using the EM Algorithm
##'
##' Estimate Cohen's d Using the EM Algorithm
##'	
##' @param data The dataset containing missing values
##' @param grouping.var The name of the grouping variable (surrounded by quotes)
##' @param rxxa Optional. The value of the reliability at the population level (to correct for unreliability)
##' @author Dustin Fife
##' @export
##' @importFrom mix prelim.mix
##' @importFrom mix em.mix
##' @importFrom mix getparam.mix
dc4 = function(data, grouping.var, rxxa=NULL){

		#### reorder data so grouping variable comes first
	g = which(names(data) %in% grouping.var)
	not.g = which(!(names(data) %in% grouping.var))
	data = data[,c(g, not.g)]

		#### identify location of grouping variable
	require(mix); 
	data[,1] = factor(data[,1])
	ss = mix::prelim.mix(data.matrix(data), 1)
	thetahat = mix::em.mix(ss)
	em.est = mix::getparam.mix(ss,thetahat, corr=TRUE)
	if (!is.null(rxxa)){
		em.d = diff(em.est$mu[outcome.var,])/em.est$sdv[outcome.var]/sqrt(rxx)
	} else {
		em.d = diff(em.est$mu[outcome.var,])/em.est$sdv[outcome.var]
	}
	return(em.d)
}


##' Correct Cohen's d Using the Pearson Lawley
##'
##' Correct Cohen's d Using the Pearson Lawley
##'	
##' @param selected.cov The selected variance/covariance matrix of all variables
##' @param pop.cov The population variance/covariance matrix of all variables used for selection
##' @param Q The proportion of applicants belonging to the minority group
##' @param grouping.var The index of the grouping variable (i.e., which column indicates the parameters for the grouping variable?)
##' @param outcome.var The index of the outcome variable (i.e., which column indicates the parameters for the outcome variable?)
##' @return The pearson lawley estimate of d
##' @author Dustin Fife
##' @export
dpl = function(selected.cov, pop.cov, Q, grouping.var, outcome.var){
	p = nrow(pop.cov)
	pl.r = cov2cor(mv.correction(selected.cov, v.pp=pop.cov, p=p))[grouping.var, outcome.var]
	not.Q = 1-P
	return(pl.r/sqrt(P*Q*(1-pl.r^2)))
}