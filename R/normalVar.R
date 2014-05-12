normalVar <-
function(x, meanS, mean, sd){
	return((x-meanS)^2*dnorm(x, mean, sd))
	}
