turnover.rate = function(table){
	table = matrix(table, nrow=2)
	return(table[2,2]/(table[1,2]+table[2,2]))
}

#### alter weights
alter_weights = function(actual.weights, optimal.weights){
    if (actual.weights=="same"){
        actual.weights = optimal.weights
    } else if (actual.weights=="close"){
        actual.weights = optimal.weights[c(2,1,3,4)]
    } else {
        actual.weights = rev(optimal.weights)
    }
    return(actual.weights)
}  
alter_weights("bad", c(0, .1, .2, .3))
alter_weights("same", c(0, .1, .2, .3))
alter_weights("close", c(0, .1, .2, .3))


#n, predictor.matrix, optimal.weights, actual.weights, quit.ratio, selection.ratio, rsq, iterations
simulate_data_turnover = function(n, predictor.matrix, optimal.weights, actual.weights, quit.ratio, selection.ratio, rsq, iterations){

	#### do weights
	if (actual.weights=="same"){
		actual.weights = optimal.weights
	} else if (actual.weights=="close"){
		actual.weights = optimal.weights[c(2,1,3,4)]
	} else {
		actual.weights = rev(optimal.weights)
	}

	d = data.frame(MASS::mvrnorm(n, mu=rep(0, times=ncol(predictor.matrix)), Sigma=predictor.matrix))
	names(d) = paste0("X", 1:ncol(predictor.matrix))
	
	#### 2. simulate "turnover" based on arbitrary weights of predictors (using multiple regression)
	res = simulate_logistic(optimal.weights, d)
	

	
	#### 3. aggregate predictors "holistically" by placing more weight on some poor predictors and select
	#### figure out total variance of X (if it's explained perfectly)
	variances = t(actual.weights) %*% t(t(actual.weights)) 
	covs = sum(2*OpenMx::vechs(predictor.matrix) * apply(combn(actual.weights,2), 2, prod))
	total = variances + covs

	selection.weights = c(t(t(c(0, actual.weights))%*%X))
	total/rsq
	error.var = sqrt((total/rsq )- total)
	d$suitability = selection.weights + rnorm(nrow(d), 0, error.var)	### add weight to remove perfect separation	
	var(d$suitability)
	summary(lm(suitability~X1 + X2 +X3 + X4, data=d))$r.squared

	d$selected = cut(d$suitability, 
	                 quantile(d$suitability,c(0,selection.ratio,1)), labels=c(0,1), include.lowest = TRUE, include.highest=TRUE) %>% 
	                as.character %>% as.numeric
	                
	return(d)	         
}

design_matrix = function(optimal.weights, d){
    betas = t(c(0, optimal.weights))	### simulate zero for interview performance
    X = t(cbind(1, d))  
    list(X=X, betas=betas)
}

simulate_logistic = function(optimal.weights, d, quit.ratio){
    #browser()
    k = design_matrix(optimal.weights, d[,c("X1", "X2", "X3", "X4")]); betas = -1*k$betas; X=k$X
    quit.prob = as.numeric(1/(1 + exp(-1*t(betas %*% X))))  ### probability of quitting
    quit = sapply(quit.prob, quit.decision)               ### actually quitting
    list(quit.prob=quit.prob, quit.actual = quit, X=X)
}


# higher means more qualified
simulate_logistic_two = function(optimal.weights, d, quit.ratio, high_is_good=FALSE, noise = .2){
  #browser()
  if (high_is_good) inv = 1 else inv = -1
  k = design_matrix(optimal.weights, d); betas = inv*k$betas; X=k$X
  
  ### add noise (to avoid perfect separation)
  pred_log_odds = betas %*% X
  if (noise>0) {
    pred_log_odds = pred_log_odds + rnorm(nrow(d), 0, noise*sd(pred_log_odds))
  }
  prob = as.numeric(1/(1 + exp(-1*t(pred_log_odds))))  ### probability of quitting
  #if (high_is_good){
    actual = (prob>quantile(prob, quit.ratio))*1               ### actually quitting
  #} else {
  #  actual = (prob<quantile(prob, quit.ratio))*1               ### actually quitting
  #}
  list(prob=prob, actual = actual, X=X)
}



simulate_poisson = function(optimal.weights, d, intercept = 0){
    #browser()
    k = design_matrix(optimal.weights, d); betas = k$betas*(-.5); X=k$X; betas[1] = intercept
    absences = as.numeric(exp(betas %*% X))
    absences = round(absences)               ### actually quitting
    #selected.optimal = cut(quit.prob, c(0,quit.ratio,1), labels=c(0,1), include.lowest = TRUE)  
    absences
}
	
quit.decision = function(x) {if (x>runif(1)) 1 else 0} 

suitability_judgment = function(weights, rho, X){
  #### figure out total variance of X (if it's explained perfectly)
  variances = t(weights) %*% t(t(weights)) 
  covs = sum(2*OpenMx::vechs(rho) * apply(combn(weights,2), 2, prod))
  total = variances + covs
  selection.weights = c(t(t(c(0, weights))%*%X))
  error.var = sqrt((total/rsq )- total)
  suitability = selection.weights + rnorm(nrow(X), 0, error.var)	### add weight to remove perfect separation	
  selected = cut(suitability, 
                   quantile(suitability,c(0,selection.ratio,1)), 
                    labels=c(0,1), include.lowest = TRUE, include.highest=TRUE) %>% 
                    as.character %>% as.numeric
  list(suitability=suitability, selected=selected)
}

simulate_estimation_turnover = function(d){	
    #browser()
	#### make data missing

  # make the quit variable missing for those not selected  
	s = d[,c(paste0("X", 1:4), "quit", "selected", "selected.optimal", "absences", "suitability")]
	s$quit[s$selected==0] = NA


	#### 4. compute turnover rate
	turnover.selected = turnover.rate(with(s, table(quit, selected)))
	turnover.optimal = turnover.rate(with(d, table(quit, selected.optimal)))

	#### compute median absenteeism
	absent.selected = s %>% filter(selected==1) %>% summarize(mn = mean(absences))
	absent.optimal = d %>% filter(selected.optimal==1) %>% summarize(mn = mean(absences))
	
	##### estimate probability of quitting from the sample that was hired
	quit.samp = glm(quit~X1 + X2 + X3 + X4, data=s, family=binomial)
	quit.pop = glm(quit~ X1 + X2 + X3 + X4, data=d, family=binomial)
	
	##### estimate predicted absences from the sample hired (versus whole sample)
	absences.samp = glm(absences~X1 + X2 + X3 + X4, data=s, family=poisson)
	absences.pop = glm(absences~X1 + X2 + X3 + X4, data=d, family=poisson)

	pop.coefs = coef(quit.pop)
	samp.coefs = coef(quit.samp)
	
	pop.coefs.pois = coef(absences.pop)
	samp.coefs.pois = coef(absences.samp)	
	
	
	##### impute data to figure out accuracy
	imp.obj = suppressWarnings(mice::mice(s %>% mutate(quit = factor(quit)), method="logreg", printFlag=F, m=10))
	require(countimp)
	imp.pois <- suppressWarnings(mice::mice(s, method = c("pois"), print = FALSE, m=10))
    
	i = 1
	#### predict turnover rate under new system
	#### or predict what it would have been had we used that in the first place
	f = function(i){

		### extract new data to predict who would have quit
		new.data = mice::complete(imp.obj, i)
		selection.ratio = length(which(!is.na(s$quit)))/nrow(s)
		
		### estimate the fitted model from the imputed data
		#quit.samp = glm(quit~X1 + X2 + X3 + X4, data=new.data, family=binomial)

		### come up with new selection based on estimated optimal system
		s$suitability.new = -1*predict(quit.samp, newdata=new.data)
		s$selected.new = cut(s$suitability.new, 
		                 quantile(s$suitability.new,c(0,selection.ratio,1)), 
		                 labels=c(0,1), include.lowest = TRUE, include.highest=TRUE) %>% 
		                  as.character %>% as.numeric
		

    # flexplot(selected.optimal~X1, data=s)
		# flexplot(selected.new~X1, data=s)
		# table(imputed = new.data$selected.new, actual = d$selected.optimal)
		### estimate turnover rate
		mean.estimates = (turnover.rate(table(quit=new.data$quit, selected = s$selected.new)))
		sd.estimates = mean.estimates*(1-mean.estimates)
		#list(mean.estimates, sd.estimates)
		mean.estimates		##### NOTE: update this to follow rubin's imputation rules
	}
	
	### this function generates new predictions for absenteeism, then
	  # simulates selection based on absenteeism scores

	f.pois = function(i){
	  new.data = mice::complete(imp.pois, i)
	  
	  ### sort according to predicted absences
	  s$absences.new = (predict(absences.samp, newdata=new.data, type="response"))
	  s = s[order(s$absences.new),]
	  head(s)
	  ### select top N scores
	  top.n = s$absences.new[1:length(which(s$selected==1))]
	  mean.absences = mean(top.n)
	  sd.absences = sd(top.n)
    mean.absences
	}
	
	
	
	#### estimate turnover rate across 10 imputations
	turnover.estimated = 1:10 %>% map_dbl(f) %>% mean
  absences.estimated = 1:10 %>% map_dbl(f.pois) %>% mean
	
	#### rubin's rules
	mean.turnover = mean(turnover.estimated)
	#sd.turnover = sd(a) + sqrt()

	##### estimate probability of getting hired from the sample that was hired
	hired.samp = glm(selected~X1 + X2 + X3 + X4, data=s, family=binomial)
	hired.samp.pois = glm(absences~X1 + X2 + X3 + X4, data=s, family=poisson)
	
	return(c(turnover.selected= turnover.selected, turnover.optimal= turnover.optimal, turnover.estimated = mean.turnover, 
	         absent.selected = as.numeric(absent.selected), absent.optimal = as.numeric(absent.optimal), absent.estimated = absences.estimated))
	         #coefsLog.optimal= pop.coefs, coefsLog.selected= samp.coefs,
	         #coefsPois.optimal = pop.coefs.pois, coefsPois.selected = samp.coefs.pois)

	#return(c(turnover.selected, turnover.optimal, turnover.estimated))
}


simulate_estimation_absent = function(d){	
    #browser()
    #### make data missing
    s = d[,c(paste0("X", 1:4), "quit", "selected", "absences")]
    s$quit[s$selected==0] = NA
    
    head(s)
    #### 4. compute turnover rate
    absent.selected = s %>% mutate(selected = factor(selected)) %>% 
        group_by(selected) %>% summarize(total_absences = sum(absences),
                                         total_people = length(absences),
                                         absence_per_person = total_absences/total_people)
    absent.selected
    
    
    ##### estimate absenteeism from the sample that was hired
    abs.samp = glm(absences~X1 + X2 + X3 + X4, data=s, family=poisson)
    abs.pop = glm(absences~ X1 + X2 + X3 + X4, data=d, family=poisson)
    
    pop.coefs = coef(abs.pop)
    samp.coefs = coef(abs.samp)

    ##### impute data to figure out accuracy
    imp.obj = countimp::countimp(s, method="poisson", m=10)
    imp.obj = suppressWarnings(mice::mice(s %>% mutate(quit = factor(quit)), method="logreg", printFlag=F, m=10))
    #?mice.impute.poisson
    #### summarize the imputations
    f = function(i){
        
        ### extract new data to predict who would have quit
        new.data = mice::complete(imp.obj, i)
        
        ### come up with new selection based on optimal system
        s$selected.new = round(predict(quit.samp, newdata=new.data, type="response"))
        
        
        ### estimate turnover rate
        mean.estimates = (turnover.rate(table(quit=new.data$quit, selected = s$selected.new)))
        sd.estimates = mean.estimates*(1-mean.estimates)
        #list(mean.estimates, sd.estimates)
        mean.estimates		##### NOTE: update this to follow rubin's imputation rules
    }
    
    #### estimate turnover rate across 10 imputations
    turnover.estimated = 1:10 %>% map_dbl(f) %>% mean
    
    
    #### rubin's rules
    mean.turnover = mean(turnover.estimated)
    #sd.turnover = sd(a) + sqrt()
    
    ##### estimate probability of getting hired from the sample that was hired
    hired.samp = glm(selected~X1 + X2 + X3 + X4, data=s, family=binomial)
    return(c(turnover.selected= turnover.selected, turnover.optimal= turnover.optimal, turnover.estimated = mean.turnover, coefs= pop.coefs, samp.coefs= samp.coefs, hired.samp= coef(hired.samp)))
    #return(c(turnover.selected, turnover.optimal, turnover.estimated))
}


