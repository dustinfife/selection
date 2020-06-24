summarize_imputation = function(i, imp, formula, pred_method, vars, invert=FALSE){
    #browser()
    new.data = mice::complete(imp, i)
    old.data = imp$data
    
    ### figure out who was selected before
    selected.before = !is.na(old.data[[vars$dv]])
    new.data$selected_old = selected.before
    
    family_method = switch(pred_method, "regression" = gaussian, "logistic" = binomial, "poisson" = poisson)
    
    ## build selection model(s)
    model_selected_optimal = glm(formula, data=new.data, family=family_method)
    formula2 = as.formula(paste0("selected_old", "~", paste0(vars$ivs, collapse="+")))
    model_selected_current = flexplot::estimates(glm(formula2, data=new.data, family=binomial))
    
    ### sort according to predicted absences
    new.data$suitability_new = predict(model_selected_optimal, newdata=new.data, type="response")
    neworder = order(new.data$suitability_new, decreasing = !invert)
    new.data = new.data[neworder,]
    old.data = old.data[neworder,]
    head(new.data)
    
    ### select top N scores
    top.n = nrow(new.data) - imp$nmis[vars$dv]
    
    ### figure out which were "selected" before and now
    selected.before = !is.na(old.data[[vars$dv]])
    selected.new = rep(FALSE, times=nrow(new.data)); selected.new[1:top.n] = TRUE
    selection_differences = table(selected.new, selected.before)
    selection_differences = sum(diag(selection_differences))/sum(selection_differences)
    
    if (pred_method == "logistic"){
        quit = new.data[[vars$dv]]
        quit_old = old.data[[vars$dv]]
        selected = selected.new*1
        selected.old = old.data[[!is.na(vars$dv)]]
        sum.tab = table(quit=quit, selected=selected)
        sum.tab.old = table(quit = quit_old)
        mean_dv = sum.tab[2,2]/sum(sum.tab[,2])
        mean_dv_old = sum.tab.old[2]/sum(sum.tab.old)
    } else {
        mean_dv =  mean(new.data[1:top.n,vars$dv])
        mean_dv_old = mean(old.data[,vars$dv], na.rm=TRUE)
    }
    
    return.list = data.frame(t(unlist(list(
        mean_dv_old = mean_dv_old, 
        mean_dv = mean_dv, 
        percent_agreement=selection_differences, 
        model_selected_current$raw.coefficients))))
    names(return.list) = c("current_mean_dv", "new_mean_dv", "percent_agreement", row.names(model_selected_current))
    return.list
}


maximizeDV_varprep = function(formula, data){
    variables = all.vars(formula)
    dv = variables[1]
    ivs = variables[-1]
    list(variables=variables, dv=dv, ivs=ivs)
}

maximizeDV_errorcheck = function(vars, data, method){

    notthere = !(vars$variables %in% names(data))
    if (sum(notthere)>0){
        stop("The variables ", vars$variables[notthere], " are not in the dataset!")
    }
    
    if (method == "logistic" & length(unique(data[!is.na(data[,vars$dv]),vars$dv]))!=2){
        stop("To use 'logistic', your criterion needs to have only two levels!")
    }
}

