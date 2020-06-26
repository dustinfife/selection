#' Estimate the maximum value of a criterion under optimal selection
#'
#' @param formula A equation of the form y~x1 + x2... The Y is the value to be maximized by optimizing selection
#' @param data The dataset
#' @param method One of the following: "regression", "logistic", or "poisson", which specifies the type of model to fit
#' to the criterion
#' @param imputations Number of imputations. Defaults to 30. 
#' @param plot Should a plot be returned? Defaults to TRUE. 
#' @importFrom magrittr "%>%" 
#' @importFrom purrr map_dfr
#' @importFrom tidyr gather
#' @importFrom countimp countimp
#'
#' @return both a vector and a plot. 
#' 
#' For the plot, The plot shows a scatterplot. The horizontal line on the plot is the estimate 
#' (e.g., proportion who quit, mean of the criterion) under the current system. Each dot represents 
#' the estimate under a different imputation.
#' 
#' The vector returns the current mean (under the current selection system), the mean under the optimal system,
#' and the proportion of individuals the two methods (old versus optimal) agree on. The vector also returns the regression
#' coefficients under a model predicting who was selected based on the predictors. 
#' 
#' 
#' @export
#'
#' @examples
#' data(selection_data)
#' maximizeDV(Quit~IQ + Biodata + Conscientiousness + Interview, data=selection_data, method="logistic")
#' testthat::expect_error(maximizeDV(Absences~IQ + Biodata + Conscientiousness + Interview, data=selection_data, method="logistic"))
#' maximizeDV(Absences~IQ + Biodata + Conscientiousness + Interview, data=selection_data, method="poisson")
#' maximizeDV(JP~IQ + Biodata + Conscientiousness + Interview, data=selection_data)
maximizeDV = function(formula, data, method=c("regression", "logistic", "poisson"), imputations=30, plot=TRUE){
    
    method_used = match.arg(method)
    
    ## get variables
    vars = maximizeDV_varprep(formula, data)
    
    ## report errors
    maximizeDV_errorcheck(vars, data, method=method_used)
    
    ## subset data to only those available
    d = data[,vars$variables]
    
    ## build the imputation model
    if (method_used == "poisson"){
        imp = countimp::countimp(d, m=imputations, print=FALSE)
    } else if (method_used == "logistic"){
        d[,vars$dv] = factor(d[,vars$dv])
        imp = mice::mice(d, m=imputations, print=FALSE)#, method="logreg")
    } else {
        imp = mice::mice(d, m=imputations, print=FALSE, method="pmm")
    }
    
    reverse = ifelse(method_used=="logistic" | method_used == "poisson", TRUE, FALSE)
    
    results = 1:imputations %>% map_dfr(summarize_imputation, 
                                        imp=imp, 
                                        formula = formula, 
                                        pred_method=method_used, 
                                        vars=vars, 
                                        invert=reverse) 
    plotres = results %>% tidyr::gather("Method", "Estimate", current_mean_dv:new_mean_dv) %>% 
        dplyr::select(Method, Estimate)
    results$x = 1
    final_results = results %>% dplyr::summarize_all(list(mean))
    
    ## output regression models
    #browser()
    regression_models = summarize_imputation(i=1, 
                                             imp=imp, 
                                            formula=formula, 
                                            pred_method=method_used, 
                                            vars=vars, 
                                            invert=reverse, 
                                            models=TRUE)
    
    ### do a plot of the DV
    if (method=="logistic") ylab = paste0("Proportion who ", vars$dv) else ylab = vars$dv
    dv_plot = ggplot2::ggplot(results, ggplot2::aes(x=x,y=new_mean_dv)) +
        flexplot::geom_jitterd(width = .45) +
        ggplot2::geom_violin(alpha = 0.1) +
        ggplot2::theme_bw() +
        ggplot2::coord_cartesian(xlim=c(0, 2)) + 
        ggplot2::theme(axis.ticks = ggplot2::element_blank(), 
              axis.text.x = ggplot2::element_blank()) +
        ggplot2::labs(y=ylab, x="")
    
    ### get range of y axis
    plot_range = (ggplot2::layer_scales(dv_plot)$y$range$range)
    offset = .05*diff(plot_range)
    if (final_results$current_mean_dv + offset > plot_range[2]) {
        offset =  final_results$current_mean_dv - offset
        v = 1
    } else {
        offset = final_results$current_mean_dv + offset
        v = 0
    }
    dv_plot = dv_plot + ggplot2::geom_hline(yintercept = final_results$current_mean_dv) +
                ggplot2::geom_text(ggplot2::aes(x = 0, 
                                        y = offset, 
                                        label = "Old System"),
                                   hjust = 0,
                                   vjust = v) 
    
    a = list(current_mean = final_results$current_mean_dv, 
             optimal_mean = final_results$new_mean_dv, 
             percent_agreement = final_results$percent_agreement,
             optimal_model = regression_models$model_selected_optimal,
             current_model = regression_models$model_selected_current,
             plot = dv_plot)
    
    attr(a, "class") = "selection"
    if (plot){
        print(dv_plot)
        return(a)
    } else {
        return(a)
    }
}


#' Print selection Summary
#'
#' Print selection Summary
#' @aliases print.selection
#' @param x a selection object
#' @param ... ignored
#' @export
print.selection = function(x,...){
    cat(paste0("                           Current mean: ", round(x$current_mean, digits=3), "\n"))
    cat(paste0("  Estimated mean (under optimal system): ", round(x$optimal_mean, digits=3), "\n"))
    cat(paste0("Percent agreement (between two systems): ", round(x$percent_agreement, digits=3), "\n"))
    cat("\n")
    cat(paste0("Objects stored in this list:\n", paste0(names(x), collapse = ", ")))
}