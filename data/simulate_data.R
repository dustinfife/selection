# set.seed(123)
# 
# require(flexplot)
# require(tidyverse)
# 
# ### simulate predictors
# rho = matrix(
#     c(1, .3, .2, .1,
#       .3, 1, .4, .2,
#       .2, .4, 1, .05,
#       .1, .2, .05, 1), nrow=4)
# d = data.frame(MASS::mvrnorm(555, c(0, 0, 0, 0), rho))
# names(d) = c("IQ", "Biodata", "Conscientiousness", "Interview")
# 
# ### simulate selection decision and optimal weights
# d$Selected = (simulate_logistic_two(optimal.weights = c(1.2, 2.4, 0, .6), d, .5, high_is_good = TRUE)$actual)
# d$Quit = (simulate_logistic_two(optimal.weights = c(2.4, 1.2, .6, 0), d[,1:4], .5, high_is_good = FALSE, noise=.5)$actual)
# 
# 
# ## create design matrix
# x = cbind(1, scale(d$IQ), scale(d$Biodata), scale(d$Conscientiousness), scale(d$Interview))
# 
# ## simulate normal criterion
# require(fifer)
# d$JP = round((x %*% t(t(c(1, 2.4, 1.2, .6, 0)))))
# d$JP = rescale(d$JP, 5, 1.5) + rnorm(nrow(d), 0, .4)
# 
# ### simulate absenteeism
# d$Absences = round(exp(x %*% t(t(c(1, 2.4, 1.2, .6, 0)*.5))))
# #hist(d$Absences)
# 
# ## simulate selection
# ### subset the data
# selection_data = d %>%
#     dplyr::select(IQ, Biodata, Conscientiousness, Interview, Absences, Quit, JP, Selected) %>%
#     mutate(Absences = ifelse(Selected==0, NA, Absences),
#            JP = ifelse(Selected==0, NA, JP),
#            Quit = ifelse(Selected==0, NA, Quit))
# 
# usethis::use_data(selection_data, overwrite=TRUE)