# Title     : Estimates.R
# Objective : Estimates
# Created by: greyhypotheses
# Created on: 10/08/2022



#' Estimates: GLMM
#'
#' @param data: a data set
#' @param expression: the mixed effects expression ->  fixed effects expression + random effects expression
#'
Estimates <- function (data, expression) {

  model <- glmer(formula = as.formula(object = paste0('cbind(positives, tests - positives) ~ ', expression)),
                 family = binomial(link = 'logit'),
                 control = glmerControl(optimizer = c('nlminbwrap', 'nlminbwrap')),
                 data = data)

  return(model)

}
