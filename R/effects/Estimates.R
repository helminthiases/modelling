# Title     : Estimates.R
# Objective : Estimates
# Created by: greyhypotheses
# Created on: 10/08/2022



#' Estimates: GLMM
#'
#' @param data: a data set
#' @param expression: the mixed effects expression ->  fixed effects expression + random effects expression
#' @param variables: A list that identifies the names of the fields
#'                      list(identifier = ..., tests = ..., positives = ...)
#'                   in <data>.
#'
Estimates <- function (data, expression, variables) {

  data <- dplyr::rename(data, 'identifier' = variables$identifier,
                        'positives' = variables$positives, 'tests' = variables$tests)

  model <- glmer(formula = as.formula(object = paste0('cbind(positives, tests - positives) ~ ', expression)),
                 family = binomial(link = 'logit'),
                 control = glmerControl(optimizer = c('nlminbwrap', 'nlminbwrap')),
                 data = data)

  return(model)

}
