# Title     : CoefficientsEstimates.R
# Objective : Table Coefficients
# Created by: greyhypotheses
# Created on: 10/08/2022



#' PrevMap Coefficient Estimates
#'
#' @description Creates the table of a model's estimated coefficients
#'
#' @param model: An object of class <PrevMap>
#' @param variables: list(strings = ..., labels = ...) for the table of a model's coefficient estimates
#'
#'               ... strings: The fixed terms of the <PrevMap> object, e.g. (Intercept)
#'               ... labels: The concurrent text forms for <strings>, e.g., I(x^2) -> $x^{2}$
#'
#' @param parameters: The concurrent coefficient estimate terms, i.e., '$\\beta_{0}$', etc., of <variables>
#'
CoefficientsEstimatesBLM <- function (model, variables, parameters) {

  estimates <- summary(model)

  reference <- data.frame(variable = variables$labels, coefficient = parameters, row.names = variables$strings)

  coefficients <- estimates$coefficients %>% data.frame() %>% dplyr::select(!z.value)
  names(coefficients) <- c('est', 'SE', 'p.value')
  coefficients <- CoefficientConfidenceInterval(parameters = coefficients)

  coefficients <- base::merge(x = coefficients, y = reference, by = 0, all.x = TRUE, sort = FALSE)
  coefficients <- coefficients %>%
    dplyr::select('variable', 'coefficient', 'est', '2.5 %', '97.5 %', 'SE', 'p.value')
  coefficients$variable <- variables$labels

  return(coefficients)

}



#' Bayes.PrevMap Coefficient Estimates
#'
#' @description Creates the table of a model's estimated coefficients
#'
#' @param model: An object of class <Bayes.PrevMap>
#' @param variables: list(strings = ..., labels = ...) for the table of a model's coefficient estimates
#'
#'               ... strings: The fixed terms of the <Bayes.PrevMap> object, e.g. (Intercept)
#'               ... labels: The concurrent text forms for <strings>, e.g., I(x^2) -> $x^{2}$
#'
#' @param parameters: The concurrent coefficient estimate terms, i.e., '$\\beta_{0}$', etc., of <variables>
#'
CoefficientsEstimatesBLB <- function (model, variables, parameters) {

  estimates <- summary(model)

  reference <- data.frame(variable = variables$labels, coefficient = parameters, row.names = variables$strings)
  coefficients <- merge(x = estimates$beta, y = reference, by = 0, all.x = TRUE, sort = FALSE)
  coefficients <- coefficients %>%
    dplyr::select('variable', 'coefficient', 'Mean', 'Median', 'Mode', 'StdErr', 'HPD 0.025', 'HPD 0.975')
  row.names(coefficients) <- NULL

  return(coefficients)

}
