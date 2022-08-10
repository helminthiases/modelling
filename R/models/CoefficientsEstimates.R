# Title     : CoefficientsEstimates.R
# Objective : Table Coefficients
# Created by: greyhypotheses
# Created on: 10/08/2022


CoefficientsEstimatesBLM <- function (model, variables, parameters) {

  estimates <- summary(model)

  reference <- data.frame(variable = variables$strings, coefficient = parameters, row.names = variables$strings)

  coefficients <- estimates$coefficients %>% data.frame() %>% dplyr::select(!z.value)
  names(coefficients) <- c('est', 'SE', 'p.value')
  coefficients <- CoefficientConfidenceInterval(parameters = coefficients)

  coefficients <- base::merge(x = coefficients, y = reference, by = 0, all.x = TRUE, sort = FALSE)
  coefficients <- coefficients %>%
    dplyr::select('variable', 'coefficient', 'est', '2.5 %', '97.5 %', 'SE', 'p.value')
  coefficients$variable <- variables$labels

  return(coefficients)

}



CoefficientsEstimatesBLB <- function (model, variables, parameters) {

  estimates <- summary(model)

  reference <- data.frame(variable = variables$labels, coefficient = parameters, row.names = variables$strings)
  coefficients <- merge(x = estimates$beta, y = reference, by = 0, all.x = TRUE, sort = FALSE)
  coefficients <- coefficients %>%
    dplyr::select('variable', 'coefficient', 'Mean', 'Median', 'Mode', 'StdErr', 'HPD 0.025', 'HPD 0.975')
  row.names(coefficients) <- NULL

  return(coefficients)

}