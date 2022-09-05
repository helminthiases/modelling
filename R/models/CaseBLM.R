# Title     : CaseBLM.R
# Objective : Case BLM
# Created by: greyhypotheses
# Created on: 16/08/2022



#' Evaluating Binomial Logistic Bayes Models
#'
#' @param mcml: An object of class <PrevMap>
#' @param training: The training data segment
#' @param testing: The testing data segment
#' @param pathstr: The storage path
#' @param notes: list(strings = ..., labels = ..., parameters = ...) for the table of a model's
#'               coefficient estimates
#'
#'               ... strings: The fixed terms of the <PrevMap> object, e.g. (Intercept)
#'               ... labels: The concurrent text forms for <strings>, e.g., I(x^2) -> $x^{2}$
#'               ... parameters: The concurrent coefficient estimate terms, i.e., '$\\beta_{0}$', etc.
#'
CaseBLM <- function (mcml, training, testing, pathstr, notes) {


  source(file = 'R/models/EvaluationMetrics.R')
  source(file = 'R/models/EvaluationGraphs.R')
  source(file = 'R/models/EvaluationVariogram.R')
  source(file = 'R/models/CoefficientsEstimates.R')
  source(file = 'R/functions/ConfidenceInterval.R')
  source(file = 'R/functions/ErrorMetrics.R')



  # Valuations (vis-à-vis training points) & Predictions (vis-à-vis testing points)
  valuations <- EvaluationMetricsBLM(model = mcml$model, data = training, type = 'marginal')
  predictions <- EvaluationMetricsBLM(model = mcml$model, data = testing, type = 'marginal')



  # Illustrating Accuracy: Diagonals
  diagonal <- DoubleDiagonalEvaluationGraphs(
    training_ = list(prediction = valuations$prevalence$predictions, prevalence = training$prevalence),
    testing_ = list(prediction = predictions$prevalence$predictions, prevalence = testing$prevalence))
  ggsave(filename = file.path(pathstr, 'diagonal.pdf'),
         plot = diagonal, height = 285, width = 565, units = 'px', dpi = 85, scale = 1)



  # Is there still evidence of residual spatial correlation?
  # The standardised residuals of the differences/errors/residuals w.r.t. the training points
  # Subsequently, the empirical variogram measures & graph w.r.t. the standardised residual
  T <- EvaluationVariogram(
    model = mcml$model,
    data = data.frame(prevalence = training$prevalence, x = training$x, y = training$y,
                      estimate = valuations$prevalence$predictions))
  ggsave(filename = file.path(pathstr, 'variogram.pdf'),
         plot = T$graph, height = 310, width = 390, units = 'px', dpi = 95, scale = 1)



  # Coefficients
  variables <- list(strings = notes$strings, labels = notes$labels)
  parameters <- notes$parameters
  coefficients <- CoefficientsEstimatesBLM(model = mcml$model, variables = variables, parameters = parameters)

  # Special
  special <- SpecialConfidenceInterval(estimates = summary(mcml$model))
  special$parameter <- c('$ln(\\sigma^2)$','$ln(\\phi)$', '$ln(\\tau^2)$')
  row.names(special) <- NULL
  special <- special %>% dplyr::select('parameter', 'estimate', 'lower_ci', 'upper_ci',
                                       'exp(estimate)', 'exp(lower_ci)', 'exp(upper_ci)')

  # Bias & RMSE
  discrepancies <- rbind(
    ErrorMetrics(observed = training$prevalence, estimated = valuations$prevalence$predictions, name = 'training'),
    ErrorMetrics(observed = testing$prevalence, estimated = predictions$prevalence$predictions, name = 'testing'))
  discrepancies <- cbind(variable = row.names(discrepancies), discrepancies)
  row.names(discrepancies) <- NULL

  # All
  estimations <- list(coefficients = coefficients,
                      special = special,
                      discrepancies = discrepancies,
                      model = mcml$model)
  saveRDS(object = estimations, file = file.path(pathstr, 'estimations.rds'))

}