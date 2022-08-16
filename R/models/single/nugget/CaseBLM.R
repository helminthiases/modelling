# Title     : CaseBLM.R
# Objective : Case BLM
# Created by: greyhypotheses
# Created on: 16/08/2022


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

  # All
  estimations <- list(coefficients = coefficients,
                      special = special,
                      discrepancies = discrepancies)
  saveRDS(object = estimations, file = file.path(pathstr, 'estimations.rds'))

}