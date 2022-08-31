# Title     : blm.R
# Objective : Binomial Logistic MCML
# Created by: greyhypotheses
# Created on: 10/08/2022



source(file = '../../EvaluationMetrics.R')
source(file = '../../EvaluationGraphs.R')
source(file = '../../EvaluationVariogram.R')
source(file = '../../CoefficientsEstimates.R')
source(file = '../../../functions/ConfidenceInterval.R')
source(file = '../../../functions/ErrorMetrics.R')



# Valuations (vis-à-vis excerpt)
valuations <- EvaluationMetricsBLM(model = mcml$model, data = excerpt, type = 'marginal')


# Illustrating Accuracy: Diagonals
diagonal <- SingleDiagonalEvaluationGraphs(
  data_ = list(prediction = valuations$prevalence$predictions, prevalence = excerpt$prevalence))
diagonal


# Is there still evidence of residual spatial correlation?
# The standardised residuals of the differences/errors/residuals w.r.t. the excerpt points
# Subsequently, the empirical variogram measures & graph w.r.t. the standardised residual
T <- EvaluationVariogram(
  model = mcml$model,
  data = data.frame(prevalence = excerpt$prevalence, x = excerpt$x, y = excerpt$y,
                    estimate = valuations$prevalence$predictions))
T$graph


# Coefficients
variables <- list(strings = c('(Intercept)', 'piped_sewer', 'elevation.km'),
                  labels = c('(Intercept)', 'piped_sewer', 'elevation.km'))
parameters <- c('$\\beta_{0}$', '$\\beta_{1}$', '$\\beta_{2}$')
coefficients <- CoefficientsEstimatesBLM(model = mcml$model, variables = variables, parameters = parameters)
coefficients


# Special
special <- SpecialConfidenceInterval(estimates = summary(mcml$model))
special$parameter <- c('$ln(\\sigma^2)$','$ln(\\phi)$', '$ln(\\tau^2)$')
row.names(special) <- NULL
special <- special %>% dplyr::select('parameter', 'estimate', 'lower_ci', 'upper_ci',
                                     'exp(estimate)', 'exp(lower_ci)', 'exp(upper_ci)')
special


# Bias & RMSE
ErrorMetrics(observed = excerpt$prevalence, estimated = valuations$prevalence$predictions, name = 'excerpt')
