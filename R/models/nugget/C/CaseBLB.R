# Title     : blb.R
# Objective : Binomial Logistic Bayes
# Created by: greyhypotheses
# Created on: 10/08/2022



source(file = '../../EvaluationMetrics.R')
source(file = '../../EvaluationGraphs.R')
source(file = '../../EvaluationVariogram.R')
source(file = '../../CoefficientsEstimates.R')
source(file = '../../../functions/ErrorMetrics.R')



# Valuations (vis-à-vis excerpt)
valuations <- EvaluationMetricsBLB(model = bayes$model, data = excerpt, type = 'marginal')


# Illustrating Accuracy: Diagonals
diagonal <- SingleDiagonalEvaluationGraphs(
  data_ = list(prediction = valuations$prevalence$predictions, prevalence = excerpt$prevalence))
diagonal


# Is there still evidence of residual spatial correlation?
# The standardised residuals of the differences/errors/residuals w.r.t. the excerpt points
# Subsequently, the empirical variogram measures & graph w.r.t. the standardised residual
T <- EvaluationVariogram(
  model = bayes$model,
  data = data.frame(prevalence = excerpt$prevalence, x = excerpt$x, y = excerpt$y,
                    estimate = valuations$prevalence$predictions))
T$graph


# Coefficients
variables <- list(strings = c('(Intercept)', 'piped_sewer', 'I(piped_sewer^2)' , 'elevation.km'),
                  labels = c('(Intercept)', 'piped_sewer', 'I(piped_sewer$^{2}$)' , 'elevation.km'))
parameters <- c('$\\beta_{0}$', '$\\beta_{1}$', '$\\beta_{2}$', '$\\beta_{3}$')
coefficients <- CoefficientsEstimatesBLB(model = bayes$model, variables = variables, parameters = parameters)
coefficients


# Special
estimates <- summary(bayes$model)
labels <- c('$\\sigma^2$','$\\phi$', '$\\tau^2$')
special <- data.frame(label = labels, rbind(estimates$sigma2, estimates$phi, estimates$tau2))
special


# Bias & RMSE
ErrorMetrics(observed = excerpt$prevalence, estimated = valuations$prevalence$predictions, name = 'excerpt')

