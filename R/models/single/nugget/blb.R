# Title     : MetricsBLB.R
# Objective : Metrics BLB
# Created by: greyhypotheses
# Created on: 31/07/2022


source(file = 'R/models/EvaluationMetrics.R')
source(file = 'R/models/EvaluationGraphs.R')
source(file = 'R/models/EvaluationVariogram.R')
source(file = 'R/models/CoefficientsEstimates.R')
source(file = 'R/functions/ErrorMetrics.R')



# Valuations (vis-à-vis training points) & Predictions (vis-à-vis testing points)
valuations <- EvaluationMetricsBLB(model = bayes$model, data = training, type = 'marginal')
predictions <- EvaluationMetricsBLB(model = bayes$model, data = testing, type = 'marginal')


# Illustrating Accuracy: Diagonals
diagonal <- DoubleDiagonalEvaluationGraphs(
  training_ = list(prediction = valuations$prevalence$predictions, prevalence = training$prevalence),
  testing_ = list(prediction = predictions$prevalence$predictions, prevalence = testing$prevalence))
diagonal


# Is there still evidence of residual spatial correlation?
# The standardised residuals of the differences/errors/residuals w.r.t. the training points
# Subsequently, the empirical variogram measures & graph w.r.t. the standardised residual
T <- EvaluationVariogram(
  model = bayes$model,
  data = data.frame(prevalence = training$prevalence, x = training$x, y = training$y,
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
ErrorMetrics(observed = training$prevalence, estimated = valuations$prevalence$predictions, name = 'training')
ErrorMetrics(observed = testing$prevalence, estimated = predictions$prevalence$predictions, name = 'testing')
