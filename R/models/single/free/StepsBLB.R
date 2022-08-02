# Title     : StepsBLB.R
# Objective : Steps BLB
# Created by: greyhypotheses
# Created on: 02/08/2022



#' Metrics: Binomial Logistic Bayes
#'
#' @param model: A model of class BayesPrevMap
#' @param training: Training data
#' @param testing: Testing data
#' @param initial: Initial settings
#'
StepsBLB <- function (model, training, testing, initial) {


  source(file = 'R/models/EvaluationMetrics.R')
  source(file = 'R/models/EvaluationGraphs.R')
  source(file = 'R/functions/StandardisedResidual.R')
  source(file = 'R/functions/EmpiricalVariogram.R')


  # Valuations (vis-à-vis training points) & Predictions (vis-à-vis testing points)
  T <- EvaluationMetricsBLB(model = model, training = training, testing = testing)
  valuations <- T$valuations
  predictions <- T$predictions


  # Is there still evidence of residual spatial correlation?
  # The standardised residuals of the differences/errors/residuals w.r.t. the training points
  # Subsequently, the empirical variogram measures & graph w.r.t. the standardised residual
  residues <- StandardisedResidual(design = model$D, observed = training$prevalence,
                                   estimated = valuations$prevalence$predictions)
  points <- EmpiricalVariogram(data = data.frame(residue = residues, x = training$x, y = training$y))
  spatial <- SpatialEvaluationGraphs(points = points, limit = 500)


  # Illustrating Accuracy: Diagonals
  estimates <- rbind(
    data.frame(prevalence = training$prevalence, prediction = valuations$prevalence$predictions, segment = 'training'),
    data.frame(prevalence = testing$prevalence, prediction = predictions$prevalence$predictions, segment = 'testing')
  )
  diagonal <- DiagonalEvaluationGraphs(estimates = estimates)


  # Bias, Error, Noise
  # Upcoming


  return(list(model = model, valuations = valuations, predictions = predictions, residues = residues,
              graph.spatial = spatial, graph.diagonal = diagonal, initial = initial))

}

