# Title     : StepsBLM.R
# Objective : Steps BLM
# Created by: greyhypotheses
# Created on: 31/07/2022



StepsBLM <- function (training, testing, terms, variables) {

  source(file = 'R/single/BinomialLogisticMCML.R')
  source(file = 'R/single/EvaluationMetricsBLM.R')
  source(file = 'R/single/Graphs.R')
  source(file = 'R/functions/StandardisedResidual.R')
  source(file = 'R/functions/EmpiricalVariogram.R')



  # Architecture
  cat(paste0('positive ~ ', {{terms}}))



  # Model
  objects <- BinomialLogisticMCML(data = training, terms = terms, variables = variables)
  model <- objects$model



  # Valuations (vis-à-vis training points) & Predictions (vis-à-vis testing points)
  T <- EvaluationMetricsBLM(model = model, training = training, testing = testing)
  valuations <- T$valuations
  predictions <- T$predictions



  # Is there still evidence of residual spatial correlation?
  # The standardised residuals of the differences/errors/residuals w.r.t. the training points
  residues <- StandardisedResidual(design = model$D, observed = training$prevalence,
                                   estimated = valuations$prevalence$predictions)

  # The empirical variogram measures & graph w.r.t. the standardised residual
  points <- EmpiricalVariogram(data = data.frame(residue = residues, x = training$x, y = training$y))
  spatial <- SpatialGraphs(points = points, limit = 500)



  # Illustrating Accuracy: Diagonals
  estimates <- rbind(
    data.frame(prevalence = training$prevalence, prediction = valuations$prevalence$predictions, segment = 'training'),
    data.frame(prevalence = testing$prevalence, prediction = predictions$prevalence$predictions, segment = 'testing')
  )
  diagonal <- DiagonalGraphs(estimates = estimates)



  # Bias, Error, Noise
  #



  return(list(model = model, valuations = valuations, predictions = predictions, residues = residues,
              graph.spatial = spatial, graph.diagonal = diagonal ))

}