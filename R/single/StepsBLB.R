# Title     : StepsBLB.R
# Objective : Steps BLB
# Created by: greyhypotheses
# Created on: 31/07/2022



StepsBLB <- function (data, terms, variables) {

  source(file = 'R/single/BinomialLogisticBayes.R')
  source(file = 'R/single/EvaluationMetricsBLB.R')
  source(file = 'R/single/Graphs.R')
  source(file = 'R/functions/StandardisedResidual.R')
  source(file = 'R/functions/EmpiricalVariogram.R')



  # Architecture
  cat(paste0('positive ~ ', {{terms}}))



  # Model
  objects <- BinomialLogisticBayes(data = excerpt, terms = terms, variables = variables)
  model <- objects$model



  # Valuations (vis-à-vis training points) & Predictions (vis-à-vis testing points)
  T <- EvaluationMetricsBLB(model = model, excerpt = excerpt, testing = testing)
  valuations <- T$valuations
  predictions <- T$predictions



  # Is there still evidence of residual spatial correlation?
  # The standardised residuals of the differences/errors/residuals w.r.t. the training points
  residues <- StandardisedResidual(design = model$D, observed = excerpt$prevalence,
                                   estimated = valuations$prevalence$predictions)

  # The empirical variogram measures & graph w.r.t. the standardised residual
  points <- EmpiricalVariogram(data = data.frame(residue = residues, x = excerpt$x, y = excerpt$y))
  spatial <- SpatialGraphs(points = points, limit = 500)



  # Illustrating Accuracy: Diagonals
  estimates <- rbind(
    data.frame(prevalence = excerpt$prevalence, prediction = valuations$prevalence$predictions, segment = 'training'),
    data.frame(prevalence = testing$prevalence, prediction = predictions$prevalence$predictions, segment = 'testing')
  )
  diagonal <- DiagonalGraphs(estimates = estimates)



  # Bias, Error, Noise
  #



  return(list(model = model, valuations = valuations, predictions = predictions, residues = residues,
              graph.spatial = spatial, graph.diagonal = diagonal ))

}

