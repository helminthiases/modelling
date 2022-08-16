# Title     : EvaluationVariogram.R
# Objective : Evaluation Variogram
# Created by: greyhypotheses
# Created on: 10/08/2022


#' Evaluation Variogram
#'
#' @param model:
#' @param data: ... a data frame that must include the model's training data <prevalence>, the
#'              model's consequent <estimate> of each prevalence value, and the coordinates whence
#'              the prevalence values originate
#'
EvaluationVariogram <- function (model, data) {

  source(file = 'R/functions/StandardisedResidual.R')
  source(file = 'R/functions/EmpiricalVariogram.R')
  source(file = 'R/models/EvaluationGraphs.R')

  # Is there still evidence of residual spatial correlation?
  # The standardised residuals of the differences/errors/residuals w.r.t. the training points
  # Subsequently, the empirical variogram measures & graph w.r.t. the standardised residual
  residues <- StandardisedResidual(design = model$D, observed = data$prevalence,
                                   estimated = data$estimate)
  points <- EmpiricalVariogram(data = data.frame(residue = residues, x = data$x, y = data$y))
  graph <- SpatialEvaluationGraphs(points = points, limit = 300)

  return(list(residues = residues, points = points, graph = graph))

}