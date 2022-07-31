# Title     : EvaluationMetricsBLM.R
# Objective : Evaluation Metrics Binomial Logistics MCML
# Created by: greyhypotheses
# Created on: 31/07/2022



#' Evaluating the Geostatistical Binomial Logistic Model
#'
#' @param model: A PrevMap binomial.logistic.MCML model
#' @param excerpt: The training data
#' @param testing: The testing data
#'
EvaluationMetricsBLM <- function (model, excerpt, testing) {

  # valuations w.r.t. excerpt points
  valuations <- spatial.pred.binomial.MCML(
    object = model,
    grid.pred = as.matrix(st_drop_geometry(excerpt[, c('x', 'y')]) / 1000),
    control.mcmc = control.mcmc.MCML(n.sim = 10000, burnin = 2000, thin = 8),
    predictors = st_drop_geometry(excerpt),
    scale.predictions = 'prevalence'
  )

  # predictions
  predictions <- spatial.pred.binomial.MCML(
    object = model,
    grid.pred = as.matrix(st_drop_geometry(testing[, c('x', 'y')]) / 1000),
    control.mcmc = control.mcmc.MCML(n.sim = 10000, burnin = 2000, thin = 8),
    predictors = st_drop_geometry(testing),
    scale.predictions = 'prevalence'
  )

  return(list(valuations = valuations, predictions = predictions))

}
