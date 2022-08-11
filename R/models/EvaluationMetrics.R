# Title     : EvaluationMetrics.R
# Objective : Evaluation Metrics
# Created by: greyhypotheses
# Created on: 02/08/2022



#' Evaluating the Geostatistical Binomial Logistic Model
#'
#' @param model: A PrevMap binomial.logistic.Bayes model
#' @param data: The data
#' @param type: Spatial prediction type -> joint or marginal
#'
EvaluationMetricsBLB <- function (model, data, type) {

  # valuations w.r.t. data points
  metrics <- spatial.pred.binomial.Bayes(
    object = model,
    grid.pred = as.matrix(st_drop_geometry(data[, c('x', 'y')]) / 1000),
    predictors = st_drop_geometry(data),
    type = type,
    scale.predictions = 'prevalence',
    quantiles = c(0.025, 0.975), standard.errors = TRUE)

  return(metrics)

}



#' Evaluating the Geostatistical Binomial Logistic Model
#'
#' @param model: A PrevMap binomial.logistic.MCML model
#' @param data: The data
#' @param type: Spatial prediction type -> joint or marginal
#'
EvaluationMetricsBLM <- function (model, data, type) {

  # valuations w.r.t. data points
  metrics <- spatial.pred.binomial.MCML(
    object = model,
    type = type,
    grid.pred = as.matrix(st_drop_geometry(data[, c('x', 'y')]) / 1000),
    control.mcmc = control.mcmc.MCML(n.sim = 10000, burnin = 2000, thin = 8),
    predictors = st_drop_geometry(data),
    scale.predictions = 'prevalence'
  )

  return(metrics)

}
