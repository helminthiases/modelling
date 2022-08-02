# Title     : EvaluationMetrics.R
# Objective : Evaluation Metrics
# Created by: greyhypotheses
# Created on: 02/08/2022



#' Evaluating the Geostatistical Binomial Logistic Model
#'
#' @param model: A PrevMap binomial.logistic.Bayes model
#' @param training: The training data
#' @param testing: The testing data
#'
EvaluationMetricsBLB <- function (model, training, testing) {

  # valuations w.r.t. training points
  valuations <- spatial.pred.binomial.Bayes(
    object = model,
    grid.pred = as.matrix(st_drop_geometry(training[, c('x', 'y')]) / 1000),
    predictors = st_drop_geometry(training),
    type = 'marginal',
    scale.predictions = 'prevalence',
    quantiles = c(0.025, 0.975), standard.errors = TRUE)

  # predictions
  predictions <- spatial.pred.binomial.Bayes(
    object = model,
    grid.pred = as.matrix(st_drop_geometry(testing[, c('x', 'y')]) / 1000),
    predictors = st_drop_geometry(testing),
    type = 'marginal',
    scale.predictions = 'prevalence',
    quantiles = c(0.025, 0.975), standard.errors = TRUE)

  return(list(valuations = valuations, predictions = predictions))

}



#' Evaluating the Geostatistical Binomial Logistic Model
#'
#' @param model: A PrevMap binomial.logistic.MCML model
#' @param training: The training data
#' @param testing: The testing data
#'
EvaluationMetricsBLM <- function (model, training, testing) {

  # valuations w.r.t. training points
  valuations <- spatial.pred.binomial.MCML(
    object = model,
    type = 'joint',
    grid.pred = as.matrix(st_drop_geometry(training[, c('x', 'y')]) / 1000),
    control.mcmc = control.mcmc.MCML(n.sim = 10000, burnin = 2000, thin = 8),
    predictors = st_drop_geometry(training),
    scale.predictions = 'prevalence'
  )

  # predictions
  predictions <- spatial.pred.binomial.MCML(
    object = model,
    type = 'joint',
    grid.pred = as.matrix(st_drop_geometry(testing[, c('x', 'y')]) / 1000),
    control.mcmc = control.mcmc.MCML(n.sim = 10000, burnin = 2000, thin = 8),
    predictors = st_drop_geometry(testing),
    scale.predictions = 'prevalence'
  )

  return(list(valuations = valuations, predictions = predictions))

}
