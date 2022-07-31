# Title     : EvaluationMetricsBLB.R
# Objective : Evaluate Binomial Logistic Bayes
# Created by: greyhypotheses
# Created on: 30/07/2022


#' Evaluating the Geostatistical Binomial Logistic Model
#'
#' @param model: A PrevMap binomial.logistic.Bayes model
#' @param excerpt: The training data
#' @param testing: The testing data
#'
BinomialLogisticBayesEVL <- function (model, excerpt, testing) {

  # valuations w.r.t. excerpt points
  valuations <- spatial.pred.binomial.Bayes(
    object = model,
    grid.pred = as.matrix(st_drop_geometry(excerpt[, c('x', 'y')]) / 1000),
    predictors = st_drop_geometry(excerpt), type = 'marginal',
    scale.predictions = 'prevalence',
    quantiles = c(0.025, 0.975), standard.errors = TRUE)

  # predictions
  predictions <- spatial.pred.binomial.Bayes(
    object = model, grid.pred = as.matrix(st_drop_geometry(testing[, c('x', 'y')]) / 1000),
    predictors = st_drop_geometry(testing), type = 'marginal', scale.predictions = 'prevalence',
    quantiles = c(0.025, 0.975), standard.errors = TRUE)

  return(list(valuations = valuations, predictions = predictions))

}
