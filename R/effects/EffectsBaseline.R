# Title     : EffectsBaseline.R
# Objective : Effects
# Created by: greyhypotheses
# Created on: 10/08/2022



#' Effects Baseline
#'
#' @param frame: The data of a year
#' @param X: A list of formulae expressions
#'
EffectsBaseline <- function (frame, X) {


  source(file = 'R/effects/Estimates.R')


  # Generalised linear mixed models
  .glmm <- function (terms) {
    string <- paste(terms, ' + (1|identifier) + (1|year)', collapse = NULL)
    model <- Estimates(data = frame, expression = string)
    return(model)
  }


  # Calculations
  models <- lapply(X = X, FUN = .glmm)


  return(models)

}