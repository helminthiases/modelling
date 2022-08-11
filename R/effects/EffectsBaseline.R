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
  source(file = 'R/effects/InitialDiagnostics.R')


  # Generalised linear mixed models
  .glmm <- function (terms) {
    string <- paste(terms, ' + (1|identifier) + (1|year)', collapse = NULL)
    model <- Estimates(data = frame, expression = string)
    return(model)
  }


  # Spatial correlatio diagnostics
  .lse <- function (terms) {
    lse <- InitialDiagnostics(data = frame, terms = terms)
    return(lse)
  }


  # Calculations
  LSE <- lapply(X = X, FUN = .lse)
  models <- lapply(X = X, FUN = .glmm)


  return(list(LSE = LSE, models = models))

}