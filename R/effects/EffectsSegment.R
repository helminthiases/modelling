# Title     : EffectsSegment.R
# Objective : Segment focused effects
# Created by: greyhypotheses
# Created on: 10/08/2022



#' Effects Segment
#'
#' @param frame: The data of a year
#' @param X: A list of formulae expressions
#'
EffectsSegment <- function (frame, X) {


  try(if (length(unique(frame$year)) > 1) stop('The observations must be asociated with a single year.'))


  source(file = 'R/effects/Estimates.R')
  source(file = 'R/effects/InitialDiagnostics.R')


  # Generalised linear mixed models
  .glmm <- function (terms) {
    string <- paste(terms, ' + (1|identifier)', collapse = NULL)
    model <- Estimates(data = frame, expression = string)
    return(model)
  }


  # Spatial correlation diagnostics
  .lse <- function (terms) {
    lse <- InitialDiagnostics(data = frame, terms = terms)
    return(lse)
  }


  # Calculations
  LSE <- lapply(X = X, FUN = .lse)
  models <- lapply(X = X, FUN = .glmm)


  return(list(LSE = LSE, models = models))


}