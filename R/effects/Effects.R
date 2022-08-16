# Title     : Effects.R
# Objective : Effects
# Created by: greyhypotheses
# Created on: 10/08/2022



#' Effects Baseline
#'
#' @param frame: A data frame
#' @param expressions: A list of formulae expressions
#' @param variables: A list that identifies the names of the fields
#'                      list(identifier = ..., tests = ..., positives = ...)
#'                   in <data>.
#'
Effects <- function (frame, expressions, variables) {


  source(file = 'R/effects/Estimates.R')


  # Generalised linear mixed models
  .glmm <- function (terms) {
    string <- paste(terms, ' + (1|identifier)', collapse = NULL)
    model <- Estimates(data = frame, expression = string, variables = variables)
    return(model)
  }


  # Calculations
  models <- lapply(X = expressions, FUN = .glmm)


  return(models)

}