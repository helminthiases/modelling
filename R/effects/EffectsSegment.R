# Title     : EffectsSegment.R
# Objective : Segment focused effects
# Created by: greyhypotheses
# Created on: 10/08/2022



#' Effects Segment
#'
#' @param frame: The data of a year
#' @param expressions: A list of formulae expressions
#'
EffectsSegment <- function (frame, expressions) {


  try(if (length(unique(frame$year)) > 1) stop('The observations must be asociated with a single year.'))


  source(file = 'R/effects/Estimates.R')


  # Generalised linear mixed models
  .glmm <- function (terms) {
    string <- paste(terms, ' + (1|identifier)', collapse = NULL)
    model <- Estimates(data = frame, expression = string)
    return(model)
  }


  # Calculations
  models <- lapply(X = expressions, FUN = .glmm)


  return(models)


}