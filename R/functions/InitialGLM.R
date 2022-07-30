# Title     : InitialGLM.R
# Objective : Investigate probable explanatory variables via generalised
#             mixed models
# Created by: greyhypotheses
# Created on: 29/07/2022

#' Prior
#'
#' @description Investigate probable explanatory variables via generalised mixed models
#'
#' @param data:
#' @param expr:
#' @param variables: A list that identifies the names of the fields
#'                      list(identifier = ..., tests = ..., positives = ...)
#'                   in <data>.
#' @param limit:
#'
InitialGLM <- function (data, expr, variables, limit) {

  source(file = 'R/diagnostics/InitialEstimates.R')

  for (i in seq_len(length.out = limit)) {

    terms <- paste0(expr[i:length(expr)], collapse = ' + ')
    estimates <- InitialEstimates(data = data, terms = terms, variables = variables)
    E <- summary(estimates$model)

    cat('\n\n')
    print(E)
    cat('\n\n')

  }

}