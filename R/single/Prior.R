# Title     : Prior.R
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
#' @param limit:
#'
Prior <- function (data, expr, limit) {

  source(file = 'R/diagnostics/InitialEstimates.R')

  for (i in seq_len(length.out = limit)) {

    terms <- paste0(expr[i:length(expr)], collapse = ' + ')
    estimates <- InitialEstimates(data = data, terms = terms)
    E <- summary(estimates$model)

    cat('\n\n')
    print(E)
    cat('\n\n')

  }

}