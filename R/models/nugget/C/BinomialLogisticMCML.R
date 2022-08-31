# Title     : BinomialLogisticMCML.R
# Objective : Binomial Logistic MCML
# Created by: greyhypotheses
# Created on: 10/08/2022



#' Binomial Logistic MCML
#'
#' @param data: A data set
#' @param terms: The fixed effects terms.
#' @param variables: A list that identifies the names of the fields
#'                      list(identifier = ..., tests = ..., positives = ...)
#'                   in <data>.
#'
BinomialLogisticMCML <- function (data, terms, variables) {


  source(file = '../../single/nugget/InitialParameterSettings.R')


  # Initial parameters, and priors, settings; nugget excluded
  initial <- InitialParameterSettings(data = data, terms = terms, variables = variables)
  parameters <- initial$parameters


  # The control settings for the MCMC Algorithm
  settings <- control.mcmc.MCML(n.sim = 10000, burnin = 2000, thin = 8)


  # Model
  # Note, binomial.logistic.MCML(.) does not evaluate as.formula(.).  Hence, if a spatial.pred.binomial.MCML(.)
  # step is upcoming, use an explicitly written formula.
  for (i in seq(from = 1, to = 3)) {
    model <- binomial.logistic.MCML(formula = positive ~ piped_sewer + I(piped_sewer^2) + elevation.km,
                                    units.m = ~examined,
                                    coords = ~I(x / 1000) + I(y / 1000),
                                    data = data,
                                    times = ~year,
                                    par0 = parameters,
                                    control.mcmc = settings,
                                    kappa = 0.5,
                                    start.cov.pars = c(parameters['phi'], parameters['tau^2']/parameters['sigma^2']),
                                    fixed.rel.nugget = NULL,
                                    method = 'nlminb')
    parameters <- coef(model)
  }


  return(list(model = model, initial = initial))

}