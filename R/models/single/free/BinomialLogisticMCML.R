# Title     : BinomialLogisticMCML.R
# Objective : Binomial Logistic MCML
# Created by: greyhypotheses
# Created on: 02/08/2022


#' Binomial Logistic MCML
#'
#' @param data: A data set
#' @param terms: The fixed effects terms.
#' @param variables: A list that identifies the names of the fields
#'                      list(identifier = ..., tests = ..., positives = ...)
#'                   in <data>.
#'
BinomialLogisticMCML <- function (data, terms, variables) {


  source(file = 'R/models/single/free/InitialParameterSettings.R')


  # Initial parameters, and priors, settings; nugget excluded
  T <- InitialParameterSettings(data = data, terms = terms, variables = variables)
  initial <- T$initial


  # The control settings for the MCMC Algorithm
  settings <- control.mcmc.MCML(n.sim = 10000, burnin = 2000, thin = 8)


  # Model
  # Note, binomial.logistic.MCML(.) does not evaluate as.formula(.).  Hence, if a spatial.pred.binomial.MCML(.)
  # step is upcoming, use an explicitly written formula.
  parameters <- initial$settings
  for (i in seq(from = 1, to = 4)) {
    model <- binomial.logistic.MCML(formula = positive ~ piped_sewer + log(p_density) + log(elevation),
                                    units.m = ~examined,
                                    coords = ~I(x / 1000) + I(y / 1000),
                                    data = data,
                                    par0 = parameters,
                                    control.mcmc = settings,
                                    kappa = 0.5,
                                    start.cov.pars = parameters['phi'],
                                    fixed.rel.nugget = 0,
                                    method = 'nlminb')
    parameters <- coef(model)
  }
  initial$settings <- parameters

  return(list(model = model, initial = initial))

}
