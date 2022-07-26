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
#' @param kappa: The smoothness parameter of the Matérn function
#'
BinomialLogisticMCML <- function (data, terms, variables, kappa = 0.5) {


  source(file = 'R/models/nugget/InitialParameterSettings.R')


  # Initial parameters, and priors, settings; nugget excluded
  initial <- InitialParameterSettings(data = data, terms = terms, variables = variables)
  parameters <- initial$parameters


  # The control settings for the MCMC Algorithm
  settings <- control.mcmc.MCML(n.sim = 10000, burnin = 2000, thin = 8)


  # Model
  # Note, binomial.logistic.MCML(.) does not evaluate as.formula(.).  Hence, if a spatial.pred.binomial.MCML(.)
  # step is upcoming, use an explicitly written formula.
  for (i in seq(from = 1, to = 4)) {
    model <- binomial.logistic.MCML(formula = positive ~ piped_sewer + log(p_density.k) + elevation.km,
                                    units.m = ~examined,
                                    coords = ~I(x / 1000) + I(y / 1000),
                                    data = data,
                                    par0 = parameters,
                                    control.mcmc = settings,
                                    kappa = kappa,
                                    start.cov.pars = c(parameters['phi'], parameters['tau^2']/parameters['sigma^2']),
                                    fixed.rel.nugget = NULL,
                                    method = 'nlminb')
    parameters <- coef(model)
  }


  return(list(model = model, initial = initial))

}
