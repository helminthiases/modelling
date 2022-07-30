# Title     : InitialParameterSettings.R
# Objective : Initial parameter settings
# Created by: greyhypotheses
# Created on: 30/07/2022


#' Initial Parameter Settings
#'
#' @param excerpt: the reduced training data set
#' @param terms: the fixed effect terms
#'
InitialParameterSettings <- function (excerpt, terms) {

  # Functions
  source(file = 'R/diagnostics/InitialEstimates.R')


  # Initial coefficient & variance/scale parameter values
  initial <- InitialEstimates(data = excerpt, terms = terms)


  # The control settings for the MCMC Algorithm
  settings <- control.mcmc.MCML(n.sim = 1000, burnin = 100, thin = 2)


  # Much more plausible initial parameter values
  parameters <- initial$settings
  for (i in seq(from = 1, to = 2)) {
    model <- binomial.logistic.MCML(formula = as.formula(paste0('positive ~ ', terms)),
                                    units.m = ~examined,
                                    coords = ~I(x / 1000) + I(y / 1000),
                                    data = excerpt,
                                    par0 = parameters,
                                    control.mcmc = settings,
                                    kappa = 0.5,
                                    start.cov.pars = c(parameters['phi'], parameters['tau^2']),
                                    method = 'nlminb')
    parameters <- coef(model)
  }


  # Prior settings for the variance/scale parameters
  priors <- summary(model)$cov.pars


  return(list(parameters = parameters, priors = priors))

}