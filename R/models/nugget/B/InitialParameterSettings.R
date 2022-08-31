# Title     : InitialParameterSettings.R
# Objective : Initial Parameter Settings
# Created by: greyhypotheses
# Created on: 02/08/2022



#' Initial Parameter Settings
#'
#' @param data: A data set
#' @param terms: The fixed effect terms
#' @param variables: A list that identifies the names of the fields
#'                      list(identifier = ..., tests = ..., positives = ...)
#'                   in <data>.
#' @param kappa: The smoothness parameter of the Matérn function
#'
InitialParameterSettings <- function (data, terms, variables, kappa = 0.5) {


  # Functions
  source(file = 'R/diagnostics/InitialEstimates.R')


  # Initial coefficient & variance/scale parameter values
  initial <- InitialEstimates(data = data, terms = terms, variables = variables)


  # The control settings for the MCMC Algorithm
  settings <- control.mcmc.MCML(n.sim = 10000, burnin = 2000, thin = 8)


  # Much more plausible initial parameter values
  parameters <- initial$settings
  for (i in seq(from = 1, to = 2)) {
    model <- binomial.logistic.MCML(formula = as.formula(paste0('positive ~ ', terms)),
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


  # Settings for variance/scale parameters priors.  Natural logarithm values.
  priors <- summary(model)$cov.pars


  return(list(parameters = parameters, priors = priors, model = model))

}
