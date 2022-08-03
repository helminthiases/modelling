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
#'
InitialParameterSettings <- function (data, terms, variables) {


  # Functions
  source(file = 'R/diagnostics/InitialEstimates.R')


  # Initial coefficient & variance/scale parameter values
  initial <- InitialEstimates(data = data, terms = terms, variables = variables)


  # The control settings for the MCMC Algorithm
  settings <- control.mcmc.MCML(n.sim = 10000, burnin = 2000, thin = 8)


  # Much more plausible initial parameter values
  parameters <- initial$settings[!(names(initial$settings) %in% 'tau^2')]
  for (i in seq(from = 1, to = 2)) {
    model <- binomial.logistic.MCML(formula = as.formula(paste0('positive ~ ', terms)),
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


  # Settings for variance/scale parameters priors.  Natural logarithm values.
  priors <- summary(model)$cov.pars


  return(list(parameters = parameters, priors = priors, model = model))

}