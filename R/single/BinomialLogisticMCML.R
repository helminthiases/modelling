# Title     : BinomialLogisticMCML.R
# Objective : Binomial Logistic MCML
# Created by: greyhypotheses
# Created on: 31/07/2022


BinomialLogisticMCML <- function (data, terms, variables) {

  source(file = 'R/single/InitialParameterSettings.R')

  # Initial parameters, and priors, settings
  T <- InitialParameterSettings(data = data, terms = terms, variables = variables)
  initial <- T$initial

  # The control settings for the MCMC Algorithm
  settings <- control.mcmc.MCML(n.sim = 10000, burnin = 2000, thin = 8)

  # Initial model
  cat('\nmodelling ...\n')
  parameters <- initial$settings
  for (i in seq(from = 1, to = 3)) {
    model <- binomial.logistic.MCML(formula = positive ~ log(unpiped_sewer) + log(piped_water) + log(p_density) + log(elevation),
                                    units.m = ~examinations,
                                    coords = ~I(x / 1000) + I(y / 1000),
                                    data = training,
                                    par0 = parameters,
                                    control.mcmc = settings,
                                    kappa = 0.5,
                                    start.cov.pars = c(parameters['phi'], parameters['tau^2']),
                                    method = 'nlminb')
    parameters <- coef(model)
  }

  return(list(model = model, initial = initial))

}
