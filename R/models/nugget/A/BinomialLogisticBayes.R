# Title     : BinomialLogisticBayes.R
# Objective : Binomial Logistic Bayes
# Created by: greyhypotheses
# Created on: 29/07/2022



#' Geostatistical Binomial Logistic Model via Bayesian Estimation
#'
#' @param data: A data set
#' @param terms: The fixed effects terms.
#' @param variables: A list that identifies the names of the fields
#'                      list(identifier = ..., tests = ..., positives = ...)
#'                   in <data>.
#' @param kappa: The smoothness parameter of the Matérn function
#'
BinomialLogisticBayes <- function (data, terms, variables, kappa = 0.5) {


  source(file = 'InitialParameterSettings.R')


  # Initial settings
  initial <- InitialParameterSettings(data = data, terms = terms, variables = variables)
  parameters <- initial$parameters
  priors <- initial$priors
  S <- apply(X = initial$model$samples, MARGIN = 2, FUN = mean)


  # For PrevMap::control.prior: Initial coefficient estimates
  coefficients <- parameters[!(names(parameters) %in% c('sigma^2', 'phi', 'tau^2'))]
  coefficients <- as.vector(coefficients)


  # Priors
  control.prior.settings <- control.prior(beta.mean = coefficients,
                                          beta.covar = diag(base::rep(x = 1, times = length(coefficients))),
                                          log.normal.sigma2 = as.vector(priors['log(sigma^2)', ]),
                                          log.normal.phi = as.vector(priors['log(phi)', ]),
                                          log.normal.nugget = as.vector(priors['log(tau^2)', ]))


  # Control settings for the MCMC algorithm used for Bayesian inference
  # base::rep(x = 0, times = length(coefficients))
  control.mcmc.settings <- control.mcmc.Bayes(n.sim = 5000, burnin = 2000, thin = 8,
                                              epsilon.S.lim = c(0.01, 0.05), L.S.lim = c(4, 16),
                                              start.beta = coefficients,
                                              start.sigma2 = parameters['sigma^2'],
                                              start.phi = parameters['phi'],
                                              start.nugget = parameters['tau^2'],
                                              start.S = S)


  # Modelling
  # Note, binomial.logistic.Bayes(.) does not evaluate as.formula(.).  Hence, if a spatial.pred.binomial.Bayes(.)
  # step is upcoming, use an explicitly written formula.
  model <- binomial.logistic.Bayes(
    formula = positive ~ piped_sewer + I(piped_sewer^2) + elevation.km,
    units.m = ~examined,
    coords = ~I(x / 1000) + I(y / 1000),
    data = data,
    control.prior = control.prior.settings,
    control.mcmc = control.mcmc.settings,
    kappa = 0.5)

  return(list(model = model, initial = initial))

}
