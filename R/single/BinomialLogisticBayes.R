# Title     : BinomialLogisticBayes.R
# Objective : Binomial Logistic Bayes
# Created by: greyhypotheses
# Created on: 29/07/2022



#' Geostatistical Binomial Logistic Model via Bayesian Estimation
#'
#' @param data: A data set
#' @param terms: The fixed effect terms
#' @param variables: A list that identifies the names of the fields
#'                      list(identifier = ..., tests = ..., positives = ...)
#'                   in <data>.
#'
BinomialLogisticBayes <- function (data, terms, variables) {


  source(file = 'R/single/InitialParameterSettings.R')


  # Initial parameters, and priors, settings
  T <- InitialParameterSettings(data = data, terms = terms, variables = variables)
  initial <- T$initial
  priors <- T$priors


  # For PrevMap::control.prior: Initial coefficient estimates
  coefficients <- attr(initial$model, which = 'beta')


  # For PrevMap::control.prior: Initial covariance [beware of positive definite constraint]
  # covariance <- summary(initial$model)$vcov


  # Priors
  # beta.mean = base::rep(x = 0, times = length(coefficients))
  # beta.covar = covariance
  control.prior.settings <- control.prior(beta.mean = coefficients,
                                          beta.covar = diag(base::rep(x = 1, times = length(coefficients))),
                                          log.normal.sigma2 = as.vector(priors['log(sigma^2)', ]),
                                          log.normal.phi = as.vector(priors['log(phi)', ]),
                                          log.normal.nugget = as.vector(priors['log(tau^2)', ]))


  # Control settings for the MCMC algorithm used for Bayesian inference
  # base::rep(x = 0, times = length(coefficients))
  control.mcmc.settings <- control.mcmc.Bayes(n.sim = 5000, burnin = 2000, thin = 4,
                                              epsilon.S.lim = c(0.01, 0.05), L.S.lim = 2,
                                              start.beta = coefficients,
                                              start.sigma2 = initial$settings['sigma^2'],
                                              start.phi = initial$settings['phi'],
                                              start.nugget = initial$settings['tau^2'],
                                              start.S = as.numeric(predict(initial$model, type = 'response')))


  # Modelling
  model <- binomial.logistic.Bayes(
    formula = as.formula(paste0('positive ~ ', terms)),
    units.m = ~examined,
    coords = ~I(x / 1000) + I(y / 1000),
    data = data,
    control.prior = control.prior.settings,
    control.mcmc = control.mcmc.settings,
    kappa = 0.5)

  return(list(model = model, initial = initial))

}
