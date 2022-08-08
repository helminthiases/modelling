# Title     : BinomialLogisticBayes.R
# Objective : Binomial Logistic Bayes
# Created by: greyhypotheses
# Created on: 01/08/2022



#' Geostatistical Binomial Logistic Model via Bayesian Estimation
#'
#' @param data: A data set
#' @param terms: The fixed effects terms.
#' @param variables: A list that identifies the names of the fields
#'                      list(identifier = ..., tests = ..., positives = ...)
#'                   in <data>.
#'
BinomialLogisticBayes <- function (data, terms, variables) {


  source(file = 'R/models/single/stochastic/InitialParameterSettings.R')


  # Initial parameters, and priors, settings
  initial <- InitialParameterSettings(data = data, terms = terms, variables = variables)
  parameters <- initial$parameters
  priors <- initial$priors
  S <- apply(X = initial$model$samples, MARGIN = 2, FUN = mean)


  # For PrevMap::control.prior: Initial coefficient estimates
  coefficients <- parameters[!(names(parameters) %in% c('sigma^2', 'phi', 'tau^2'))]
  coefficients <- as.vector(coefficients)


  # Priors
  log.prior.sigmasqr <- function (sigmasqr){
    dlnorm(sigmasqr, meanlog = priors['log(sigma^2)', 'Estimate'], sdlog = 2*priors['log(sigma^2)', 'StdErr'], log = TRUE)
  }
  log.prior.phi <- function (phi){
    dlnorm(phi, meanlog = priors['log(phi)', 'Estimate'], sdlog = 2*priors['log(phi)', 'StdErr'], log = TRUE)
  }
  control.prior.settings <- control.prior(beta.mean = base::rep(x = 0, times = length(coefficients)),
                                          beta.covar = 100*diag(base::rep(x = 1, times = length(coefficients))),
                                          log.prior.sigma2 = log.prior.sigmasqr,
                                          log.prior.phi = log.prior.phi,
                                          log.prior.nugget = NULL)


  # Control settings for the MCMC algorithm used for Bayesian inference
  # base::rep(x = 0, times = length(coefficients))
  control.mcmc.settings <- control.mcmc.Bayes(n.sim = 8000, burnin = 2000, thin = 8,
                                              start.beta = base::rep(x = 0, times = length(coefficients)),
                                              start.sigma2 = parameters[['sigma^2']],
                                              start.phi = parameters[['phi']],
                                              start.S = S,
                                              epsilon.S.lim = c(0.01, 0.05), L.S.lim = c(4, 16), start.nugget = NULL)


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