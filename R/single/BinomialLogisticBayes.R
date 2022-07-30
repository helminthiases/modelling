# Title     : BinomialLogisticBayes.R
# Objective : Binomial Logistic Bayes
# Created by: greyhypotheses
# Created on: 29/07/2022



BinomialLogisticBayes <- function (training, initial) {


  # For PrevMap::control.prior: Initial coefficient estimates
  coefficients <- attr(initial$model, which = 'beta')


  # For PrevMap::control.prior: Initial covariance [beware of positive definite constraint]
  # covariance <- summary(initial$model)$vcov


  # For PrevMap::control.prior: The distribution settings of the priors
  # of the variance/scale parameters
  natural <- log(initial$settings[c('sigma^2', 'phi', 'tau^2')])
  deviation <- 1
  parameters <- list(ln.sigmasqr = c(natural[['sigma^2']], deviation),
                     ln.phi = c(natural[['phi']], deviation),
                     ln.tausqr = c(natural[['tau^2']], deviation))


  # Priors
  # beta.mean = base::rep(x = 0, times = length(coefficients))
  # beta.covar = covariance
  control.prior.settings <- control.prior(beta.mean = coefficients,
                                          beta.covar = diag(base::rep(x = 1, times = length(coefficients))),
                                          log.normal.sigma2 = parameters[['ln.sigmasqr']],
                                          log.normal.phi = parameters[['ln.phi']],
                                          log.normal.nugget = parameters[['ln.tausqr']])


  # Control settings for the MCMC algorithm used for Bayesian inference
  # base::rep(x = 0, times = length(coefficients))
  control.mcmc.settings <- control.mcmc.Bayes(n.sim = 5000, burnin = 2000, thin = 1,
                                              epsilon.S.lim = c(0.01, 0.05), L.S.lim = 2,
                                              start.beta = coefficients,
                                              start.sigma2 = initial$settings['sigma^2'],
                                              start.phi = initial$settings['phi'],
                                              start.nugget = initial$settings['tau^2'],
                                              start.S = as.numeric(predict(initial$model, type = 'response')))


  # Modelling
  model <- binomial.logistic.Bayes(
    formula = positive ~ log(unpiped_sewer) + log(surface_sewer) + log(piped_sewer) + log(p_density) + log(elevation),
    units.m = ~examined,
    coords = ~I(x / 1000) + I(y / 1000),
    data = training,
    control.prior = control.prior.settings,
    control.mcmc = control.mcmc.settings,
    kappa = 0.5)

}
