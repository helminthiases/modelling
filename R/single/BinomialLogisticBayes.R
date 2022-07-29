# Title     : BinomialLogisticBayes.R
# Objective : Binomial Logistic Bayes
# Created by: greyhypotheses
# Created on: 29/07/2022



BinomialLogisticBayes <- function (training, initial) {


  # Initial coefficient estimates
  coefficients <- attr(initial$model, which = 'beta')


  # Initial covariance [beware of positive definite constraint]
  # covariance <- summary(initial$model)$vcov


  #  Try
  data <- tibble::add_column(training,
                             e.l. = log( (training$positive + 0.5) / (training$examined - training$positive + 0.5) ), .before = 1)
  T <- linear.model.MLE(formula = e.l. ~ log(unpiped_sewer) + log(surface_sewer) + log(piped_sewer) + log(p_density) + log(elevation),
                        coords = ~I(x / 1000) + I(y / 1000),
                        data = data,
                        kappa = 0.5,
                        start.cov.pars = c(initial$settings['phi'], initial$settings['tau^2'] / initial$settings['sigma^2']),
                        method = 'nlminb',
                        messages = TRUE)
  estimates <- summary(object = T)


  # Initial (a) Gaussian process variance, (b) spatial correlation scale, and (c) nugget effect variance
  parameters <- estimates$cov.pars


  # Priors
  # base::rep(x = 0, times = length(coefficients))
  # diag(base::rep(x = 1, times = length(coefficients)))
  control.prior.settings <- control.prior(beta.mean = coefficients,
                                          beta.covar = diag(base::rep(x = 1, times = length(coefficients))),
                                          log.normal.sigma2 = as.vector(parameters['log(sigma^2)', ]),
                                          log.normal.phi = as.vector(parameters['log(phi)', ]),
                                          log.normal.nugget = as.vector(parameters['log(tau^2)', ]))


  # Control settings for the MCMC algorithm used for Bayesian inference
  # base::rep(x = 0, times = length(coefficients))
  control.mcmc.settings <- control.mcmc.Bayes(n.sim = 5000, burnin = 2000, thin = 1,
                                              epsilon.S.lim = c(0.05, 0.1), L.S.lim = c(5, 25),
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
