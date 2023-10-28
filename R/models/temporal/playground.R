# Title     : playground.R
# Objective : Playground
# Created by: greyhypotheses
# Created on: 24/08/2022



source(file = 'R/data/StudyData.R')
source(file = 'R/functions/GeographicObject.R')
source(file = 'R/functions/SpatialSplitting.R')
source(file = 'R/diagnostics/InitialEstimates.R')



# A data set
ISO2 <- 'TG'
infection <- 'hk'
frame <- StudyData(ISO2 = ISO2, infection = infection)


# Geographic form
instances <- GeographicObject(data = frame)


# Spatial splitting
T <- SpatialSplitting(instances = instances, step = 4)
training <- T$training
testing <- T$testing
rm(T)


# Preparing
variables <- list(identifier = 'identifier', tests = 'examined', positives = 'positive')
terms <- 'piped_sewer + log(p_density.k) + elevation.km + (1|year)'


# Initial coefficient & variance/scale parameter values
initial <- InitialEstimates(data = training, terms = terms, variables = variables)
parameters <- initial$settings


# The control settings for the MCMC Algorithm
settings <- control.mcmc.MCML(n.sim = 10000, burnin = 2000, thin = 8)


# Model
# Note, binomial.logistic.MCML(.) does not evaluate as.formula(.).  Hence, if a spatial.pred.binomial.MCML(.)
# step is upcoming, use an explicitly written formula.
kappa <- 0.5
for (i in seq(from = 1, to = 3)) {
  model <- binomial.logistic.MCML(formula = positive ~ piped_sewer + log(p_density.k) + elevation.km,
                                  units.m = ~examined,
                                  coords = ~I(x / 1000) + I(y / 1000),
                                  times = ~year,
                                  sst.model = 'DM',
                                  kappa.t = kappa,
                                  data = training,
                                  par0 = parameters,
                                  control.mcmc = settings,
                                  kappa = kappa,
                                  start.cov.pars = c(parameters[['phi']], parameters[['tau^2']]/parameters[['sigma^2']]),
                                  fixed.rel.nugget = 0,
                                  method = 'nlminb')
  parameters <- coef(model)
}
