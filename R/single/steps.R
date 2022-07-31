# Title     : steps.R
# Objective : Modelling steps
# Created by: greyhypotheses
# Created on: 31/07/2022



# Architecture
cat(paste0('positive ~ ', {{terms}}))



# Model
objects <- BinomialLogisticBayes(data = excerpt, terms = terms, variables = variables)
model <- objects$model



# Valuations (vis-à-vis training points) & Predictions (vis-à-vis testing points)
T <- EvaluationMetricsBLB(model = model, excerpt = excerpt, testing = testing)
valuations <- T$valuations
predictions <- T$predictions



# Is there still evidence of residual spatial correlation?
# The standardised residuals of the differences/errors/residuals w.r.t. the training points
residues <- StandardisedResidual(design = model$D, observed = excerpt$prevalence,
                                 estimated = valuations$prevalence$predictions)

# The empirical variogram measures & graph w.r.t. the standardised residual
points <- EmpiricalVariogram(data = data.frame(residue = residues, x = excerpt$x, y = excerpt$y))
