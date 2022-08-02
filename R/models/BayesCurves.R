# Title     : BayesCurves.R
# Objective : Curve
# Created by: greyhypotheses
# Created on: 01/08/2022



BayesCurves <- function (bayes) {

  # The number of coefficients
  N <- dim(bayes$model$D)[2]


  # Trace Plots: Coefficients
  for (i in seq_len(N)) {
    trace.plot(bayes$model, param = 'beta', component.beta = i)
  }

  # Trace Plots: Scale/Variance Parameters
  for (i in c('sigma2', 'phi', 'tau2')) {
    trace.plot(bayes$model, param = i)
  }

  # Density Plots: Coefficients
  for (i in seq_len(N)) {
    dens.plot(bayes$model, param = 'beta', component.beta = i)
  }

  # Density Plots: Scale/Variance Parameters
  for (i in c('sigma2', 'phi', 'tau2')) {
    dens.plot(bayes$model, param = i)
  }

}
