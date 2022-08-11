# Title     : ConfidenceInterval.R
# Objective : Calculate a variety of confidence intervals
# Created by: greyhypotheses
# Created on: 30/05/2022



#'
#' @param parameters: a data frame that must include the fields
#'                      <est>: the estimated value
#'                      <SE>: the standard error of the estimated value
#'
CoefficientConfidenceInterval <- function (parameters) {

  # calculate the 95% interval
  parameters$interval <- qnorm(p = 0.975, lower.tail = TRUE) * parameters$SE

  # calculate the confidence intervals
  parameters[, c('2.5 %', '97.5 %')] <- parameters$est +
    matrix(data = parameters$interval) %*%  matrix(data = c(-1, 1), nrow = 1, ncol = 2)
  parameters <- parameters %>% dplyr::select(!'interval')

  return(parameters)

}




#' Variance Confidence Interval
#'
#'
#' @description The confidence intervals of measures of variance
#'
#' @param estimate: Estimated variance
#' @param level: The confidence interval level percentage, e.g., 95
#' @param N: The number of observations
#'
VarianceConfidenceInterval <- function (estimate, level, N) {

  alpha <- 1 - level/100
  L <- qchisq(p = alpha/2, df = N - 1, lower.tail = TRUE)
  U <- qchisq(p = alpha/2, df = N - 1, lower.tail = FALSE)

  Q <- (N - 1)*estimate

  lower <- Q/U
  upper <- Q/L

  return(list(lower = lower, upper = upper))

}


#'
#' @param estimates: summary( object, log.cov.pars = ... )
#' @param log.cov.pars: are the estimates ln(estimates)?
#'
SpecialConfidenceInterval <- function (estimates, log.cov.pars = TRUE) {


  # the σ*σ, ϕ, and τ*τ estimaates
  parameters <- data.frame(estimates$cov.pars)
  names(parameters) <- c('estimate', 'StdErr')


  # their confidence intervals
  parameters$interval <- qnorm(p = 0.975, lower.tail = TRUE) * parameters$StdErr
  parameters[, c('lower_ci', 'upper_ci')] <- parameters$estimate +
    matrix(data = parameters$interval) %*%  matrix(data = c(-1, 1), nrow = 1, ncol = 2)


  # if the estimates are natural logarithm estimates, calculate their normal forms
  if (log.cov.pars) {
    parameters[, c('exp(estimate)', 'exp(lower_ci)', 'exp(upper_ci)')] <-
      as.matrix(exp(parameters[, c('estimate', 'lower_ci', 'upper_ci')]))
  }


  # exclude irrelevant fields
  parameters <- parameters %>%
    dplyr::select(!c('StdErr', 'interval'))

  return(parameters)


}

