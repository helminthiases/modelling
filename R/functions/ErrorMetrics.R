# Title     : ErrorMetrics.R
# Objective : Error Metrics
# Created by: greyhypotheses
# Created on: 10/08/2022


#' Error Metrics
#'
#' @description Calculates bias & root mean square error
#'
#' @param observed
#' @param estimated
#' @param name
#'
ErrorMetrics <- function (observed, estimated, name) {

  # differences/errors
  differences <- estimated - observed

  metrics <- data.frame(Bias = round(mean(differences), digits = 3),
                        RMSE = round(sqrt( sum(differences^2) / length(differences) ), digits = 3),
                        row.names = name)

  return(metrics)

}