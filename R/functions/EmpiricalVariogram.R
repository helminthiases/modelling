# Title     : EmpiricalVariogram.R
# Objective : Empirical Variogram
# Created by: greyhypotheses
# Created on: 31/07/2022



#' Empirical Variogram Graph
#'
#' @note plot(an object of class variogram, type = 'b', frame.plot = FALSE) is a
#'       variogram drawing option
#'
#' @param data: A data frame consisting of fields residue, x, and y; per row, each x & y pair
#'              is the coordinate point of the residue value
#'
EmpiricalVariogram <- function (data) {

  # variogram points
  #
  measures <- variogram(data = data, var.name = ~residue, coords = ~I(x / 1000) + I(y / 1000))


  # variogram envelop
  limits <- variog.mc.env(coords = as.matrix(data[, c('x', 'y')]/1000),
                          data = data$residue, obj.variog = measures, nsim = 5000)
  points <- data.frame(distance = limits$u, estimate = measures$v,
                       estimate.lower = limits$v.lower, estimate.upper = limits$v.upper)

  # hence
  return(points)

}