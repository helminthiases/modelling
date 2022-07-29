# Title     : DataSplitTemporal.R
# Objective : Data splitting function
# Created by: greyhypotheses
# Created on: 26/07/2022


#'
#' @param data: The modelling data set
#' @param splits: A list of the form list(training = ..., testing = ...), wherein
#'                each ellipsis denotes a year, or list of years.
#'
DataSplitTemporal <- function (data, splits) {

  # the splits
  training <- data[data$year %in% splits$training, ]
  testing <- data[data$year %in% splits$testing, ]

  return(list(training = training, testing = testing))

}