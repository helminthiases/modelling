# Title     : Time.R
# Objective : Observations per time period
# Created by: greyhypotheses
# Created on: 27/07/2022


#'
#' @param data: The modelling data data frame, it must include the field <year>
#'
Time <- function (data) {

  # This expression counts the # of records per year
  data %>%
    dplyr::select(year) %>%
    dplyr::group_by(year) %>%
    summarise(N = n())

}