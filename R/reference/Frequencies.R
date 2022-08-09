# Title     : Frequencies.R
# Objective : Observations per time period, identifier, etc
# Created by: greyhypotheses
# Created on: 27/07/2022


#'
#' @param data: The modelling data data frame, it must include the field <year>
#'
TimeFrequencies <- function (data) {

  # This expression counts the # of records per year
  data %>%
    dplyr::select(year) %>%
    dplyr::group_by(year) %>%
    summarise(N = n())

}


#'
#' @param data: The modelling data data frame, it must include the field <identifier>
#'
IdentifierFrequencies <- function (data) {

  # This expression counts the # of records per geographic site identifier
  data %>%
    dplyr::select(identifier) %>%
    dplyr::group_by(identifier) %>%
    summarise(N = n()) %>%
    dplyr::arrange(desc(N))

}
