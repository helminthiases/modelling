# Title     : Frequencies.R
# Objective : Observations per time period, identifier, etc
# Created by: greyhypotheses
# Created on: 27/07/2022


#'
#' @param data: The modelling data data frame, it must include the field <year>
#'
TimeFrequencies <- function (data, pathstr) {

  # This expression counts the # of records per year
  tally <- data %>%
    dplyr::select(year) %>%
    dplyr::group_by(year) %>%
    summarise(N = n())

  # Write
  utils::write.table(x = tally,
                     file = file.path(pathstr, 'frequencies', 'timeFrequencies.csv'),
                     append = FALSE,
                     sep = ',',
                     na = '',
                     row.names = FALSE,
                     col.names = TRUE,
                     fileEncoding = 'UTF-8')

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
