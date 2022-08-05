# Title     : StudyData.R
# Objective : Study Data
# Created by: greyhypotheses
# Created on: 23/07/2022


#' Study Data
#'
#' @param ISO2: The ISO 3166-1 alpha-2 of a country
#' @param infection: The infection in focus → ascariasis, hookworm, or trichuriasis
#'
StudyData <- function(ISO2, infection, add.extraneous = FALSE) {


  # The infection of interest
  abbreviations <- list('asc' = c('asc_examined', 'asc_positive', 'asc_prevalence'),
                        'hk' = c('hk_examined', 'hk_positive', 'hk_prevalence'),
                        'tt' = c('tt_examined', 'tt_positive', 'tt_prevalence'))
  measures <- abbreviations[infection]


  # The fields of interest
  core <- c('identifier', 'iso2', 'longitude', 'latitude', 'year',
            'improved_sewer', 'unpiped_sewer', 'surface_sewer', 'piped_sewer', 'unimproved_sewer',
            'improved_water', 'unpiped_water', 'surface_water', 'piped_water', 'unimproved_water',
            'p_density', 'elevation')


  # Extra
  extra <- c('AnnualPrecip', 'AnPET','AridityIndex')


  # Setting selections & types
  if (add.extraneous) {
    select <- c(core, extra, unlist(measures, use.names = FALSE))
    core <- c(core, extra)
  } else {
    select <- c(core, unlist(measures, use.names = FALSE))
  }
  colClasses <- c('identifier' = 'integer', 'iso2' = 'character', 'year' = 'integer')


  # Reading-in the data
  path <- file.path(getwd(), 'data', paste0(ISO2, '.csv'))
  frame <- data.table::fread(file = path, header = TRUE, select = select, colClasses = colClasses,
                             data.table = FALSE)


  # removing the infection based prefixes from the names of the measurements fields
  frame <- dplyr::rename_with(frame, ~ gsub(pattern = paste0(infection, '_'), replacement = '', .x),
                              starts_with(paste0(infection, '_')))
  dim(frame)


  # removing instances that have NaN core values
  states <- frame %>%
    dplyr::select(tidyselect::all_of(core)) %>%
    complete.cases()
  frame <- frame[states, ]


  # access percentages as fractional measures
  access <- c('improved_sewer', 'unpiped_sewer', 'surface_sewer', 'piped_sewer', 'unimproved_sewer',
              'improved_water', 'unpiped_water', 'surface_water', 'piped_water', 'unimproved_water')
  frame[, access] <- frame[, access] / 100


  # if prevalence exists, then examinations > 0
  frame <- frame[!is.nan(frame$prevalence), ]
  dim(frame)


  # Extra
  frame$elevation.km <- frame$elevation / 1000


  # Hence
  return(frame)

}