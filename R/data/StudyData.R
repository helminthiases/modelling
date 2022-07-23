# Title     : StudyData.R
# Objective : Study Data
# Created by: greyhypotheses
# Created on: 23/07/2022


StudyData <- function(ISO2) {

  path <- file.path(getwd(), 'data', paste0(ISO2, '.csv'))

  select <- c('identifier', 'iso2', 'longitude', 'latitude', 'year',
              'hk_examined', 'hk_positive', 'hk_prevalence',
              'asc_examined', 'asc_positive', 'asc_prevalence',
              'tt_examined', 'tt_positive', 'tt_prevalence',
              'improved_sewer', 'unpiped_sewer', 'surface_sewer', 'piped_sewer', 'unimproved_sewer',
              'improved_water', 'unpiped_water', 'surface_water', 'piped_water', 'unimproved_water',
              'p_density', 'elevation')

  colClasses <- c('identifier' = 'integer', 'iso2' = 'character', 'year' = 'integer',
                  'hk_examined' = 'integer', 'hk_positive' = 'integer', 'asc_examined' = 'integer', 'asc_positive' = 'integer',
                  'tt_examined' = 'integer', 'tt_positive' = 'integer')

  frame <- data.table::fread(file = path, header = TRUE, select = select, colClasses = colClasses,
                             data.table = FALSE)
  str(frame)

  tidyr::pivot_longer()


}