# Title     : StudyData.R
# Objective : Study Data
# Created by: greyhypotheses
# Created on: 23/07/2022


StudyData <- function(ISO2) {

  selection <- 'hookworm'

  abbreviations <- list('ascariasis' = c('asc_examined', 'asc_positive', 'asc_prevalence'),
                        'hookworm' = c('hk_examined', 'hk_positive', 'hk_prevalence'),
                        'trichuriasis' = c('tt_examined', 'tt_positive', 'tt_prevalence'))

  infection <- abbreviations[selection]

  path <- file.path(getwd(), 'data', paste0(ISO2, '.csv'))

  select <- c('identifier', 'iso2', 'longitude', 'latitude', 'year',
              'improved_sewer', 'unpiped_sewer', 'surface_sewer', 'piped_sewer', 'unimproved_sewer',
              'improved_water', 'unpiped_water', 'surface_water', 'piped_water', 'unimproved_water',
              'p_density', 'elevation', unlist(infection, use.names = FALSE))

  colClasses <- c('identifier' = 'integer', 'iso2' = 'character', 'year' = 'integer')

  frame <- data.table::fread(file = path, header = TRUE, select = select, colClasses = colClasses,
                             data.table = FALSE)
  str(frame)




}