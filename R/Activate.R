# Title     : Activate.R
# Objective : Activate
# Created by: greyhypotheses
# Created on: 23/07/2022


#' Activate Libraries
#'
Activate <- function () {

  packages <- c('data.table', 'tidyverse', 'moments', 'rmarkdown', 'latex2exp', 'mapview',
                'roxygen2', 'healthcareai', 'equatiomatic', 'rstatix', 'matrixStats', 'patchwork',
                'geoR', 'PrevMap', 'kableExtra', 'bookdown', 'lme4', 'nlme', 'DescTools',
                'sf', 'raster', 'tmap', 'terra', 'spData', 'tidygeocoder', 'rnaturalearth', 'geodata')

  # Activate
  .activate <- function (x){
    library(x, character.only = TRUE)
    if (x == 'rmarkdown') {library(tinytex)}
  }
  lapply(packages[!(packages %in% c('tidyverse', 'healthcareai', 'equatiomatic', 'data.table',
                                    'terra', 'raster'))], .activate)

  # Special Case
  if ('tidyverse' %in% packages) {
    lapply(X = c('magrittr', 'dplyr', 'tibble', 'ggplot2', 'stringr', 'lubridate'), .activate)
  }

  # Active libraries
  sessionInfo()

}