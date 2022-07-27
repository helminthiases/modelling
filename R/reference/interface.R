# Title     : interface.R
# Objective : interface
# Created by: greyhypotheses
# Created on: 27/07/2022


# functions
source(file = 'R/single/StudyData.R')
source(file = 'R/reference/Time.R')
source(file = 'R/reference/Graphs.R')
source(file = 'R/reference/Distributions.R')


# a data set
ISO2 <- 'TG'
infection <- 'hk'
frame <- StudyData(ISO2 = ISO2, infection = infection)


# the number of observations per year
Time(data = frame)


# graphs
# w.r.t. TG, the years of interest are 2009 & 2015
excerpt <- frame %>%
  dplyr::filter(year %in% c(2009, 2015))

Distributions(excerpt = excerpt)
ElevationGraphs(excerpt = excerpt)
DensityGraphs(excerpt = excerpt)
SewerGraphs(excerpt = excerpt)







