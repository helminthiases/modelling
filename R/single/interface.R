# Title     : interface.R
# Objective : Interface
# Created by: greyhypotheses
# Created on: 26/07/2022


# functions
source(file = 'R/single/StudyData.R')
source(file = 'R/single/DataSplitTemporal.R')


# a data set
ISO2 <- 'TG'
infection <- 'hk'
frame <- StudyData(ISO2 = ISO2, infection = infection)
splits <- list(training = 2009, testing = 2015)


# This expression counts the # of records per year; TG → 1985: 1,
# 2009: 1090, 2015: 1077
frame %>%
  dplyr::select(year) %>%
  dplyr::group_by(year) %>%
  summarise(N = n())

# Splitting
T <- DataSplitTemporal(data = frame, splits = splits)