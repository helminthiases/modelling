# Title     : interface.R
# Objective : Interface
# Created by: greyhypotheses
# Created on: 26/07/2022


# functions
source(file = 'R/single/StudyData.R')


# a data set
ISO2 <- 'TG'
infection <- 'hookworm'
frame <- StudyData(ISO2 = ISO2, infection = infection)


# This expression counts the # of records per year; TG → 1985: 1,
# 2009: 1090, 2015: 1077
frame %>%
  dplyr::select(year) %>%
  dplyr::group_by(year) %>%
  summarise(N = n())