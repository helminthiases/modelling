# Title     : interface.R
# Objective : Interface
# Created by: greyhypotheses
# Created on: 26/07/2022


# functions
source(file = 'R/single/StudyData.R')
source(file = 'R/single/DataSplitTemporal.R')
source(file = 'R/single/InitialDiagnostics.R')


# a data set
ISO2 <- 'TG'
infection <- 'hk'
frame <- StudyData(ISO2 = ISO2, infection = infection)
splits <- list(training = 2009, testing = 2015)


# Splitting
T <- DataSplitTemporal(data = frame, splits = splits)
