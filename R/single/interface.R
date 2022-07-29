# Title     : interface.R
# Objective : Interface
# Created by: greyhypotheses
# Created on: 26/07/2022


# functions
source(file = 'R/single/StudyData.R')
source(file = 'R/single/GeographicObject.R')
source(file = 'R/single/DataSplitTemporal.R')
source(file = 'R/single/InitialDiagnostics.R')


# a data set
ISO2 <- 'TG'
infection <- 'hk'
frame <- StudyData(ISO2 = ISO2, infection = infection)


# geographic form
instances <- GeographicObject(data = frame)


# Splitting
splits <- list(training = 2009, testing = 2015)
T <- DataSplitTemporal(data = instances, splits = splits)
training <- T$training
testing <- T$testing


# Diagnostics
terms <- 'improved_sewer + unpiped_sewer + surface_sewer + piped_sewer + unimproved_sewer + log(p_density) + log(elevation)'
InitialDiagnostics(data = training, terms = terms)


