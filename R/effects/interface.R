# Title     : interface.R
# Objective : Interface
# Created by: greyhypotheses
# Created on: 30/07/2022


# functions
source(file = 'R/data/StudyData.R')
source(file = 'R/functions/GeographicObject.R')
source(file = 'R/functions/DataSplitTemporal.R')
source(file = 'R/functions/SpatialExcerpt.R')
source(file = 'R/functions/InitialGLM.R')


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
rm(T)


# Reducing
excerpt <- SpatialExcerpt(data = training, step = 4)


# Key Variables
variables <- list(identifier = 'identifier', tests = 'examined', positives = 'positive')


# Plausible fixed effects
expr <- c('log(improved_sewer)', 'log(unimproved_sewer)', 'log(piped_sewer)', 'log(unpiped_sewer)',  'log(surface_sewer)',
          'log(p_density)', 'log(elevation)')
InitialGLM(data = data, expr = expr, variables = variables, limit = 5)


expr <- c('log(improved_sewer)', 'log(unimproved_sewer)', 'log(surface_sewer)', 'log(piped_sewer)', 'log(unpiped_sewer)',
          'log(p_density)', 'log(elevation)')
InitialGLM(data = data, expr = expr, variables = variables, limit = 5)
