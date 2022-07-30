# Title     : interface.R
# Objective : Interface
# Created by: greyhypotheses
# Created on: 26/07/2022


# functions
source(file = 'R/data/StudyData.R')
source(file = 'R/functions/GeographicObject.R')
source(file = 'R/functions/DataSplitTemporal.R')
source(file = 'R/functions/SpatialExcerpt.R')
source(file = 'R/diagnostics/InitialEstimates.R')
source(file = 'R/functions/InitialGLM.R')
source(file = 'R/single/BinomialLogisticBayes.R')


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


# Plausible fixed effects terms
expr <- c('log(improved_sewer)', 'log(unimproved_sewer)', 'log(piped_sewer)', 'log(unpiped_sewer)',  'log(surface_sewer)',
          'log(p_density)', 'log(elevation)')
InitialGLM(data = excerpt, expr = expr, limit = 5)
rm(expr)

expr <- c('log(improved_sewer)', 'log(unimproved_sewer)', 'log(surface_sewer)', 'log(piped_sewer)', 'log(unpiped_sewer)',
          'log(p_density)', 'log(elevation)')
InitialGLM(data = excerpt, expr = expr, limit = 5)
rm(expr)


# Diagnostics
terms <- 'log(unpiped_sewer) + log(piped_sewer) + log(p_density) + log(elevation)'
initial <- InitialEstimates(data = excerpt, terms = terms)


# 1. Bayesian Model
BinomialLogisticBayes(excerpt = excerpt, terms = terms)
