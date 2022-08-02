# Title     : interface.R
# Objective : interface
# Created by: greyhypotheses
# Created on: 02/08/2022



# functions
source(file = 'R/data/StudyData.R')
source(file = 'R/functions/GeographicObject.R')
source(file = 'R/functions/SpatialSplitting.R')
source(file = 'R/diagnostics/InitialEstimates.R')
source(file = 'R/models/single/stochastic/BinomialLogisticBayes.R')
source(file = 'R/models/single/stochastic/MetricsBLB.R')


# a data set
ISO2 <- 'TG'
infection <- 'hk'
frame <- StudyData(ISO2 = ISO2, infection = infection)


# geographic form
instances <- GeographicObject(data = frame)


# Spatial Splitting
T <- SpatialSplitting(instances = instances, step = 4)
training <- T$training
testing <- T$testing
rm(T)


# Core variables
variables <- list(identifier = 'identifier', tests = 'examined', positives = 'positive')


# Diagnostics
terms <- 'piped_sewer + log(p_density) + elevation.km + I(elevation.km^2)'
initial <- InitialEstimates(data = training, terms = terms, variables = variables)
initial$settings


# Modelling
bayes <- BinomialLogisticBayes(data = training, terms = terms, variables = variables)
metrics <- MetricsBLB(model = bayes$model, training = training, testing = testing)





