# Title     : interface.R
# Objective : Interface
# Created by: greyhypotheses
# Created on: 26/07/2022


# functions
source(file = 'R/data/StudyData.R')
source(file = 'R/functions/GeographicObject.R')
source(file = 'R/functions/SpatialSplitting.R')
source(file = 'R/functions/DataSplitTemporal.R')
source(file = 'R/functions/SpatialExcerpt.R')
source(file = 'R/diagnostics/InitialEstimates.R')
source(file = 'R/models/single/nugget/StepsBLB.R')
source(file = 'R/models/single/nugget/StepsBLM.R')


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


# Splitting
# splits <- list(training = 2009, testing = 2015)
# T <- DataSplitTemporal(data = instances, splits = splits)
# training <- T$training
# testing <- T$testing


# Reducing
# training <- SpatialExcerpt(data = training, step = 3)


# Core variables
variables <- list(identifier = 'identifier', tests = 'examined', positives = 'positive')


# Diagnostics
terms <- 'log(piped_sewer) + log(p_density) + log(elevation)'
initial <- InitialEstimates(data = training, terms = terms, variables = variables)
initial$settings


# Modelling
mcml <- StepsBLM(training = training, testing = testing, terms = terms, variables = variables)
bayes <- StepsBLB(training = training, testing = testing, terms = terms, variables = variables)

mcml$graph.spatial
mcml$graph.diagonal
summary(mcml$model)

bayes$graph.spatial
bayes$graph.diagonal
summary(bayes$model)