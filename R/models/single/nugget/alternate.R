# Title     : alternate.R
# Objective : Alternate
# Created by: greyhypotheses
# Created on: 05/08/2022


# functions
source(file = 'R/data/StudyData.R')
source(file = 'R/functions/GeographicObject.R')
source(file = 'R/functions/SpatialSplitting.R')
source(file = 'R/diagnostics/InitialEstimates.R')
source(file = 'R/models/single/nugget/BinomialLogisticBayes.R')
source(file = 'R/models/single/nugget/BinomialLogisticMCML.R')
source(file = 'R/models/single/nugget/MetricsBLB.R')
source(file = 'R/models/single/nugget/MetricsBLM.R')


# a data set
ISO2 <- 'TG'
infection <- 'hk'
frame <- StudyData(ISO2 = ISO2, infection = infection)


# geographic form
instances <- GeographicObject(data = frame)
dim(instances)


# Spatial splitting
T <- SpatialSplitting(instances = instances, step = 4)
training <- T$training
testing <- T$testing
rm(T)


# Core variables
variables <- list(identifier = 'identifier', tests = 'examined', positives = 'positive')


# Diagnostics
terms <- 'piped_sewer + I(piped_sewer^2) + elevation.km'
initial <- InitialEstimates(data = training, terms = terms, variables = variables)
summary(initial$model)
initial$settings


# Modelling

# ... model, initial
mcml <- BinomialLogisticMCML(data = training, terms = terms, variables = variables)
mcml <- MetricsBLM(model = mcml$model, training = training, testing = testing, initial = mcml$initial)

mcml$graph.spatial
mcml$graph.diagonal
summary(mcml$model)

# ... model, initial
bayes <- BinomialLogisticBayes(data = training, terms = terms, variables = variables)
bayes <- MetricsBLB(model = bayes$model, training = training, testing = testing, initial = bayes$initial)

bayes$graph.spatial
bayes$graph.diagonal
summary(bayes$model)
