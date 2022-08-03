# Title     : interface.R
# Objective : Interface
# Created by: greyhypotheses
# Created on: 26/07/2022



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
