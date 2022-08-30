# Title     : interface.R
# Objective : Interface
# Created by: greyhypotheses
# Created on: 02/08/2022



# functions
source(file = '../../../data/StudyData.R')
source(file = '../../../functions/GeographicObject.R')
source(file = '../../../functions/SpatialSplitting.R')
source(file = '../../../diagnostics/InitialEstimates.R')
source(file = 'BinomialLogisticBayes.R')
source(file = 'BinomialLogisticMCML.R')



# a data set
ISO2 <- 'TG'
infection <- 'hk'
frame <- StudyData(ISO2 = ISO2, infection = infection)


# An experiment cycle
frame <- frame[frame$year == 2015, ]
row.names(frame) <- NULL


# geographic form
instances <- GeographicObject(data = frame)


# Spatial Splitting
T <- SpatialSplitting(instances = instances, step = 2)
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


# ... model, initial
bayes <- BinomialLogisticBayes(data = training, terms = terms, variables = variables)
