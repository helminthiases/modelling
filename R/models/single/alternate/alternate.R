# Title     : alternate.R
# Objective : Alternate
# Created by: greyhypotheses
# Created on: 05/08/2022



# functions
source(file = 'R/data/StudyData.R')
source(file = 'R/functions/GeographicObject.R')
source(file = 'R/functions/SpatialExcerpt.R')
source(file = 'R/diagnostics/InitialEstimates.R')
source(file = 'R/models/single/alternate/BinomialLogisticBayes.R')
source(file = 'R/models/single/alternate/BinomialLogisticMCML.R')


# a data set
ISO2 <- 'TG'
infection <- 'hk'
frame <- StudyData(ISO2 = ISO2, infection = infection)


# geographic form
instances <- GeographicObject(data = frame)
dim(instances)


# Spatial splitting
excerpt <- SpatialExcerpt(data = instances, step = 4)



# Core variables
variables <- list(identifier = 'identifier', tests = 'examined', positives = 'positive')


# Diagnostics
terms <- 'piped_sewer + elevation.km'
initial <- InitialEstimates(data = excerpt, terms = terms, variables = variables)
summary(initial$model)
initial$settings


# Modelling

# ... model, initial
mcml <- BinomialLogisticMCML(data = excerpt, terms = terms, variables = variables)


# ... model, initial
bayes <- BinomialLogisticBayes(data = excerpt, terms = terms, variables = variables)
