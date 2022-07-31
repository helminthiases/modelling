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
source(file = 'R/single/BinomialLogisticBayes.R')
source(file = 'R/single/BinomialLogisticBayesEVL.R')


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
excerpt <- SpatialExcerpt(data = training, step = 3)


# Core variables
variables <- list(identifier = 'identifier', tests = 'examined', positives = 'positive')


# Diagnostics
terms <- 'log(unpiped_sewer) + log(piped_sewer) + log(p_density) + log(elevation)'
initial <- InitialEstimates(data = excerpt, terms = terms, variables = variables)


# A Model
objects <- BinomialLogisticBayes(data = excerpt, variables = variables)
model <- objects$model

T <- BinomialLogisticBayesEVL(model = model, excerpt = excerpt, testing = testing)
valuations <- T$valuations
predictions <- T$predictions

trainees <- data.frame(prevalence = excerpt$prevalence, prediction = valuations$prevalence$predictions)
tests <- data.frame(prevalence = testing$prevalence, prediction = predictions$prevalence$predictions)

ggplot(data = trainees, mapping = aes(x = prediction, y = prevalence)) +
  geom_point(alpha = 0.35) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_line(size = 0.05),
        axis.text.x = element_text(size = 9), axis.text.y = element_text(size = 9),
        axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12)) +
  xlab(label = '\nprevalence: prediction\n') +
  ylab(label = '\nprevalence: original\n')
