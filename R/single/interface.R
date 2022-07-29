# Title     : interface.R
# Objective : Interface
# Created by: greyhypotheses
# Created on: 26/07/2022


# functions
source(file = 'R/single/StudyData.R')
source(file = 'R/single/GeographicObject.R')
source(file = 'R/single/DataSplitTemporal.R')
source(file = 'R/single/InitialDiagnostics.R')
source(file = 'R/single/InitialEstimates.R')


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
terms <- 'log(unpiped_sewer) + log(surface_sewer) + log(piped_sewer) + log(p_density) + log(elevation)'
estimates <- InitialEstimates(data = training, terms = terms)


# Aside
expr <- c('log(improved_sewer)', 'log(unimproved_sewer)', 'log(unpiped_sewer)', 'log(surface_sewer)',
  'log(piped_sewer)', 'log(p_density)', 'log(elevation)')

for (i in seq_len(length.out = 3)) {

  terms <- paste0(expr[i:length(expr)], collapse = ' + ')
  estimates <- InitialEstimates(data = training, terms = terms)
  E <- summary(estimates$model)

  cat('\n', terms, '\n AIC: ', E$AICtab['AIC'], '\n')
  E$coefficients
  cat('\n\n')

}






