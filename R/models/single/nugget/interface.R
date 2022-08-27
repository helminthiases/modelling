# Title     : interface.R
# Objective : Interface
# Created by: greyhypotheses
# Created on: 26/07/2022



# Functions
source(file = 'R/data/StudyData.R')
source(file = 'R/functions/GeographicObject.R')
source(file = 'R/functions/SpatialSplitting.R')
source(file = 'R/diagnostics/InitialEstimates.R')
source(file = 'R/models/single/nugget/BinomialLogisticBayes.R')
source(file = 'R/models/single/nugget/BinomialLogisticMCML.R')
source(file = 'R/models/single/nugget/CaseBLB.R')
source(file = 'R/models/single/nugget/CaseBLM.R')


# Setting-up
options <- c('A', 'B', 'C')

cases <- list(A = 'piped_sewer + I(piped_sewer^2) + elevation.km',
              B = 'piped_sewer + log(p_density.k) + elevation.km',
              C = 'piped_sewer + elevation.km')

features <- list(A = list(strings = c('(Intercept)', 'piped_sewer', 'I(piped_sewer^2)', 'elevation.km'),
                          labels = c('1', 'piped\\underline{\\hspace{0.125cm}}sewer', 'I(piped\\underline{\\hspace{0.125cm}}sewer$^{2}$)', 'elevation.km'),
                          parameters = c('$\\beta_{0}$', '$\\beta_{1}$', '$\\beta_{2}$', '$\\beta_{3}$')),
                 B = list(strings = c('(Intercept)', 'piped_sewer', 'log(p_density.k)', 'elevation.km'),
                          labels = c('1', 'piped\\underline{\\hspace{0.125cm}}sewer', 'log(p\\underline{\\hspace{0.125cm}}density.k)', 'elevation.km'),
                          parameters = c('$\\beta_{0}$', '$\\beta_{1}$', '$\\beta_{2}$', '$\\beta_{3}$')),
                 C = list(strings = c('(Intercept)', 'piped_sewer', 'elevation.km'),
                          labels = c('1', 'piped\\underline{\\hspace{0.125cm}}sewer', 'elevation.km'),
                          parameters = c('$\\beta_{0}$', '$\\beta_{1}$', '$\\beta_{2}$')))


# ... a function for preparing directories
.directory <- function (pathstr) {
  if (dir.exists(paths = pathstr)) {
    base::unlink(pathstr, recursive = TRUE)
  }
  if (!dir.exists(paths = pathstr)) {
    dir.create(path = pathstr, showWarnings = TRUE, recursive = TRUE)
  }
}


# The data set
ISO2 <- 'TG'
infection <- 'hk'
frame <- StudyData(ISO2 = ISO2, infection = infection)


# An experiment cycle
frame <- frame[frame$year == 2015, ]
row.names(frame) <- NULL


# Geographic form
instances <- GeographicObject(data = frame)
instances$year <- as.factor(instances$year)


# Spatial splitting
T <- SpatialSplitting(instances = instances, step = 2)
training <- T$training
testing <- T$testing
rm(T)


# Core variables
variables <- list(identifier = 'identifier', tests = 'examined', positives = 'positive')


# MCML
for (option in options){

  # Diagnostics
  terms <- cases[[option]]
  initial <- InitialEstimates(data = training, terms = terms, variables = variables, kappa = 0.5)
  summary(initial$model)
  initial$settings

  # Labels, stings, etc
  notes <- features[[option]]

  # ... mcml
  mcml <- BinomialLogisticMCML(data = training, terms = terms, variables = variables, kappa = 0.5)

  # ... mcml
  pathstr <- file.path(getwd(), 'warehouse', 'models', 'nugget', 'blm', option)
  .directory(pathstr = pathstr)
  CaseBLM(mcml = mcml, training = training, testing = testing, pathstr = pathstr, notes = notes)

}


# Bayes
for (option in options) {

  # Diagnostics
  terms <- cases[[option]]
  initial <- InitialEstimates(data = training, terms = terms, variables = variables, kappa = 0.5)
  summary(initial$model)
  initial$settings

  # Labels, stings, etc
  notes <- features[[option]]

  # ... bayes
  bayes <- BinomialLogisticBayes(data = training, terms = terms, variables = variables, kappa = 0.5)

  # ... bayes
  pathstr <- file.path(getwd(), 'warehouse', 'models', 'nugget', 'blb', option)
  .directory(pathstr = pathstr)
  CaseBLB(bayes = bayes, training = training, testing = testing, pathstr = pathstr, notes = notes)

}
