# Title     : interface.R
# Objective : Interface
# Created by: greyhypotheses
# Created on: 05/08/2022



# functions
source(file = 'R/data/StudyData.R')
source(file = 'R/functions/GeographicObject.R')
source(file = 'R/functions/SpatialSplitting.R')
source(file = 'R/diagnostics/InitialEstimates.R')
source(file = 'R/models/nugget/C/BinomialLogisticBayes.R')
source(file = 'R/models/nugget/C/BinomialLogisticMCML.R')
source(file = 'R/models/nugget/C/CaseBLB.R')
source(file = 'R/models/nugget/C/CaseBLM.R')



# Setting-up
terms <- 'poly(piped_sewer, 2) + elevation.km'
notes <- list(strings = c('(Intercept)', 'poly(piped_sewer, 2)1', 'poly(piped_sewer, 2)2', 'elevation.km'),
              labels = c('1', 'piped\\underline{\\hspace{0.125cm}}sewer', 'I(piped\\underline{\\hspace{0.125cm}}sewer$^{2}$)', 'elevation.km'),
              parameters = c('$\\beta_{0}$', '$\\beta_{1}$', '$\\beta_{2}$', '$\\beta_{3}$'))



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

# ... diagnostics
initial <- InitialEstimates(data = training, terms = terms, variables = variables, kappa = 0.5)
summary(initial$model)
initial$settings

# ... mcml
mcml <- BinomialLogisticMCML(data = training, terms = terms, variables = variables, kappa = 0.5)
pathstr <- file.path(getwd(), 'warehouse', 'models', 'nugget', 'blm', 'B')
.directory(pathstr = pathstr)
CaseBLM(mcml = mcml, training = training, testing = testing, pathstr = pathstr, notes = notes)



# Bayes

# ... diagnostics
initial <- InitialEstimates(data = training, terms = terms, variables = variables, kappa = 0.5)
summary(initial$model)
initial$settings

# ... bayes
bayes <- BinomialLogisticBayes(data = training, terms = terms, variables = variables, kappa = 0.5)
pathstr <- file.path(getwd(), 'warehouse', 'models', 'nugget', 'blb', 'B')
.directory(pathstr = pathstr)
CaseBLB(bayes = bayes, training = training, testing = testing, pathstr = pathstr, notes = notes)