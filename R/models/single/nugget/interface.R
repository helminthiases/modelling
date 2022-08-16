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


# Case
option <- 2


# A data set
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


# Diagnostics
cases <- list(A = 'piped_sewer + I(piped_sewer^2) + elevation.km',
              B = 'piped_sewer + log(p_density.k) + elevation.km')
terms <- cases[[option]]
initial <- InitialEstimates(data = training, terms = terms, variables = variables, kappa = 0.5)
summary(initial$model)
initial$settings


# Labels, stings, etc
notes <- list(strings = c('(Intercept)', 'piped_sewer', 'log(p_density.k)' , 'elevation.km'),
              labels = c('(Intercept)', 'piped_sewer', 'log(p_density.k)' , 'elevation.km'),
              parameters = c('$\\beta_{0}$', '$\\beta_{1}$', '$\\beta_{2}$', '$\\beta_{3}$'))


# Modelling

# ... mcml
mcml <- BinomialLogisticMCML(data = training, terms = terms, variables = variables, kappa = 0.5)

# ... bayes
bayes <- BinomialLogisticBayes(data = training, terms = terms, variables = variables, kappa = 0.5)




# Evaluating

# ... setting-up
.directory <- function (pathstr) {
  if (dir.exists(paths = pathstr)) {
    base::unlink(pathstr, recursive = TRUE)
  }
  if (!dir.exists(paths = pathstr)) {
    dir.create(path = pathstr, showWarnings = TRUE, recursive = TRUE)
  }
}

# ... mcml
pathstr <- file.path(getwd(), 'warehouse', 'models', 'nugget', 'blm', names(cases)[[option]])
.directory(pathstr = pathstr)
CaseBLM(mcml = mcml, training = training, testing = testing, pathstr = pathstr, notes = notes)

pathstr <- file.path(getwd(), 'warehouse', 'models', 'nugget', 'blb', names(cases)[[option]])
.directory(pathstr = pathstr)
CaseBLB(mcml = mcml, training = training, testing = testing, pathstr = pathstr, notes = notes)