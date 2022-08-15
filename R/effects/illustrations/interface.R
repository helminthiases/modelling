# Title     : interface.R
# Objective : interface
# Created by: greyhypotheses
# Created on: 12/08/2022


par(bty = 'n', fg = 'grey')


# functions
source(file = 'R/data/StudyData.R')
source(file = 'R/functions/GeographicObject.R')
source(file = 'R/effects/illustrations/IllustrationBaseline.R')
source(file = 'R/effects/illustrations/IllustrationSegment.R')


# a data set
ISO2 <- 'TG'
infection <- 'hk'
frame <- StudyData(ISO2 = ISO2, infection = infection)
frame$year <- factor(frame$year)


# Geographic form
instances <- GeographicObject(data = frame)


# Core variables
variables <- list(identifier = 'identifier', tests = 'examined', positives = 'positive')


# Setting-up
pathstr <- file.path(getwd(), 'warehouse', 'effects', 'illustration')
if (dir.exists(paths = pathstr)) {
  base::unlink(pathstr, recursive = TRUE)
}
if (!dir.exists(paths = pathstr)) {
  dir.create(path = pathstr, showWarnings = TRUE, recursive = TRUE)
}



# Manuscript illustrations
IllustrationBaseline(data = instances, variables = variables)
IllustrationSegment(data = instances, variables = variables)
