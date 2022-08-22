# Title     : interface.R
# Objective : interface
# Created by: greyhypotheses
# Created on: 27/07/2022


# functions
source(file = 'R/data/StudyData.R')
source(file = 'R/reference/Frequencies.R')
source(file = 'R/reference/Distributions.R')
source(file = 'R/reference/Sewer.R')
source(file = 'R/reference/Water.R')
source(file = 'R/reference/Miscellaneous.R')
source(file = 'R/reference/Extraneous.R')


# Outputs path
pathstr <- file.path(getwd(), 'warehouse', 'reference')


# A data set: The project will focus on TG, and briefly explore MW.
ISO2 <- 'TG'
infection <- 'hk'
frame <- StudyData(ISO2 = ISO2, infection = infection)


# setting-up
sections <- c('distributions', 'frequencies', 'sewer', 'miscellaneous')
for (section in sections) {
  path <- file.path(pathstr, section)
  # delete
  if (dir.exists(paths = path)) {
    base::unlink(path, recursive = TRUE)
  }
  # create
  if (!dir.exists(paths = path)) {
    dir.create(path = path, showWarnings = TRUE, recursive = TRUE)
  }
}


# Longitudinal aspects?
IdentifierFrequencies(data = frame)

# The number of observations per year
TimeFrequencies(data = frame, pathstr = pathstr)

# Graphs
DensityDistributions(data = frame, pathstr = pathstr)
MapDistributions(data = frame, pathstr = pathstr)
CandleDistributions(data = frame)
AggregateSewer(data = frame)
DisaggregateSewer(data = frame, pathstr = pathstr)
AggregateWater(data = frame)
DisaggregateWater(data = frame)
AggregateMiscellaneous(data = frame)
DisaggregateMiscellaneous(data = frame, pathstr = pathstr)

