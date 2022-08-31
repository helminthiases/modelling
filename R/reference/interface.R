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
source(file = 'R/reference/Elevation.R')
source(file = 'R/reference/Population.R')


# Outputs path
pathstr <- file.path(getwd(), 'warehouse', 'reference')


# A data set: The project will focus on TG, and briefly explore MW.
country <- 'Togo'
ISO2 <- 'TG'
ISO3 <- 'TGO'
infection <- 'hk'
frame <- StudyData(ISO2 = ISO2, infection = infection)
frame$EL <- {(frame$positive + 0.5) / (frame$examined - frame$positive + 0.5)} %>%
  log()


# setting-up
sections <- c('distributions', 'frequencies', 'sewer', 'extraneous')
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
DensityDistributions(data = frame, pathstr = file.path(pathstr, 'distributions'))
MapDistributions(data = frame, pathstr = file.path(pathstr, 'distributions'))
CandleDistributions(data = frame)

AggregateSewer(data = frame)
DisaggregateSewer(data = frame, pathstr = file.path(pathstr, 'sewer'))

AggregateWater(data = frame)
DisaggregateWater(data = frame)

AggregateElevation(data = frame)
DisaggregateElevation(data = frame, pathstr = file.path(pathstr, 'extraneous'))

AggregatePopulation(data = frame)
DisaggregatePopulation(data = frame, pathstr = file.path(pathstr, 'extraneous'))


