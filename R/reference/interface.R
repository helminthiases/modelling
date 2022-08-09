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



# A data set: The project will focus on TG, and briefly explore MW.
ISO2 <- 'TG'
infection <- 'hk'
add.extraneous <- FALSE
frame <- StudyData(ISO2 = ISO2, infection = infection, add.extraneous = add.extraneous)


# Longitudinal aspects?
IdentifierFrequencies(data = frame)


# The number of observations per year
TimeFrequencies(data = frame)


# Graphs
DensityDistributions(data = frame)
MapDistributions(data = frame)
CandleDistributions(data = frame)
AggregateSewer(data = frame)
DisaggregateSewer(data = frame)
AggregateWater(data = frame)
DisaggregateWater(data = frame)
AggregateMiscellaneous(data = frame)
DisaggregateMiscellaneous(data = frame)

if (add.extraneous) {
  AggregateExtraneous(data = frame)
  DisaggregateExtraneous(data = frame)
}



