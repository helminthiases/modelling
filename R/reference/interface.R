# Title     : interface.R
# Objective : interface
# Created by: greyhypotheses
# Created on: 27/07/2022


# functions
source(file = 'R/data/StudyData.R')
source(file = 'R/reference/Time.R')
source(file = 'R/reference/Distributions.R')
source(file = 'R/reference/Sewer.R')
source(file = 'R/reference/Water.R')
source(file = 'R/reference/Miscellaneous.R')
source(file = 'R/reference/Extraneous.R')



# a data set: TG, MW, etc
ISO2 <- 'TG'
infection <- 'hk'
add.extraneous <- FALSE
frame <- StudyData(ISO2 = ISO2, infection = infection, add.extraneous = add.extraneous)

if (ISO2 == 'MW') {
  frame <- frame[frame$year %in% c(2012, 2017), ]
}



# the number of observations per year
Time(data = frame)


# graphs
Distributions(data = frame)
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



