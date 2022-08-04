# Title     : interface.R
# Objective : interface
# Created by: greyhypotheses
# Created on: 27/07/2022


# functions
source(file = 'R/data/StudyData.R')
source(file = 'R/reference/Time.R')
source(file = 'R/reference/Distributions.R')
source(file = 'R/reference/Graphs.R')
source(file = 'R/reference/AggregateGraphs.R')



# a data set
ISO2 <- 'TG'
infection <- 'hk'
frame <- StudyData(ISO2 = ISO2, infection = infection)


# the number of observations per year
Time(data = frame)


# graphs
Distributions(data = frame)

SewerGraphs(data = frame)
SewerAggregateGraphs(data = frame)

MiscellaneousGraphs(data = frame)
MiscellaneousAggregateGraphs(data = frame)





