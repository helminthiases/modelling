# Title     : interface.R
# Objective : Interface
# Created by: greyhypotheses
# Created on: 26/07/2022


# functions
source(file = 'R/single/StudyData.R')
source(file = 'R/single/DataSplitTemporal.R')
source(file = 'R/single/InitialDiagnostics.R')


# a data set
ISO2 <- 'TG'
infection <- 'hk'
frame <- StudyData(ISO2 = ISO2, infection = infection)


# geography
frame <- sf::st_as_sf(frame, coords = c('longitude', 'latitude'), crs = 'EPSG:4326')
frame$longitude <- as.numeric(st_coordinates(frame)[, 1])
frame$latitude <- as.numeric(st_coordinates(frame)[, 2])

EPSG <- 3857 # Web Mercator https://epsg.io/3857
frame <- sf::st_transform(frame, crs = paste0('EPSG:', EPSG))
frame$x <- as.numeric(st_coordinates(frame)[, 1])
frame$y <- as.numeric(st_coordinates(frame)[, 2])


# Splitting
splits <- list(training = 2009, testing = 2015)
T <- DataSplitTemporal(data = frame, splits = splits)
training <- T$training
testing <- T$testing

InitialDiagnostics(data = training)



