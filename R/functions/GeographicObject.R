# Title     : GeographicObject.R
# Objective : Converting from a data frame object, to a geographic object (sf + data.frame)
# Created by: greyhypotheses
# Created on: 27/07/2022

#'
#'
#' @param data: an object of class data.frame that includes the fields <longitude> & <latitude>
#'
GeographicObject <- function (data) {

  # Convert the data frame to a geographic object whose CRS = 4326
  frame <- sf::st_as_sf(data, coords = c('longitude', 'latitude'), crs = 'EPSG:4326')

  frame$longitude <- as.numeric(sf::st_coordinates(frame)[, 1])
  frame$latitude <- as.numeric(sf::st_coordinates(frame)[, 2])


  # Transforming: from 4326 to 3857
  EPSG <- 3857 # Web Mercator https://epsg.io/3857

  frame <- sf::st_transform(frame, crs = paste0('EPSG:', EPSG))
  frame$x <- as.numeric(st_coordinates(frame)[, 1])
  frame$y <- as.numeric(st_coordinates(frame)[, 2])

  return(frame)

}