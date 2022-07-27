# Title     : Graphs.R
# Objective : Graphs
# Created by: greyhypotheses
# Created on: 26/07/2022



#' Examinations Sites Elevations
#'
#' @param excerpt: The modelling data set w.r.t. the years of interest
#'
ElevationGraphs <- function (excerpt) {

  ggplot(data = excerpt, mapping = aes(x = elevation, y = prevalence)) +
    geom_point(alpha = 0.35) +
    facet_wrap(~year) +
    theme_minimal() +
    theme(panel.spacing = unit(x = 2, units = 'lines'),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(size = 0.05),
          axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12),
          axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10)) +
    xlab(label = '\nelevation (metres)\n') +
    ylab(label = '\nprevalence\n')

}


#' The Graphs of Population Density
#'
#' @param excerpt: The modelling data set w.r.t. the years of interest
#'
DensityGraphs <- function (excerpt) {

  ggplot(data = excerpt, mapping = aes(x = log(p_density), y = prevalence)) +
    geom_point(alpha = 0.35) +
    facet_wrap(~year) +
    theme_minimal() +
    theme(plot.margin = margin(t = 10, r = 25, b = 10, l = 25, unit = 'pt'),
          panel.spacing = unit(x = 2, units = 'lines'),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(size = 0.05),
          strip.text.x = element_text(face = 'bold', size = 10),
          axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12),
          axis.text.x = element_text(size = 9, angle = 90, vjust = 0.5), axis.text.y = element_text(size = 9)) +
    xlab(label = '\nln(population density)\n(people per square kilometre)\n') +
    ylab(label = '\nprevalence\n')

}


#' The Graphs of the Sewer Access Categories
#'
#' @param excerpt: The modelling data set w.r.t. the years of interest
#'
SewerGraphs <- function (excerpt) {

  variables <- c('year', 'improved_sewer', 'unpiped_sewer', 'surface_sewer', 'piped_sewer', 'unimproved_sewer', 'prevalence')

  instances <- excerpt %>%
    dplyr::select(variables) %>%
    gather(key = 'sewage', value = 'access_percentage', -c(year, prevalence))

  instances$year <- as.factor(instances$year)

  ggplot(data = instances, mapping = aes(x = access_percentage, y = prevalence, colour = year)) +
    geom_point(alpha = 0.35) +
    facet_wrap(~sewage) +
    theme_minimal() +
    theme(panel.spacing = unit(x = 2, units = 'lines'),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(size = 0.05),
          strip.text.x = element_text(face = 'bold', size = 10),
          axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12),
          axis.text.x = element_text(size = 9), axis.text.y = element_text(size = 9)) +
    xlab(label = '\naccess percentage\n') +
    ylab(label = '\nprevalence\n')

}
