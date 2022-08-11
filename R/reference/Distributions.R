# Title     : Distributions.R
# Objective : Distributions
# Created by: greyhypotheses
# Created on: 26/07/2022



#' Prevalence Distributions: Density Graphs
#'
#' @param excerpt: The modelling data set
#'
DensityDistributions <- function (data) {

  data$year <- as.factor(data$year)

  ggplot(data = data, mapping = aes(x = prevalence, fill = year)) +
    geom_density(alpha = 0.35, colour = 'white') +
    theme_minimal() +
    theme(panel.spacing = unit(x = 2, units = 'lines'),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(size = 0.05),
          strip.text.x = element_text(face = 'bold', size = 10),
          axis.text.x = element_text(size = 9), axis.text.y = element_text(size = 9),
          axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12)) +
    xlab(label = '\nprevalence\n') +
    ylab(label = '\ndensity\n')

}


#' Prevalence Distributions: Map
#'
#' @param excerpt: The modelling data set
#'
MapDistributions <- function (data) {

  source(file = 'R/functions/GeographicObject.R')

  data$year <- as.factor(data$year)

  instances <- GeographicObject(data = data)

  tm_shape(instances) +
    tm_layout(main.title = '\n', frame = FALSE, inner.margins = c(0.01, 0.01, 0.01, 0.01),
              outer.margins = c(0.1, 0.1, 0.1, 0.1), main.title.size = 0.95,
              main.title.color = 'black', main.title.fontface = 'bold', main.title.position = 'center',
              legend.outside = TRUE, legend.position = c('left', 'bottom')) +
    tm_bubbles(size = 'prevalence',
               col = 'year',
               alpha = 0.35,
               border.col = 'white',
               border.alpha = 0,
               breaks = c(0, 0.05, 0.1, 0.2, 0.5, 1),
               palette = c('orange', 'black'),
               title.size = 'Prevalence',
               title.col = 'Year', scale = 0.80)

}


#' Prevalence Distributions: Candle/Box Plots
#'
#' @param excerpt: The modelling data set
#'
CandleDistributions <- function (data) {

  ggplot(data = data, mapping = aes(x = as.factor(year), y = prevalence)) +
    geom_boxplot(notch = TRUE, notchwidth = 0.5) +
    geom_jitter(alpha = 0.35, width = 0.25, size = 0.5, colour = 'olivedrab') +
    theme_minimal() +
    theme(panel.grid.minor = element_blank(), panel.grid.major = element_line(size = 0.05),
          axis.text.x = element_text(size = 10, angle = 90, vjust = 0.5), axis.text.y = element_text(size = 10),
          axis.title.x = element_text(size = 11), axis.title.y = element_text(size = 11)) +
    xlab(label = '\n') +
    ylab(label = '\nprevalence\n')

}
