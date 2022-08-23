# Title     : Distributions.R
# Objective : Distributions
# Created by: greyhypotheses
# Created on: 26/07/2022



#' Prevalence Distributions: Density Graphs
#'
#' @param data: The modelling data set
#' @param pathstr:
#'
DensityDistributions <- function (data, pathstr) {

  data$year <- as.factor(data$year)

  diagram <- ggplot(data = data, mapping = aes(x = prevalence, fill = year)) +
    geom_density(alpha = 0.65, colour = 'white') +
    scale_fill_manual(values = c('orange', 'black')) +
    theme_minimal() +
    theme(panel.spacing = unit(x = 2, units = 'lines'),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(size = 0.05, colour = 'white'),
          strip.text.x = element_text(face = 'bold', size = 10),
          axis.text.x = element_text(size = 9), axis.text.y = element_text(size = 9),
          axis.title.x = element_text(size = 10), axis.title.y = element_text(size = 10),
          legend.title = element_text(size = 10), legend.text = element_text(size = 9)) +
    xlab(label = '\nprevalence\n') +
    ylab(label = '\ndensity\n') +
    guides(fill = guide_legend(title = 'Year'))
  print(diagram)

  ggsave(filename = file.path(pathstr, 'densityDistributions.pdf'), width = 370, height = 205, units = 'px',
         plot = diagram, dpi = 95, scale = 1)

}


#' Prevalence Distributions: Map
#'
#' @param data: The modelling data set
#' @param pathstr:
#'
MapDistributions <- function (data, pathstr) {

  source(file = 'R/functions/GeographicObject.R')

  data$year <- as.factor(data$year)

  instances <- GeographicObject(data = data)

  diagram <- tm_shape(instances) +
    tm_layout(main.title = NULL, frame = FALSE, inner.margins = c(0.01, 0.01, 0.01, 0.01),
              outer.margins = c(0.01, 0.01, 0.01, 0.01), main.title.size = 0.95,
              main.title.color = 'black', main.title.fontface = 'bold', main.title.position = 'center',
              legend.outside = TRUE, legend.position = c('left', 'center'),
              legend.title.size = 1.25, legend.width = -1, legend.height = 2, legend.text.size = 0.95) +
    tm_bubbles(size = 'prevalence',
               col = 'year',
               alpha = 0.65,
               border.col = 'white',
               border.alpha = 0,
               breaks = c(0, 0.05, 0.1, 0.2, 0.5, 1),
               palette = c('orange', 'black'),
               title.size = 'Prevalence',
               title.col = 'Year', scale = 0.80)
  print(diagram)

  tmap::tmap_save(diagram, dpi = 95, width = 625, height = 650, units = 'px',
                  filename = file.path(pathstr, 'mapDistributions.pdf'))

}


#' Prevalence Distributions: Candle/Box Plots
#'
#' @param data: The modelling data set
#'
CandleDistributions <- function (data) {

  ggplot(data = data, mapping = aes(x = as.factor(year), y = prevalence)) +
    geom_boxplot(notch = TRUE, notchwidth = 0.5) +
    geom_jitter(alpha = 0.35, width = 0.25, size = 0.5, colour = 'olivedrab') +
    theme_minimal() +
    theme(panel.grid.minor = element_blank(), panel.grid.major = element_line(size = 0.05),
          axis.text.x = element_text(size = 9, angle = 90, vjust = 0.5), axis.text.y = element_text(size = 9),
          axis.title.x = element_text(size = 11), axis.title.y = element_text(size = 11)) +
    xlab(label = '\n') +
    ylab(label = '\nprevalence\n')

}
