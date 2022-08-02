# Title     : Distributions.R
# Objective : Distributions
# Created by: greyhypotheses
# Created on: 26/07/2022



#' Prevalence Distributions
#'
#' @param excerpt: The modelling data set w.r.t. the years of interest
#'
Distributions <- function (data) {

  ggplot(data = data, mapping = aes(x = prevalence)) +
    geom_density() +
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
