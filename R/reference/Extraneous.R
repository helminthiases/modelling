# Title     : Extraneous.R
# Objective : Extraneous
# Created by: greyhypotheses
# Created on: 06/08/2022



#' Graphs of climatic features.
#'
#' @param data: The data
#'
DisaggregateExtraneous <- function (data) {

  variables <- c('year', 'AnnualPrecip', 'AnPET', 'AridityIndex', 'prevalence')

  instances <- data %>%
    dplyr::select(dplyr::all_of(variables)) %>%
    gather(key = 'extraneous', value = 'value', -c(year, prevalence))

  instances$year <- as.factor(instances$year)

  graph <- ggplot(data = instances, mapping = aes(x = value, y = prevalence, colour = year)) +
    geom_point(alpha = 0.05) +
    geom_smooth(se = FALSE, size = 0.25, method = 'lm', formula = y ~ splines::bs(x, df = 3), linetype = 'solid') +
    geom_smooth(se = FALSE, size = 0.25, method = 'lm', formula = y ~ x, linetype = 'dashed') +
    scale_colour_manual(values = c('black', 'orange')) +
    facet_wrap(~extraneous, scales = 'free') +
    theme_minimal() +
    theme(panel.spacing = unit(x = 3, units = 'lines'),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(size = 0.05),
          strip.text.x = element_text(face = 'bold', size = 10),
          axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12),
          axis.text.x = element_text(size = 9), axis.text.y = element_text(size = 9)) +
    xlab(label = '\nvalue\n') +
    ylab(label = '\nprevalence\n')
  print(graph)


  graph <- ggplot(data = instances, mapping = aes(x = log(value), y = prevalence, colour = year)) +
    geom_point(alpha = 0.05) +
    geom_smooth(se = FALSE, size = 0.25, method = 'lm', formula = y ~ splines::bs(x, df = 3), linetype = 'solid') +
    geom_smooth(se = FALSE, size = 0.25, method = 'lm', formula = y ~ x, linetype = 'dashed') +
    scale_colour_manual(values = c('black', 'orange')) +
    facet_wrap(~extraneous, scales = 'free') +
    theme_minimal() +
    theme(panel.spacing = unit(x = 3, units = 'lines'),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(size = 0.05),
          strip.text.x = element_text(face = 'bold', size = 10),
          axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12),
          axis.text.x = element_text(size = 9), axis.text.y = element_text(size = 9)) +
    xlab(label = '\nln(value)\n') +
    ylab(label = '\nprevalence\n')
  print(graph)


}



#' Graphs of climatic features.
#'
#' @param data: The data
#'
AggregateExtraneous <- function (data) {

  variables <- c('AnnualPrecip', 'AnPET', 'AridityIndex', 'prevalence')

  instances <- data %>%
    dplyr::select(dplyr::all_of(variables)) %>%
    gather(key = 'extraneous', value = 'value', -prevalence)

  graph <- ggplot(data = instances, mapping = aes(x = value, y = prevalence)) +
    geom_point(alpha = 0.05) +
    geom_smooth(se = FALSE, size = 0.25, method = 'lm', formula = y ~ splines::bs(x, df = 3),
                linetype = 'solid', colour = 'olivedrab') +
    geom_smooth(se = FALSE, size = 0.25, method = 'lm', formula = y ~ x,
                linetype = 'dashed', colour = 'olivedrab') +
    facet_wrap(~extraneous, scales = 'free') +
    theme_minimal() +
    theme(panel.spacing = unit(x = 3, units = 'lines'),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(size = 0.05),
          strip.text.x = element_text(face = 'bold', size = 10),
          axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12),
          axis.text.x = element_text(size = 9), axis.text.y = element_text(size = 9)) +
    xlab(label = '\nvalue\n') +
    ylab(label = '\nprevalence\n')
  print(graph)

  graph <- ggplot(data = instances, mapping = aes(x = log(value), y = prevalence)) +
    geom_point(alpha = 0.05) +
    geom_smooth(se = FALSE, size = 0.25, method = 'lm', formula = y ~ splines::bs(x, df = 3),
                linetype = 'solid', colour = 'olivedrab') +
    geom_smooth(se = FALSE, size = 0.25, method = 'lm', formula = y ~ x,
                linetype = 'dashed', colour = 'olivedrab') +
    facet_wrap(~extraneous, scales = 'free') +
    theme_minimal() +
    theme(panel.spacing = unit(x = 3, units = 'lines'),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(size = 0.05),
          strip.text.x = element_text(face = 'bold', size = 10),
          axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12),
          axis.text.x = element_text(size = 9), axis.text.y = element_text(size = 9)) +
    xlab(label = '\nln(value)\n') +
    ylab(label = '\nprevalence\n')
  print(graph)

}