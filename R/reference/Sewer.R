# Title     : Sewer.R
# Objective : Sewer
# Created by: greyhypotheses
# Created on: 03/08/2022



#' The Graphs of the Sewer Access Categories
#'
#' @param data: The data
#'
DisaggregateSewer <- function (data) {

  variables <- c('year', 'improved_sewer', 'unpiped_sewer', 'surface_sewer', 'piped_sewer', 'unimproved_sewer', 'prevalence')

  instances <- data %>%
    dplyr::select(dplyr::all_of(variables)) %>%
    gather(key = 'sewage', value = 'access_percentage', -c(year, prevalence))

  instances$year <- as.factor(instances$year)

  graph <- ggplot(data = instances, mapping = aes(x = access_percentage, y = prevalence, colour = year)) +
    geom_point(alpha = 0.05, na.rm = TRUE) +
    geom_smooth(se = FALSE, size = 0.25, method = 'lm', formula = y ~ splines::bs(x, df = 3), linetype = 'solid') +
    geom_smooth(se = FALSE, size = 0.25, method = 'lm', formula = y ~ x, linetype = 'dashed') +
    scale_colour_manual(values = c('black', 'orange')) +
    facet_wrap(~sewage) +
    theme_minimal() +
    theme(panel.spacing = unit(x = 3, units = 'lines'),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(size = 0.05),
          strip.text.x = element_text(face = 'bold', size = 10),
          axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12),
          axis.text.x = element_text(size = 9), axis.text.y = element_text(size = 9)) +
    xlab(label = '\naccess percentage\n') +
    ylab(label = '\nprevalence\n')
  print(graph)


  graph <- ggplot(data = instances, mapping = aes(x = log(access_percentage), y = prevalence, colour = year)) +
    geom_point(alpha = 0.05, na.rm = TRUE) +
    geom_smooth(se = FALSE, size = 0.25, method = 'lm', formula = y ~ splines::bs(x, df = 3), linetype = 'solid') +
    geom_smooth(se = FALSE, size = 0.25, method = 'lm', formula = y ~ x, linetype = 'dashed') +
    scale_colour_manual(values = c('black', 'orange')) +
    facet_wrap(~sewage) +
    theme_minimal() +
    theme(panel.spacing = unit(x = 3, units = 'lines'),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(size = 0.05),
          strip.text.x = element_text(face = 'bold', size = 10),
          axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12),
          axis.text.x = element_text(size = 9), axis.text.y = element_text(size = 9)) +
    xlab(label = '\nln(access percentage)\n') +
    ylab(label = '\nprevalence\n')
  print(graph)

}



#' The Graphs of the Sewer Access Categories
#'
#' @param data: The data
#'
AggregateSewer <- function (data) {

  variables <- c('improved_sewer', 'unpiped_sewer', 'surface_sewer', 'piped_sewer', 'unimproved_sewer', 'prevalence')

  instances <- data %>%
    dplyr::select(dplyr::all_of(variables)) %>%
    gather(key = 'sewage', value = 'access_percentage', -prevalence)

  graph <- ggplot(data = instances, mapping = aes(x = access_percentage, y = prevalence)) +
    geom_point(alpha = 0.05, na.rm = TRUE) +
    geom_smooth(se = FALSE, size = 0.25, method = 'lm', formula = y ~ splines::bs(x, df = 3), linetype = 'solid', colour = 'olivedrab') +
    geom_smooth(se = FALSE, size = 0.25, method = 'lm', formula = y ~ x, linetype = 'dashed', colour = 'olivedrab') +
    facet_wrap(~sewage) +
    theme_minimal() +
    theme(panel.spacing = unit(x = 3, units = 'lines'),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(size = 0.05),
          strip.text.x = element_text(face = 'bold', size = 10),
          axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12),
          axis.text.x = element_text(size = 9), axis.text.y = element_text(size = 9)) +
    xlab(label = '\naccess percentage\n') +
    ylab(label = '\nprevalence\n')
  print(graph)

  graph <- ggplot(data = instances, mapping = aes(x = log(access_percentage), y = prevalence)) +
    geom_point(alpha = 0.05, na.rm = TRUE) +
    geom_smooth(se = FALSE, size = 0.25, method = 'lm', formula = y ~ splines::bs(x, df = 3), linetype = 'solid', colour = 'olivedrab') +
    geom_smooth(se = FALSE, size = 0.25, method = 'lm', formula = y ~ x, linetype = 'dashed', colour = 'olivedrab') +
    facet_wrap(~sewage) +
    theme_minimal() +
    theme(panel.spacing = unit(x = 3, units = 'lines'),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(size = 0.05),
          strip.text.x = element_text(face = 'bold', size = 10),
          axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12),
          axis.text.x = element_text(size = 9), axis.text.y = element_text(size = 9)) +
    xlab(label = '\nln(access percentage)\n') +
    ylab(label = '\nprevalence\n')
  print(graph)

}
