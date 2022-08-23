# Title     : Population.R
# Objective : Population
# Created by: greyhypotheses
# Created on: 05/08/2022



#' Graphs of features; excluding WASH variables.
#'
#' @param data: The data
#' @param pathstr:
#'
DisaggregatePopulation <- function (data, pathstr) {

  variables <- c('year', 'p_density.k', 'EL')

  instances <- data %>%
    dplyr::select(dplyr::all_of(variables))

  instances$year <- as.factor(instances$year)

  diagram <- ggplot(data = instances, mapping = aes(x = p_density.k, y = EL, colour = year)) +
    geom_point(alpha = 0.05) +
    geom_smooth(se = FALSE, size = 0.25, method = 'lm', formula = y ~ splines::bs(x, df = 3), linetype = 'solid', na.rm = TRUE) +
    geom_smooth(se = FALSE, size = 0.25, method = 'lm', formula = y ~ x, linetype = 'dashed', na.rm = TRUE) +
    scale_colour_manual(values = c('orange', 'black')) +
    theme_minimal() +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_line(size = 0.05),
          strip.text.x = element_text(face = 'bold', size = 10),
          axis.title.x = element_text(size = 11), axis.title.y = element_text(size = 11),
          axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10),
          legend.title = element_text(size = 11), legend.text = element_text(size = 10)) +
    xlab(label = '\nthousand inhabitants per\nsquare kilometres\n') +
    ylab(label = '\nempirical logit (prevalence)\n') +
    guides(colour = guide_legend(title = 'Year'))
  print(diagram)
  ggsave(filename = file.path(pathstr, 'disaggregatePopulationDensityReal.pdf'),
         plot = diagram, dpi = 85, width = 395, height = 285, units = 'px', scale = 1)


  diagram <- ggplot(data = instances, mapping = aes(x = log(p_density.k), y = EL, colour = year)) +
    geom_point(alpha = 0.05) +
    geom_smooth(se = FALSE, size = 0.25, method = 'lm', formula = y ~ splines::bs(x, df = 3), linetype = 'solid', na.rm = TRUE) +
    geom_smooth(se = FALSE, size = 0.25, method = 'lm', formula = y ~ x, linetype = 'dashed', na.rm = TRUE) +
    scale_colour_manual(values = c('orange', 'black')) +
    theme_minimal() +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_line(size = 0.05),
          strip.text.x = element_text(face = 'bold', size = 10),
          axis.title.x = element_text(size = 11, face = 'italic'), axis.title.y = element_text(size = 11),
          axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10),
          legend.title = element_text(size = 11), legend.text = element_text(size = 10)) +
    xlab(label = '\nln(thousand inhabitants\nper square kilometres)\n') +
    ylab(label = '\nempirical logit (prevalence)\n') +
    guides(colour = guide_legend(title = 'Year'))
  print(diagram)
  ggsave(filename = file.path(pathstr, 'disaggregatePopulationDensityLN.pdf'),
         plot = diagram, dpi = 85, width = 395, height = 285, units = 'px', scale = 1)

}



#' Graphs of features; excluding WASH variables.
#'
#' @param data: The data
#'
AggregatePopulation <- function (data) {

  variables <- c('p_density.k', 'EL')

  instances <- data %>%
    dplyr::select(dplyr::all_of(variables))

  graph <- ggplot(data = instances, mapping = aes(x = p_density.k, y = EL)) +
    geom_point(alpha = 0.05, na.rm = TRUE) +
    geom_smooth(se = FALSE, size = 0.25, method = 'lm', formula = y ~ splines::bs(x, df = 3),
                linetype = 'solid', colour = 'olivedrab', na.rm = TRUE) +
    geom_smooth(se = FALSE, size = 0.25, method = 'lm', formula = y ~ x,
                linetype = 'dashed', colour = 'olivedrab', na.rm = TRUE) +
    scale_y_continuous(breaks = c(0, 0.5, 1.0), limits = c(0, 1.0)) +
    theme_minimal() +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_line(size = 0.05),
          strip.text.x = element_text(face = 'bold', size = 10),
          axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12),
          axis.text.x = element_text(size = 9), axis.text.y = element_text(size = 9)) +
    xlab(label = '\nthousand inhabitants per square kilometres\n') +
    ylab(label = '\nempirical logit (prevalence)\n')
  print(graph)

  graph <- ggplot(data = instances, mapping = aes(x = log(p_density.k), y = EL)) +
    geom_point(alpha = 0.05, na.rm = TRUE) +
    geom_smooth(se = FALSE, size = 0.25, method = 'lm', formula = y ~ splines::bs(x, df = 3),
                linetype = 'solid', colour = 'olivedrab', na.rm = TRUE) +
    geom_smooth(se = FALSE, size = 0.25, method = 'lm', formula = y ~ x,
                linetype = 'dashed', colour = 'olivedrab', na.rm = TRUE) +
    scale_y_continuous(breaks = c(0, 0.5, 1.0), limits = c(0, 1.0)) +
    theme_minimal() +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_line(size = 0.05),
          strip.text.x = element_text(face = 'bold', size = 10),
          axis.title.x = element_text(size = 11), axis.title.y = element_text(size = 11),
          axis.text.x = element_text(size = 9), axis.text.y = element_text(size = 9)) +
    xlab(label = '\nln(thousand inhabitants per square kilometres)\n') +
    ylab(label = '\nempirical logit (prevalence)\n')
  print(graph)

}
