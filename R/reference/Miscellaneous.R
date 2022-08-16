# Title     : Miscellaneous.R
# Objective : Miscellaneous
# Created by: greyhypotheses
# Created on: 05/08/2022



#' Graphs of features; excluding WASH variables.
#'
#' @param data: The data
#' @param pathstr:
#'
DisaggregateMiscellaneous <- function (data, pathstr) {

  variables <- c('year', 'elevation.km', 'p_density', 'prevalence')

  instances <- data %>%
    dplyr::select(dplyr::all_of(variables)) %>%
    gather(key = 'miscellaneous', value = 'value', -c(year, prevalence))

  instances$year <- as.factor(instances$year)

  diagram <- ggplot(data = instances, mapping = aes(x = value, y = prevalence, colour = year)) +
    geom_point(alpha = 0.05) +
    geom_smooth(se = FALSE, size = 0.25, method = 'lm', formula = y ~ splines::bs(x, df = 3), linetype = 'solid', na.rm = TRUE) +
    geom_smooth(se = FALSE, size = 0.25, method = 'lm', formula = y ~ x, linetype = 'dashed', na.rm = TRUE) +
    scale_y_continuous(breaks = c(0, 0.5, 1.0), limits = c(0, 1.0)) +
    scale_colour_manual(values = c('black', 'orange')) +
    facet_wrap(~miscellaneous, scales = 'free_x') +
    theme_minimal() +
    theme(panel.spacing = unit(x = 3, units = 'lines'),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(size = 0.05),
          strip.text.x = element_text(face = 'bold', size = 10),
          axis.title.x = element_text(size = 11), axis.title.y = element_text(size = 11),
          axis.text.x = element_text(size = 9), axis.text.y = element_text(size = 9)) +
    xlab(label = '\nvalue\n') +
    ylab(label = '\nprevalence\n')
  print(diagram)
  ggsave(filename = file.path(pathstr, 'miscellaneous', 'disaggregateMiscellaneousReal.pdf'),
         plot = diagram, dpi = 85, width = 575, height = 250, units = 'px', scale = 1)


  diagram <- ggplot(data = instances, mapping = aes(x = log(value), y = prevalence, colour = year)) +
    geom_point(alpha = 0.05) +
    geom_smooth(se = FALSE, size = 0.25, method = 'lm', formula = y ~ splines::bs(x, df = 3), linetype = 'solid', na.rm = TRUE) +
    geom_smooth(se = FALSE, size = 0.25, method = 'lm', formula = y ~ x, linetype = 'dashed', na.rm = TRUE) +
    scale_y_continuous(breaks = c(0, 0.5, 1.0), limits = c(0, 1.0)) +
    scale_colour_manual(values = c('black', 'orange')) +
    facet_wrap(~miscellaneous, scales = 'free_x') +
    theme_minimal() +
    theme(panel.spacing = unit(x = 3, units = 'lines'),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(size = 0.05),
          strip.text.x = element_text(face = 'bold', size = 10),
          axis.title.x = element_text(size = 11), axis.title.y = element_text(size = 11),
          axis.text.x = element_text(size = 9), axis.text.y = element_text(size = 9)) +
    xlab(label = '\nln(value)\n') +
    ylab(label = '\nprevalence\n')
  print(diagram)
  ggsave(filename = file.path(pathstr, 'miscellaneous', 'disaggregateMiscellaneousLN.pdf'),
         plot = diagram, dpi = 85, width = 575, height = 250, units = 'px', scale = 1)

}



#' Graphs of features; excluding WASH variables.
#'
#' @param data: The data
#'
AggregateMiscellaneous <- function (data) {

  variables <- c('elevation.km', 'p_density', 'prevalence')

  instances <- data %>%
    dplyr::select(dplyr::all_of(variables)) %>%
    gather(key = 'miscellaneous', value = 'value', -prevalence)

  graph <- ggplot(data = instances, mapping = aes(x = value, y = prevalence)) +
    geom_point(alpha = 0.05, na.rm = TRUE) +
    geom_smooth(se = FALSE, size = 0.25, method = 'lm', formula = y ~ splines::bs(x, df = 3),
                linetype = 'solid', colour = 'olivedrab', na.rm = TRUE) +
    geom_smooth(se = FALSE, size = 0.25, method = 'lm', formula = y ~ x,
                linetype = 'dashed', colour = 'olivedrab', na.rm = TRUE) +
    scale_y_continuous(breaks = c(0, 0.5, 1.0), limits = c(0, 1.0)) +
    facet_wrap(~miscellaneous, scales = 'free') +
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
    geom_point(alpha = 0.05, na.rm = TRUE) +
    geom_smooth(se = FALSE, size = 0.25, method = 'lm', formula = y ~ splines::bs(x, df = 3),
                linetype = 'solid', colour = 'olivedrab', na.rm = TRUE) +
    geom_smooth(se = FALSE, size = 0.25, method = 'lm', formula = y ~ x,
                linetype = 'dashed', colour = 'olivedrab', na.rm = TRUE) +
    scale_y_continuous(breaks = c(0, 0.5, 1.0), limits = c(0, 1.0)) +
    facet_wrap(~miscellaneous, scales = 'free') +
    theme_minimal() +
    theme(panel.spacing = unit(x = 3, units = 'lines'),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(size = 0.05),
          strip.text.x = element_text(face = 'bold', size = 10),
          axis.title.x = element_text(size = 11), axis.title.y = element_text(size = 11),
          axis.text.x = element_text(size = 9), axis.text.y = element_text(size = 9)) +
    xlab(label = '\nln(value)\n') +
    ylab(label = '\nprevalence\n')
  print(graph)

}
