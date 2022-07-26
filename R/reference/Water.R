# Title     : Water.R
# Objective : Water
# Created by: greyhypotheses
# Created on: 06/08/2022


#' The Graphs of the Water Access Categories
#'
#' @param data: The data
#'
DisaggregateWater <- function (data) {

  variables <- c('year', 'improved_water', 'unpiped_water', 'surface_water', 'piped_water', 'unimproved_water', 'EL')

  instances <- data %>%
    dplyr::select(dplyr::all_of(variables)) %>%
    gather(key = 'water', value = 'access_percentage', -c(year, EL))

  instances$year <- as.factor(instances$year)

  graph <- ggplot(data = instances, mapping = aes(x = access_percentage, y = EL, colour = year)) +
    geom_point(alpha = 0.05, na.rm = TRUE) +
    geom_smooth(se = FALSE, size = 0.25, method = 'lm', formula = y ~ splines::bs(x, df = 3), linetype = 'solid', na.rm = TRUE) +
    geom_smooth(se = FALSE, size = 0.25, method = 'lm', formula = y ~ x, linetype = 'dashed', na.rm = TRUE) +
    scale_x_continuous(breaks = c(0, 0.5, 1.0), limits = c(0, 1.0)) +
    scale_colour_manual(values = c('orange', 'black')) +
    facet_wrap(~water) +
    theme_minimal() +
    theme(panel.spacing = unit(x = 2, units = 'lines'),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(size = 0.05),
          strip.text.x = element_text(face = 'bold', size = 10),
          axis.title.x = element_text(size = 11, hjust = 0.5, margin = margin(t = 9, r = 0, b = 9, l = 0, unit = 'pt')),
          axis.title.y = element_text(size = 11),
          axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10)) +
    xlab(label = TeX(input = 'access fraction, $1$ $\\equiv$ $100\\%$') ) +
    ylab(label = '\nempirical logit (prevalence)\n')
  print(graph)


  graph <- ggplot(data = instances, mapping = aes(x = log(access_percentage), y = EL, colour = year)) +
    geom_point(alpha = 0.05, na.rm = TRUE) +
    geom_smooth(se = FALSE, size = 0.25, method = 'lm', formula = y ~ splines::bs(x, df = 3), linetype = 'solid', na.rm = TRUE) +
    geom_smooth(se = FALSE, size = 0.25, method = 'lm', formula = y ~ x, linetype = 'dashed', na.rm = TRUE) +
    scale_colour_manual(values = c('orange', 'black')) +
    facet_wrap(~water) +
    theme_minimal() +
    theme(panel.spacing = unit(x = 3, units = 'lines'),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(size = 0.05),
          strip.text.x = element_text(face = 'bold', size = 10),
          axis.title.x = element_text(size = 11, hjust = 0.5, margin = margin(t = 9, r = 0, b = 9, l = 0, unit = 'pt')),
          axis.title.y = element_text(size = 11),
          axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10)) +
    xlab(label = TeX(input = 'ln(access fraction), ln($1$) $\\equiv$ ln($100\\%$)') ) +
    ylab(label = '\nempirical logit (prevalence)\n')
  print(graph)

}



#' The Graphs of the Water Access Categories
#'
#' @param data: The data
#'
AggregateWater <- function (data) {

  variables <- c('improved_water', 'unpiped_water', 'surface_water', 'piped_water', 'unimproved_water', 'EL')

  instances <- data %>%
    dplyr::select(dplyr::all_of(variables)) %>%
    gather(key = 'water', value = 'access_percentage', -EL)

  graph <- ggplot(data = instances, mapping = aes(x = access_percentage, y = EL)) +
    geom_point(alpha = 0.05, na.rm = TRUE) +
    geom_smooth(se = FALSE, size = 0.25, method = 'lm', formula = y ~ splines::bs(x, df = 3), linetype = 'solid', colour = 'olivedrab', na.rm = TRUE) +
    geom_smooth(se = FALSE, size = 0.25, method = 'lm', formula = y ~ x, linetype = 'dashed', colour = 'olivedrab', na.rm = TRUE) +
    scale_x_continuous(breaks = c(0, 0.5, 1.0), limits = c(0, 1.0)) +
    facet_wrap(~water) +
    theme_minimal() +
    theme(panel.spacing = unit(x = 3, units = 'lines'),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(size = 0.05),
          strip.text.x = element_text(face = 'bold', size = 10),
          axis.title.x = element_text(size = 11, hjust = 0.5, margin = margin(t = 9, r = 0, b = 9, l = 0, unit = 'pt')),
          axis.title.y = element_text(size = 11),
          axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10)) +
    xlab(label = TeX(input = 'access fraction, $1$ $\\equiv$ $100\\%$') ) +
    ylab(label = '\nempirical logit (prevalence)\n')
  print(graph)

  graph <- ggplot(data = instances, mapping = aes(x = log(access_percentage), y = EL)) +
    geom_point(alpha = 0.05, na.rm = TRUE) +
    geom_smooth(se = FALSE, size = 0.25, method = 'lm', formula = y ~ splines::bs(x, df = 3), linetype = 'solid', colour = 'olivedrab', na.rm = TRUE) +
    geom_smooth(se = FALSE, size = 0.25, method = 'lm', formula = y ~ x, linetype = 'dashed', colour = 'olivedrab', na.rm = TRUE) +
    facet_wrap(~water) +
    theme_minimal() +
    theme(panel.spacing = unit(x = 3, units = 'lines'),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(size = 0.05),
          strip.text.x = element_text(face = 'bold', size = 10),
          axis.title.x = element_text(size = 11, hjust = 0.5, margin = margin(t = 9, r = 0, b = 9, l = 0, unit = 'pt')),
          axis.title.y = element_text(size = 11),
          axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10)) +
    xlab(label = TeX(input = 'ln(access fraction), ln($1$) $\\equiv$ ln($100\\%$)') ) +
    ylab(label = '\nempirical logit (prevalence)\n')
  print(graph)

}
