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
          axis.title.x = element_text(size = 11), axis.title.y = element_text(size = 11),
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
    theme_minimal() +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_line(size = 0.05),
          strip.text.x = element_text(face = 'bold', size = 10),
          axis.title.x = element_text(size = 11), axis.title.y = element_text(size = 11),
          axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10)) +
    xlab(label = '\nthousand inhabitants per\nsquare kilometres\n') +
    ylab(label = '\nempirical logit (prevalence)\n')
  print(graph)

  graph <- ggplot(data = instances, mapping = aes(x = log(p_density.k), y = EL)) +
    geom_point(alpha = 0.05, na.rm = TRUE) +
    geom_smooth(se = FALSE, size = 0.25, method = 'lm', formula = y ~ splines::bs(x, df = 3),
                linetype = 'solid', colour = 'olivedrab', na.rm = TRUE) +
    geom_smooth(se = FALSE, size = 0.25, method = 'lm', formula = y ~ x,
                linetype = 'dashed', colour = 'olivedrab', na.rm = TRUE) +
    theme_minimal() +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_line(size = 0.05),
          strip.text.x = element_text(face = 'bold', size = 10),
          axis.title.x = element_text(size = 11), axis.title.y = element_text(size = 11),
          axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10)) +
    xlab(label = '\nln(thousand inhabitants per\nsquare kilometres)\n') +
    ylab(label = '\nempirical logit (prevalence)\n')
  print(graph)

}


#' Population
#'
MapPopulation <- function (data, pathstr, year = 2015) {

  source(file = 'R/functions/GeographicObject.R')

  data <- data %>% dplyr::filter(year == year)
  instances <- GeographicObject(data = data)

  diagram <- tm_shape(instances) +
    tm_layout(main.title = NULL, frame = FALSE, inner.margins = c(0.04, 0.02, 0.04, 0.02),
              outer.margins = c(0.02, 0.02, 0.02, 0.02), main.title.size = 0.95,
              main.title.color = 'black', main.title.fontface = 'bold', main.title.position = 'center',
              legend.outside = TRUE, legend.position = c('left', 'center'),
              legend.title.color = 1.25, legend.width = -1, legend.height = 2, legend.text.size = 0.95) +
    tm_bubbles(size = 'p_density.k',
               alpha = 0.65,
               col = 'p_density.k',
               border.col = 'white',
               border.alpha = 0,
               breaks = c(0, 1, 2, 4, 6, 8, 10),
               palette = paletteer::paletteer_c(palette = 'ggthemes::Green-Gold', n = 35),
               legend.shape.show = FALSE, legend.size.show = FALSE,
               title.col = 'Population Density, 2015\n(thousand per sq. km)')
  print(diagram)

  tmap::tmap_save(diagram, dpi = 95, width = 625, height = 650, units = 'px',
                  filename = file.path(pathstr, 'mapPopulation.pdf'))

}



