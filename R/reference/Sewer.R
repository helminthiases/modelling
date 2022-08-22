# Title     : Sewer.R
# Objective : Sewer
# Created by: greyhypotheses
# Created on: 03/08/2022



#' The Graphs of the Sewer Access Categories
#'
#' @param data: The data
#' @param pathstr:
#'
DisaggregateSewer <- function (data, pathstr) {

  variables <- c('year', 'unpiped_sewer', 'surface_sewer', 'piped_sewer', 'EL')

  instances <- data %>%
    dplyr::select(dplyr::all_of(variables)) %>%
    gather(key = 'sewage', value = 'access_percentage', -c(year, EL))

  instances$year <- as.factor(instances$year)

  diagram <- ggplot(data = instances, mapping = aes(x = access_percentage, y = EL, colour = year)) +
    geom_point(alpha = 0.05, na.rm = TRUE) +
    geom_smooth(se = FALSE, size = 0.25, method = 'lm', formula = y ~ splines::bs(x, df = 3), linetype = 'solid', na.rm = TRUE) +
    geom_smooth(se = FALSE, size = 0.25, method = 'lm', formula = y ~ x, linetype = 'dashed', na.rm = TRUE) +
    scale_x_continuous(breaks = c(0, 0.5, 1.0), limits = c(0, 1.0)) +
    scale_colour_manual(values = c('orange', 'black')) +
    facet_wrap(~sewage, ncol = 2) +
    theme_minimal() +
    theme(panel.spacing = unit(x = 2, units = 'lines'),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(size = 0.01),
          strip.text.x = element_text(face = 'bold', size = 10),
          axis.title.x = element_text(size = 10, lineheight = 1.25), axis.text.x = element_text(size = 9),
          axis.title.y = element_text(size = 10), axis.text.y = element_text(size = 9),
          legend.title = element_text(size = 10), legend.text = element_text(size = 9)) +
    xlab(label = '\naccess fraction\n 1 ≡ 100%\n') +
    ylab(label = '\nempirical logit (prevalence)\n') +
    guides(colour = guide_legend(title = 'Year'))
  print(diagram)
  ggsave(filename = file.path(pathstr, 'sewer', 'disaggregateSewerReal.pdf'),
         plot = diagram, dpi = 85, scale = 1, width = 675, height = 375, units = 'px')


  diagram <- ggplot(data = instances, mapping = aes(x = log(access_percentage), y = EL, colour = year)) +
    geom_point(alpha = 0.05, na.rm = TRUE) +
    geom_smooth(se = FALSE, size = 0.25, method = 'lm', formula = y ~ splines::bs(x, df = 3), linetype = 'solid', na.rm = TRUE) +
    geom_smooth(se = FALSE, size = 0.25, method = 'lm', formula = y ~ x, linetype = 'dashed', na.rm = TRUE) +
    scale_colour_manual(values = c('orange', 'black')) +
    facet_wrap(~sewage, ncol = 2) +
    theme_minimal() +
    theme(panel.spacing = unit(x = 2, units = 'lines'),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(size = 0.01),
          strip.text.x = element_text(face = 'bold', size = 10),
          axis.title.x = element_text(size = 10, lineheight = 1.25), axis.text.x = element_text(size = 9),
          axis.title.y = element_text(size = 10), axis.text.y = element_text(size = 9),
          legend.title = element_text(size = 10), legend.text = element_text(size = 9)) +
    xlab(label = '\nln(access fraction)\n ln(1) ≡ ln(100%)\n') +
    ylab(label = '\nempirical logit (prevalence)\n') +
    guides(colour = guide_legend(title = 'Year'))
  print(diagram)
  ggsave(filename = file.path(pathstr, 'sewer', 'disaggregateSewerLN.pdf'),
         plot = diagram, dpi = 85, scale = 1, width = 675, height = 375, units = 'px')

}



#' The Graphs of the Sewer Access Categories
#'
#' @param data: The data
#'
AggregateSewer <- function (data) {

  variables <- c('unpiped_sewer', 'surface_sewer', 'piped_sewer', 'prevalence')

  instances <- data %>%
    dplyr::select(dplyr::all_of(variables)) %>%
    gather(key = 'sewage', value = 'access_percentage', -EL)

  graph <- ggplot(data = instances, mapping = aes(x = access_percentage, y = EL)) +
    geom_point(alpha = 0.05, na.rm = TRUE) +
    geom_smooth(se = FALSE, size = 0.25, method = 'lm', formula = y ~ splines::bs(x, df = 3), linetype = 'solid', colour = 'olivedrab') +
    geom_smooth(se = FALSE, size = 0.25, method = 'lm', formula = y ~ x, linetype = 'dashed', colour = 'olivedrab') +
    scale_x_continuous(breaks = c(0, 0.5, 1.0), limits = c(0, 1.0)) +
    scale_y_continuous(breaks = c(0, 0.5, 1.0), limits = c(0, 1.0)) +
    facet_wrap(~sewage, ncol = 2) +
    theme_minimal() +
    theme(panel.spacing = unit(x = 3, units = 'lines'),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(size = 0.05),
          strip.text.x = element_text(face = 'bold', size = 10),
          axis.title.x = element_text(size = 11), axis.title.y = element_text(size = 11),
          axis.text.x = element_text(size = 9), axis.text.y = element_text(size = 9)) +
    xlab(label = '\naccess fraction\n 1 ≡ 100%\n') +
    ylab(label = '\nempirical logit (prevalence)\n')
  print(graph)

  graph <- ggplot(data = instances, mapping = aes(x = log(access_percentage), y = EL)) +
    geom_point(alpha = 0.05, na.rm = TRUE) +
    geom_smooth(se = FALSE, size = 0.25, method = 'lm', formula = y ~ splines::bs(x, df = 3), linetype = 'solid', colour = 'olivedrab') +
    geom_smooth(se = FALSE, size = 0.25, method = 'lm', formula = y ~ x, linetype = 'dashed', colour = 'olivedrab') +
    scale_y_continuous(breaks = c(0, 0.5, 1.0), limits = c(0, 1.0)) +
    facet_wrap(~sewage, ncol = 2) +
    theme_minimal() +
    theme(panel.spacing = unit(x = 3, units = 'lines'),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(size = 0.05),
          strip.text.x = element_text(face = 'bold', size = 10),
          axis.title.x = element_text(size = 11), axis.title.y = element_text(size = 11),
          axis.text.x = element_text(size = 9), axis.text.y = element_text(size = 9)) +
    xlab(label = '\nln(access fraction)\n ln(1) ≡ ln(100%)\n') +
    ylab(label = '\nempirical logit (prevalence)\n')
  print(graph)

}
