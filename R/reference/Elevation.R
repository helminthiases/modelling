# Title     : Elevation.R
# Objective : Elevation
# Created by: greyhypotheses
# Created on: 22/08/2022


#' Graphs of features; excluding WASH variables.
#'
#' @param data: The data
#' @param pathstr:
#'
DisaggregateElevation <- function (data, pathstr) {

  variables <- c('year', 'elevation.km', 'EL')

  instances <- data %>%
    dplyr::select(dplyr::all_of(variables))

  instances$year <- as.factor(instances$year)

  diagram <- ggplot(data = instances, mapping = aes(x = elevation.km, y = EL, colour = year)) +
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
    xlab(label = '\nelevation <kilometres>\n') +
    ylab(label = '\nempirical logit (prevalence)\n') +
    guides(colour = guide_legend(title = 'Year'))
  print(diagram)
  ggsave(filename = file.path(pathstr, 'disaggregateElevationReal.pdf'),
         plot = diagram, dpi = 85, width = 395, height = 285, units = 'px', scale = 1)


  diagram <- ggplot(data = instances, mapping = aes(x = log(elevation.km), y = EL, colour = year)) +
    geom_point(alpha = 0.05) +
    geom_smooth(se = FALSE, size = 0.25, method = 'lm', formula = y ~ splines::bs(x, df = 3), linetype = 'solid', na.rm = TRUE) +
    geom_smooth(se = FALSE, size = 0.25, method = 'lm', formula = y ~ x, linetype = 'dashed', na.rm = TRUE) +
    scale_colour_manual(values = c('orange', 'black')) +
    theme_minimal() +
    theme(panel.spacing = unit(x = 3, units = 'lines'),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(size = 0.05),
          strip.text.x = element_text(face = 'bold', size = 10),
          axis.title.x = element_text(size = 11), axis.title.y = element_text(size = 11),
          axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10),
          legend.title = element_text(size = 11), legend.text = element_text(size = 10)) +
    xlab(label = '\nln(elevation <kilometres>)\n') +
    ylab(label = '\nempirical logit (prevalence)\n') +
    guides(colour = guide_legend(title = 'Year'))
  print(diagram)
  ggsave(filename = file.path(pathstr, 'disaggregateElevationLN.pdf'),
         plot = diagram, dpi = 85, width = 395, height = 285, units = 'px', scale = 1)

}



#' Graphs of features; excluding WASH variables.
#'
#' @param data: The data
#'
AggregateElevation <- function (data) {

  variables <- c('elevation.km', 'EL')

  instances <- data %>%
    dplyr::select(dplyr::all_of(variables))

  graph <- ggplot(data = instances, mapping = aes(x = elevation.km, y = EL)) +
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
          axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10)) +
    xlab(label = '\nelevation <kilometres>\n') +
    ylab(label = '\nempirical logit (prevalence)\n')
  print(graph)

  graph <- ggplot(data = instances, mapping = aes(x = log(elevation.km), y = EL)) +
    geom_point(alpha = 0.05, na.rm = TRUE) +
    geom_smooth(se = FALSE, size = 0.25, method = 'lm', formula = y ~ splines::bs(x, df = 3),
                linetype = 'solid', colour = 'olivedrab', na.rm = TRUE) +
    geom_smooth(se = FALSE, size = 0.25, method = 'lm', formula = y ~ x,
                linetype = 'dashed', colour = 'olivedrab', na.rm = TRUE) +
    scale_y_continuous(breaks = c(0, 0.5, 1.0), limits = c(0, 1.0)) +
    theme_minimal() +
    theme(panel.spacing = unit(x = 3, units = 'lines'),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(size = 0.05),
          strip.text.x = element_text(face = 'bold', size = 10),
          axis.title.x = element_text(size = 11), axis.title.y = element_text(size = 11),
          axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10)) +
    xlab(label = '\nln(elevation <kilometres>)\n') +
    ylab(label = '\nempirical logit (prevalence)\n')
  print(graph)

}


#' Elevation
#'
#' @param country: A country's name, e.g., 'Togo'
#' @param ISO3: The country's ISO 3166 Alpha 3 code - 'TGO'
#'
MapElevation <- function (country, ISO3) {

  data(world)
  frame <- world[world$name_long == country, ]
  frame <- sf::st_union(frame)
  rm(world)

  # elevations
  elevations <- geodata::elevation_30s(country = ISO3, path = tempdir())

  # elevations within boundary
  terrain <- terra::mask(elevations, terra::vect(frame))

  diagram <- tm_shape(terrain) +
    tm_layout(main.title = NULL, frame = FALSE, inner.margins = c(0.01, 0.01, 0.01, 0.01),
              outer.margins = c(0.01, 0.01, 0.01, 0.01),
              legend.outside = TRUE, legend.position = c('left', 'center'), legend.outside.size = 0.35,
              legend.title.size = 1.25, legend.height = 4, legend.text.size = 0.95) +
    tm_raster(title = 'Elevation (metres)', midpoint = NA, alpha = 0.60, palette = viridisLite::cividis(n = 11, direction = -1))
  print(diagram)

  tmap::tmap_save(diagram, dpi = 95, width = 625, height = 650, units = 'px',
                  filename = file.path(pathstr, 'mapElevation.pdf'))

}