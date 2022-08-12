# Title     : IllustrationSegment.R
# Objective : For the manuscript
# Created by: greyhypotheses
# Created on: 12/08/2022



#' Illustration
#'
#' @param data
#' @param variables
#'
#' @note save(..., file = file.path(pathstr, 'segment'), ascii = TRUE,
#'            compress = TRUE, compression_level = 7)
#'
IllustrationSegment <- function (data, variables) {


  source(file = 'R/effects/Estimates.R')
  source(file = 'R/diagnostics/InitialDiagnostics.R')
  source(file = 'R/functions/SpatialExcerpt.R')


  # The segment
  data <- instances[data$year == 2015, ]


  # Modelling set-up; preventing ill-conditioning
  data <- SpatialExcerpt(data = data, step = 2)


  # setting-up
  pathstr <- file.path(getwd(), 'warehouse', 'effects', 'illustration')
  if (!dir.exists(paths = pathstr)) {
    dir.create(path = pathstr, showWarnings = TRUE, recursive = TRUE)
  }
  if (file.exists(paths = file.path(pathstr, 'segment'))) {
    base::unlink(file.path(pathstr, 'segment'))
  }


  # fixed terms
  .fixed <- function (model) {

    # the coefficient estimates
    template <- summary(object = model)
    estimates <- template$coefficients[, c('Estimate', 'Pr(>|z|)')] %>% data.frame()
    names(estimates) <- c('estimate', 'p.value')

    # their descriptions
    descriptions <- data.frame(term = c('1', 'piped\\underline{\\hspace{0.125cm}}sewer',
                                        'piped\\underline{\\hspace{0.125cm}}sewer$^{2}$', 'elevation.km'),
                               coefficient = c('$\\beta_{0}$', '$\\beta_{1}$', '$\\beta_{2}$', '$\\beta_{3}$'),
                               row.names = c('(Intercept)', 'piped_sewer', 'I(piped_sewer^2)', 'elevation.km'))

    # hence, a comprehensible summary
    T <- merge(x = descriptions, y = estimates,
               by = 0, all.x = TRUE, sort = FALSE) %>% dplyr::select(!Row.names)

    return(T)

  }


  # random terms
  .random <- function (model) {

    template <- summary(object = model)
    deviation <- template$varcor$identifier['(Intercept)', '(Intercept)']

    T <- data.frame(group =  'identifier', variable = '$\\tau^{2}_{_{b}}$',
                    deviation = deviation)

    return(T)

  }


  # spatial correlation
  .graph <- function (points) {
    graph <- ggplot(data = points, mapping = aes(x = distance, y = estimate)) +
      scale_y_continuous(breaks = c(0.70, 0.8, 0.9, 1.00, 1.1), limits = c(0.6, 1.15)) +
      geom_line() +
      geom_ribbon(mapping = aes(ymin = `estimate.lower`, ymax = `estimate.upper`), alpha = 0.3, linetype = 0) +
      geom_point(alpha = 0.65, size = 1) +
      geom_vline(xintercept = 0, alpha = 0.35, size = 0.25) +
      theme_minimal() +
      theme(panel.grid.minor = element_blank(),
            panel.grid.major = element_line(size = 0.05),
            plot.title = element_text(hjust = 0.25, size = 13, face = 'bold'),
            plot.caption = element_text(hjust = 0, size = 11, colour = 'darkgrey'),
            axis.title.x = element_text(size = 12, face = 'bold'), axis.text.x = element_text(size = 10),
            axis.title.y = element_text(size = 12, face = 'bold'), axis.text.y = element_text(size = 10)) +
      xlab(label = '\ndistance\n') +
      ylab(label = '\nvariogram\n')
    return(graph)
  }


  # In focus
  terms <- 'piped_sewer + I(piped_sewer^2) + elevation.km'
  string <- paste(terms, ' + (1|identifier)', collapse = NULL)
  model <- Estimates(data = data, expression = string, variables = variables)
  fixed_ <- .fixed(model = model)
  random_ <- .random(model = model)


  # diagnostics
  LSE <- InitialDiagnostics(data = data, terms = terms, variables = variables)
  points <- data.frame(distance = as.numeric(LSE$distance.bins), estimate = as.numeric(LSE$obs.variogram),
                       estimate.lower = LSE$lower.lim, estimate.upper = LSE$upper.lim)
  graph <- .graph(points = points)
  LSE$graph <- graph


  # save
  save(fixed_, random_, LSE, file = file.path(pathstr, 'segment'), ascii = TRUE,
       compress = TRUE, compression_level = 7)
  ggsave(filename = file.path(pathstr, 'segment.pdf'),
         plot = graph, width = 390, units = 'px', dpi = 95, scale = 1)

}