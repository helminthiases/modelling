# Title     : IllustrationsBaseline.R
# Objective : For the manuscript
# Created by: greyhypotheses
# Created on: 12/08/2022


#' Illustration
#'
#' @param data
#' @param variables
#'
#' @note save(..., file = file.path(pathstr, 'baseline'), ascii = TRUE,
#'            compress = TRUE, compression_level = 7)
#'
IllustrationsBaseline <- function (data, variables) {


  source(file = 'R/effects/Estimates.R')
  source(file = 'R/diagnostics/InitialDiagnostics.R')
  source(file = 'R/functions/ConfidenceInterval.R')


  # fixed terms
  .fixed <- function (model) {

    # the coefficient estimates
    template <- summary(object = model)
    estimates <- template$coefficients[, c('Estimate', 'Std. Error', 'Pr(>|z|)')] %>% data.frame()
    names(estimates) <- c('est', 'SE', 'p.value')

    # their descriptions
    descriptions <- data.frame(term = c('1', 'piped\\underline{\\hspace{0.125cm}}sewer',
                                        'piped\\underline{\\hspace{0.125cm}}sewer$^{2}$', 'elevation.km'),
                               coefficient = c('$\\beta_{0}$', '$\\beta_{1}$', '$\\beta_{2}$', '$\\beta_{3}$'),
                               row.names = c('(Intercept)', 'piped_sewer', 'I(piped_sewer^2)', 'elevation.km'))

    # hence, a comprehensible summary
    T <- merge(x = descriptions, y = estimates,
               by = 0, all.x = TRUE, sort = FALSE) %>% dplyr::select(!Row.names)

    # CI
    T <- CoefficientConfidenceInterval(parameters = T)
    T <- T %>% select(term, coefficient, '2.5 %', '97.5 %', SE, 'p.value')


    return(T)

  }


  # random terms
  .random <- function (model) {

    template <- summary(object = model)
    deviations <- data.frame(template$varcor) %>%
      dplyr::select(grp, vcov)
    deviations <- dplyr::rename(deviations, 'group' = 'grp', 'variance' = 'vcov')

    descriptions <- data.frame(
      group =  c('identifier', 'year'),
      variable = c('$\\tau^{2}_{_{b}}$', '$\\tau^{2}_{_{c}}$'))

    T <- dplyr::left_join(x = descriptions, y = deviations, by = 'group')

    return(T)

  }


  # spatial correlation
  .graph <- function (points) {
    graph <- ggplot(data = points, mapping = aes(x = distance, y = estimate)) +
      scale_y_continuous(breaks = c(0.70, 0.9, 1.1), limits = c(0.6, 1.15)) +
      geom_line() +
      geom_ribbon(mapping = aes(ymin = `estimate.lower`, ymax = `estimate.upper`), alpha = 0.3, linetype = 0) +
      geom_point(alpha = 0.65, size = 1, na.rm = TRUE) +
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
  terms <- 'piped_sewer + I(piped_sewer^2) + elevation.km + (1|year)'
  string <- paste(terms, ' + (1|identifier)', collapse = NULL)
  model <- Estimates(data = data, expression = string, variables = variables)
  fixed_ <- .fixed(model = model)
  random_ <- .random(model = model)


  # diagnostics
  LSE <- InitialDiagnostics(data = data, terms = terms, variables = variables, kappa = 1.5)
  points <- data.frame(distance = as.numeric(LSE$distance.bins), estimate = as.numeric(LSE$obs.variogram),
                       estimate.lower = LSE$lower.lim, estimate.upper = LSE$upper.lim)
  graph <- .graph(points = points)
  LSE$graph <- graph


  # save
  save(fixed_, random_, file = file.path(pathstr, 'baseline.Rdata'), ascii = TRUE,
       compress = TRUE, compression_level = 7)
  ggsave(filename = file.path(pathstr, 'baseline.pdf'),
         plot = graph, width = 390, units = 'px', dpi = 95, scale = 1)


}








