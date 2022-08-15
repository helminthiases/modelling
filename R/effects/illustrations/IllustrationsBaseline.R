# Title     : IllustrationBaseline.R
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
IllustrationBaseline <- function (data, variables) {


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


  # In focus
  terms <- 'piped_sewer + I(piped_sewer^2) + elevation.km'
  string <- paste(terms, ' + (1|identifier) + (1|year)', collapse = NULL)
  model <- Estimates(data = data, expression = string, variables = variables)
  fixed_ <- .fixed(model = model)
  random_ <- .random(model = model)


  # save
  save(fixed_, random_, file = file.path(pathstr, 'baseline.Rdata'), ascii = TRUE,
       compress = TRUE, compression_level = 7)


}








