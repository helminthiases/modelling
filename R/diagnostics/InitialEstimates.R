# Title     : InitialEstimates.R
# Objective : Initial Estimates
# Created by: greyhypotheses
# Created on: 29/07/2022



#' Initial Diagnostics: Inspecting Spatial Correlation
#'
#' @param data: a training data set
#' @param terms: the fixed effects
#' @param variables: A list that identifies the names of the fields
#'                      list(identifier = ..., tests = ..., positives = ...)
#'                   in <data>.
#' @param kappa: the smoothness parameter of the Matérn function
#'
InitialEstimates <- function (data, terms, variables, kappa = 0.5) {

  data <- dplyr::rename(data, 'identifier' = variables$identifier,
                        'positives' = variables$positives, 'tests' = variables$tests)


  # Addressing spat.corr.diagnostic's peculiar identification code rules
  indices <- data[, 'identifier', drop = FALSE] %>% st_drop_geometry()
  indices <- indices %>%
    dplyr::group_by(identifier) %>%
    summarise(N = n())
  indices$code <- seq(from = 1, to = base::nrow(indices))

  data <- base::merge(x = data, y = indices, by = 'identifier', all.x = TRUE, sort = FALSE)


  # Hence
  if (length(unique(data$code)) == dim(data)[1]) {
    ID.coords <- NULL
  } else {
    ID.coords <- as.vector(data$code)
  }


  # Initial scale & variance parameter values
  par(bty = 'n', fg = 'grey')
  parameters <- spat.corr.diagnostic(formula = as.formula(object = paste0('positives ~', terms)),
                                     units.m = ~tests,
                                     nAGQ = 18,
                                     data = st_drop_geometry(data),
                                     coords = ~I(x / 1000) + I(y / 1000),
                                     likelihood = 'Binomial',
                                     lse.variogram = TRUE,
                                     kappa = kappa,
                                     ID.coords = ID.coords)


  # GLMM
  model <- glmer(formula = as.formula(object = paste0('cbind(positives, tests - positives) ~ ', terms, ' + (1|identifier)')),
                 family = binomial(link = 'logit'),
                 control = glmerControl(optimizer = c('nlminbwrap', 'nlminbwrap')),
                 data = st_drop_geometry(data))
  estimates <- summary(object = model)


  # All parameters: c(beta, sigmasqr, phi, tausqr)
  settings <- c(estimates$coefficients[, 'Estimate'], parameters$lse.variogram['sigma^2'],
                parameters$lse.variogram['phi'], parameters$lse.variogram['tau^2'])


  return(list(model = model, settings = settings))


}
