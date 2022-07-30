# Title     : InitialDiagnostics.R
# Objective : Initial Diagnostics
# Created by: greyhypotheses
# Created on: 27/07/2022



#' Initial Diagnostics: Inspecting Spatial Correlation
#'
#' @param data: a training data set
#' @param terms: the fixed effects
#'
InitialDiagnostics <- function (data, terms) {


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


  # illustrating diagnostics
  par(bty = 'n', fg = 'grey')
  spat.corr.diagnostic(formula = as.formula(object = paste0('positive ~', terms)),
                       units.m = ~examined,
                       nAGQ = 18,
                       data = st_drop_geometry(data),
                       coords = ~I(x / 1000) + I(y / 1000),
                       likelihood = 'Binomial',
                       lse.variogram = TRUE,
                       ID.coords = ID.coords)

}