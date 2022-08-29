# Title     : SpatialCorrelation.R
# Objective : Spatial Correlation
# Created by: greyhypotheses
# Created on: 24/08/2022


SpatialCorrelation <- function (frame, variables, expressions, indices, group,
                                reduce = TRUE, step = 2, part = 1) {


  # Functions
  source(file = 'R/diagnostics/InitialDiagnostics.R')
  source(file = 'R/functions/GeographicObject.R')
  source(file = 'R/functions/SpatialExcerpt.R')


  # Geographic form
  instances <- GeographicObject(data = frame)


  # Pre-empting ill-conditioning due to spatially close points
  if (reduce) {
    instances <- SpatialExcerpt(data = instances, step = step, part = part)
  }


  for (i in indices) {
    InitialDiagnostics(data = instances, terms = expressions[[i]], variables = variables, kappa = 0.5)
    title(main = paste0(group, ': ', i))
  }


}