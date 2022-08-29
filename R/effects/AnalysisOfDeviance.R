# Title     : AnalysisOfDeviance.R
# Objective : Analysis of deviance
# Created by: greyhypotheses
# Created on: 24/08/2022


AnalysisOfDeviance <- function (model, indices) {

  string <- paste0('model[[', indices, ']]', collapse = ', ')
  string <- paste0('anova(', string, ')')
  print(eval(parse(text = string)))

}