# Title     : interface.R
# Objective : Interface
# Created by: greyhypotheses
# Created on: 30/07/2022



# functions
source(file = 'R/data/StudyData.R')
source(file = 'R/effects/Expressions.R')
source(file = 'R/effects/EffectsBaseline.R')
source(file = 'R/effects/EffectsSegment.R')
source(file = 'R/diagnostics/InitialDiagnostics.R')
source(file = 'R/functions/GeographicObject.R')
source(file = 'R/functions/SpatialExcerpt.R')


# a data set
ISO2 <- 'TG'
infection <- 'hk'
frame <- StudyData(ISO2 = ISO2, infection = infection)
frame <- GeographicObject(data = frame)


# Setting-up
variables <- list(identifier = 'identifier', tests = 'examined', positives = 'positive')


# Effects
expressions_ <- Expressions()


# Options
baseline <- EffectsBaseline(frame = frame, expressions = expressions_[[1]], variables = variables)

instances <- frame[frame$year == 2015, ]
row.names(instances) <- NULL
later <- EffectsSegment(frame = instances, expressions = expressions_[[2]], variables = variables)

instances <- frame[frame$year == 2009, ]
row.names(instances) <- NULL
earlier <- EffectsSegment(frame = instances, expressions = expressions_[[2]], variables = variables)


# Inspect
anova(baseline[[1]], baseline[[2]], baseline[[3]], baseline[[4]], baseline[[5]],
      baseline[[6]], baseline[[7]], baseline[[8]], baseline[[9]])
anova(earlier[[1]], earlier[[2]], earlier[[3]], earlier[[4]], earlier[[5]],
      earlier[[6]], earlier[[7]], earlier[[8]], earlier[[9]])
anova(later[[1]], later[[2]], later[[3]], later[[4]], later[[5]],
      later[[6]], later[[7]], later[[8]], later[[9]])

arc <- later
anova(arc[[7]], arc[[8]], arc[[3]], arc[[6]], arc[[2]])
indices <- c(7, 8, 3, 6, 2)


# Summaries
for (i in indices) {
  InitialDiagnostics(data = frame, terms = expressions_[[1]][[i]], variables = variables, kappa = 0.5)
  title(main = paste0('baseline: ', i))
}

instances <- frame[frame$year == 2015, ]
row.names(instances) <- NULL
instances <- SpatialExcerpt(data = instances, step = 2, part = 2)
for (i in indices) {
  InitialDiagnostics(data = instances, terms = expressions_[[2]][[i]], variables = variables, kappa = 0.5)
  title(main = paste0('later: ', i))
}

instances <- frame[frame$year == 2009, ]
row.names(instances) <- NULL
instances <- SpatialExcerpt(data = instances, step = 2, part = 2)
for (i in indices) {
  InitialDiagnostics(data = instances, terms = expressions_[[2]][[i]], variables = variables, kappa = 0.5)
  title(main = paste0('earlier: ', i))
}

