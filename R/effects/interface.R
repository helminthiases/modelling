# Title     : interface.R
# Objective : Interface
# Created by: greyhypotheses
# Created on: 30/07/2022



# functions
source(file = 'R/data/StudyData.R')
source(file = 'R/effects/Expressions.R')
source(file = 'R/effects/Effects.R')
source(file = 'R/diagnostics/InitialDiagnostics.R')
source(file = 'R/functions/GeographicObject.R')
source(file = 'R/functions/SpatialExcerpt.R')
source(file = 'R/effects/SpatialCorrelation.R')
source(file = 'R/effects/AnalysisOfDeviance.R')



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
baseline <- Effects(frame = frame, expressions = expressions_[[1]], variables = variables)
later <- Effects(frame = frame[frame$year == 2015, ], expressions = expressions_[[2]], variables = variables)
earlier <- Effects(frame = frame[frame$year == 2009, ], expressions = expressions_[[2]], variables = variables)


# Inspect:  Expressions 7 & 3 lead to <earlier> & <later> models with sigificant coefficients.
AnalysisOfDeviance(model = baseline, indices = seq_len(length.out = length(x = baseline)))
AnalysisOfDeviance(model = later, indices = seq_len(length.out = length(x = later)))
AnalysisOfDeviance(model = earlier, indices = seq_len(length.out = length(x = earlier)))

AnalysisOfDeviance(model = baseline, indices = c(7, 8, 3, 6, 2))
AnalysisOfDeviance(model = later, indices = c(7, 8, 3, 6, 2))
AnalysisOfDeviance(model = earlier, indices = c(7, 8, 3, 6, 2))


# Spatial Features
SpatialCorrelation(frame = frame,
                   variables = variables,
                   expressions = unlist(expressions_[[1]], use.names = FALSE),
                   indices = c(1,7,3),
                   group = 'baseline')

SpatialCorrelation(frame = frame[frame$year == 2015, ],
                   variables = variables,
                   expressions = unlist(expressions_[[2]], use.names = FALSE),
                   indices = seq_len(length.out = length(later)),
                   group = 'later', reduce = TRUE, step = 2, part = 2)

SpatialCorrelation(frame = frame[frame$year == 2009, ],
                   variables = variables,
                   expressions = unlist(expressions_[[2]], use.names = FALSE),
                   indices = c(7,3),
                   group = 'earlier', reduce = FALSE)

