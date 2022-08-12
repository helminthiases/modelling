# Title     : interface.R
# Objective : Interface
# Created by: greyhypotheses
# Created on: 30/07/2022



# functions
source(file = 'R/data/StudyData.R')
source(file = 'R/effects/Expressions.R')
source(file = 'R/effects/EffectsBaseline.R')
source(file = 'R/effects/EffectsSegment.R')


# a data set
ISO2 <- 'TG'
infection <- 'hk'
frame <- StudyData(ISO2 = ISO2, infection = infection)
frame$year <- factor(frame$year)


# Setting-up
variables <- list(identifier = 'identifier', tests = 'examined', positives = 'positive')
frame <- dplyr::rename(frame, 'identifier' = variables$identifier,
                      'positives' = variables$positives, 'tests' = variables$tests)


# Effects
expressions <- Expressions()

baseline <- EffectsBaseline(frame = frame, expressions = expressions[[2]])

instances <- frame[frame$year == 2015, ]
row.names(instances) <- NULL
later <- EffectsSegment(frame = instances, expressions = expressions[[2]])

instances <- frame[frame$year == 2009, ]
row.names(instances) <- NULL
earlier <- EffectsSegment(frame = instances, expressions = expressions[[2]])


# Inspect
anova(baseline[[5]], baseline[[4]], baseline[[2]], baseline[[3]], baseline[[6]], baseline[[1]])
anova(later[[4]], later[[5]], later[[2]], later[[3]], later[[6]], later[[1]])
anova(earlier[[5]], earlier[[4]], earlier[[2]], earlier[[3]], earlier[[6]], earlier[[1]])


# Summaries
for (i in seq_len(length(baseline))) {
  print(summary(baseline[[i]]))
  print(summary(later[[i]]))
  print(summary(earlier[[i]]))
  cat('\n\n\n\n')
}
