# Title     : interface.R
# Objective : Interface
# Created by: greyhypotheses
# Created on: 30/07/2022



# functions
source(file = 'R/data/StudyData.R')
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
X <- list('1' = paste('improved_sewer + unimproved_sewer + piped_sewer + log(unpiped_sewer) ',
                      '+ surface_sewer + log(p_density.k) + elevation.km', collapse = NULL),
          '2' = 'piped_sewer + log(unpiped_sewer) + surface_sewer + log(p_density.k) + elevation.km',
          '3' = 'piped_sewer + log(p_density.k) + elevation.km',
          '4' = 'piped_sewer + I(piped_sewer^2) + surface_sewer + log(p_density.k) + elevation.km',
          '5' = 'piped_sewer + I(piped_sewer^2) + log(p_density.k) + elevation.km',
          '6' = 'piped_sewer + surface_sewer + log(p_density.k) + elevation.km',
          '7' = 'piped_sewer + I(piped_sewer^2) + elevation.km',
          '8' = 'piped_sewer + surface_sewer + elevation.km',
          '9' = 'piped_sewer + I(piped_sewer^2) + surface_sewer + elevation.km')


# Effects
baseline <- EffectsBaseline(frame = frame, X = X)

instances <- frame[frame$year == 2015, ]
row.names(instances) <- NULL
later <- EffectsSegment(frame = instances, X = X)

instances <- frame[frame$year == 2009, ]
row.names(instances) <- NULL
earlier <- EffectsSegment(frame = instances, X = X)


# Inspect
dplyr::bind_rows(baseline$LSE)
dplyr::bind_rows(later$LSE)
dplyr::bind_rows(earlier$LSE)

baseline_ <- baseline$models
later_ <- later$models
earlier_ <- earlier$models

anova(baseline_[[8]], baseline_[[7]], baseline_[[3]], baseline_[[6]], baseline_[[2]])
anova(later_[[8]], later_[[7]], later_[[3]], later_[[6]], later_[[2]])
anova(earlier_[[8]], earlier_[[7]], earlier_[[3]], earlier_[[6]], earlier_[[2]])


# In focus: 3, 7
for (i in c(3, 7)) {
  summary(later_[[i]])
  summary(earlier_[[i]])
}

for (i in c(2, 6, 8)) {
  print(summary(later_[[i]]))
  print(summary(earlier_[[i]]))
}
