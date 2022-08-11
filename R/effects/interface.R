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
segment <- EffectsSegment(frame = instances, X = X)



# Inspect
dplyr::bind_rows(baseline$LSE)
dplyr::bind_rows(segment$LSE)

baseline_ <- baseline$models
segment_ <- segment$models
anova(baseline_[[8]], baseline_[[7]], baseline_[[3]], baseline_[[6]], baseline_[[2]])
anova(segment_[[8]], segment_[[7]], segment_[[3]], segment_[[6]], segment_[[2]])
