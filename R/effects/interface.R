# Title     : interface.R
# Objective : Interface
# Created by: greyhypotheses
# Created on: 30/07/2022



# functions
source(file = 'R/data/StudyData.R')
source(file = 'R/effects/Estimates.R')


# a data set
ISO2 <- 'TG'
infection <- 'hk'
frame <- StudyData(ISO2 = ISO2, infection = infection)
frame$year <- factor(frame$year)


# Key Variables
variables <- list(identifier = 'identifier', tests = 'examined', positives = 'positive')
frame <- dplyr::rename(frame, 'identifier' = variables$identifier,
                      'positives' = variables$positives, 'tests' = variables$tests)


# Plausible fixed effects
string <- paste('improved_sewer + unimproved_sewer + piped_sewer + log(unpiped_sewer) ',
              '+ surface_sewer + log(p_density.k) + elevation.km ',
              '+ (1|identifier) + (1|year)', collapse = NULL)
model <- Estimates(data = frame, expression = string)
models <- list('1' = model)
summary(object = model)


string <- paste('piped_sewer + log(unpiped_sewer) + surface_sewer + log(p_density.k) + elevation.km ',
                '+ (1|identifier) + (1|year)', collapse = NULL)
model <- Estimates(data = frame, expression = string)
models <- append(models, list('2' = model))
summary(object = model)


string <- paste('piped_sewer + log(p_density.k) + elevation.km ',
                '+ (1|identifier) + (1|year)', collapse = NULL)
model <- Estimates(data = frame, expression = string)
models <- append(models, list('3' = model))
summary(object = model)


string <- paste('piped_sewer + I(piped_sewer^2) + surface_sewer + log(p_density.k) + elevation.km ',
                '+ (1|identifier) + (1|year)', collapse = NULL)
model <- Estimates(data = frame, expression = string)
models <- append(models, list('4' = model))
summary(object = model)


string <- paste('piped_sewer + I(piped_sewer^2) + log(p_density.k) + elevation.km ',
                '+ (1|identifier) + (1|year)', collapse = NULL)
model <- Estimates(data = frame, expression = string)
models <- append(models, list('5' = model))
summary(object = model)


string <- paste('piped_sewer + surface_sewer + log(p_density.k) + elevation.km',
                '+ (1|identifier) + (1|year)', collapse = NULL)
model <- Estimates(data = frame, expression = string)
models <- append(models, list('6' = model))
summary(object = model)


string <- paste('piped_sewer + I(piped_sewer^2) + elevation.km ',
                '+ (1|identifier) + (1|year)', collapse = NULL)
model <- Estimates(data = frame, expression = string)
models <- append(models, list('7' = model))
summary(object = model)


string <- paste('piped_sewer + surface_sewer + elevation.km ',
                '+ (1|identifier) + (1|year)', collapse = NULL)
model <- Estimates(data = frame, expression = string)
models <- append(models, list('8' = model))
summary(object = model)


string <- paste('piped_sewer + I(piped_sewer^2) + surface_sewer + elevation.km ',
                '+ (1|identifier) + (1|year)', collapse = NULL)
model <- Estimates(data = frame, expression = string)
models <- append(models, list('9' = model))
summary(object = model)



# All the models
cat('\n\nAll the models ...', sep = '\n')
print(anova(models[[1]], models[[2]], models[[8]], models[[4]], models[[6]], models[[5]],
            models[[7]], models[[3]], models[[9]]))

anova(models[[8]], models[[7]], models[[3]])
anova(models[[9]], models[[5]], models[[6]])
anova(models[[4]], models[[2]])
anova(models[[1]])


# Hence: Reject 1
anova(models[[3]], models[[6]], models[[2]])
anova(models[[7]], models[[6]], models[[2]])

