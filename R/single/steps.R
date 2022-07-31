# Title     : steps.R
# Objective : Modelling steps
# Created by: greyhypotheses
# Created on: 31/07/2022



# Architecture
cat(paste0('positive ~ ', {{terms}}))



# Model
objects <- BinomialLogisticBayes(data = excerpt, variables = variables, terms = terms)
model <- objects$model



# Valuations (vis-à-vis training points) & Predictions (vis-à-vis testing points)
T <- BinomialLogisticBayesEVL(model = model, excerpt = excerpt, testing = testing)
valuations <- T$valuations
predictions <- T$predictions



# Is there still evidence of residual spatial correlation?
# The standardised residuals of the differences/errors/residuals w.r.t. the training points
residues <- StandardisedResidual(design = model$D, observed = excerpt$prevalence,
                                 estimated = valuations$prevalence$predictions)

# The empirical variogram measures & graph w.r.t. the standardised residual
points <- EmpiricalVariogram(data = data.frame(residue = residues, x = excerpt$x, y = excerpt$y))

# Graph
map <- ggplot(data = points[points$distance < 500, ], mapping = aes(x = distance, y = estimate)) +
  scale_y_continuous(breaks = c(0, 0.50, 1.00, 1.5), limits = c(0, 1.8)) +
  geom_line() +
  geom_ribbon(mapping = aes(ymin = `estimate.lower`, ymax = `estimate.upper`), alpha = 0.3, linetype = 0) +
  geom_point(alpha = 0.65, size = 1) +
  geom_vline(xintercept = 0, alpha = 0.35, size = 0.25) +
  theme_minimal() +
  theme(panel.spacing = unit(x = 3, units = 'lines'),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(size = 0.05),
        plot.title = element_text(hjust = 0.25, size = 13, face = 'bold'),
        plot.caption = element_text(hjust = 0, size = 11, colour = 'darkgrey'),
        axis.title.x = element_text(size = 12, face = 'bold'), axis.text.x = element_text(size = 10),
        axis.title.y = element_text(size = 12, face = 'bold'), axis.text.y = element_text(size = 10)) +
  xlab(label = '\ndistance\n') +
  ylab(label = '\nvariogram\n')
print(map)



# Illustrating Accuracy: Diagonals
trainees <- data.frame(prevalence = excerpt$prevalence, prediction = valuations$prevalence$predictions)
tests <- data.frame(prevalence = testing$prevalence, prediction = predictions$prevalence$predictions)

diagonals <- rbind(cbind(trainees, segment = 'training'), cbind(tests, segment = 'testing'))

ggplot(data = diagonals, mapping = aes(x = prediction, y = prevalence)) +
  geom_segment(mapping = aes(x = 0, y = 0, xend = 1, yend = 1), alpha = 0.2, colour = 'lightgrey', size = 0.01) +
  geom_point(alpha = 0.35) +
  facet_wrap(~segment) +
  theme_minimal() +
  theme(panel.spacing = unit(x = 3, units = 'lines'),
        panel.grid.minor = element_blank(), panel.grid.major = element_line(size = 0.05),
        strip.text.x = element_text(size = 10),
        axis.text.x = element_text(size = 9), axis.text.y = element_text(size = 9),
        axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12)) +
  xlab(label = '\nprevalence: prediction\n') +
  ylab(label = '\nprevalence: original\n') +
  xlim(0, 1) +
  ylim(0, 1)



# Bias, Error, Noise

