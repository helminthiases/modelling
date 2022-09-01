# Title     : QuantileLine.R
# Objective : Quantile Line
# Created by: greyhypotheses
# Created on: 10/08/2022


QuantileLine <- function (model) {

  effects <- lme4::ranef(object = model)

  intercepts <- data.frame(intercept = effects$identifier[, '(Intercept)'])

  ggplot(data = intercepts, aes(sample = intercept)) +
    stat_qq() +
    stat_qq_line() +
    theme_minimal() +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_line(size = 0.05),
          axis.title.x = element_text(size = 11), axis.title.y = element_text(size = 11),
          axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10)) +
    xlab(label = '\nTheoretical Quantiles\n') +
    ylab(label = '\nSample Quantiles\n')

}