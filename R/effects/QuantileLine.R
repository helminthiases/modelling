# Title     : QuantileLine.R
# Objective : Quantile Line
# Created by: greyhypotheses
# Created on: 10/08/2022


QuantileLine <- function (model) {

  effects <- ranef(object = model)

  intercepts <- data.frame(intercept = effects$identifier[, '(Intercept)'])

  ggplot(data = intercepts, aes(sample = intercept)) +
    stat_qq() +
    stat_qq_line() +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5),
          panel.spacing = unit(x = 3, units = 'line'),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(size = 0.05)) +
    xlab(label = '\nTheoretical Quantiles\n') +
    ylab(label = '\nSample Quantiles\n') +
    ggtitle(label = 'Sites')

}