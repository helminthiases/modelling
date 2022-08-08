# Title     : DataSplitFractional.R
# Objective : Data splitting
# Created by: greyhypotheses
# Created on: 05/08/2022



#' Data Split Fractional
#'
#' @param instances: The modelling data set
#' @param fraction: The training data fraction
#'
DataSplitFractional <- function (instances, fraction) {


  source(file = 'R/functions/SpatialExcerpt.R')


  # ... splitting via this quotient ensures similar training & testing prevalence
  # distributions
  instances$quotient <- instances$prevalence

  # ... splitting: internal validation
  segments <- healthcareai::split_train_test(d = instances, outcome = quotient,
                                             percent_train = fraction, seed = 5)
  training <- segments$train
  testing <- segments$test



  # the distributions of the splits: maps
  # outer.margin -> c(bottom, left, top, right)
  distributions <- list()
  distributions$training <- tmap::tm_shape(training) +
    tm_layout(main.title = '\nTraining Set\n', frame = FALSE,
              outer.margins = c(0.1, 0.1, 0.1, 0.1), main.title.size = 0.95,
              main.title.color = 'olivedrab', main.title.fontface = 'bold') +
    tm_dots(col = 'olivedrab', alpha = 0.65)
  distributions$testing <- tmap::tm_shape(testing) +
    tm_layout(main.title = '\nTesting Set\n', frame = FALSE,
              outer.margins = c(0.1, 0.1, 0.1, 0.1), main.title.size = 0.95,
              main.title.color = 'olivedrab', main.title.fontface = 'bold') +
    tm_dots(col = 'olivedrab', alpha = 0.65)
  print(tmap::tmap_arrange(distributions$training, distributions$testing))



  # the distributions of the splits: density graphs
  spreads <- rbind(data.frame(prevalence = training$prevalence, segment = 'training'),
                   data.frame(prevalence = testing$prevalence, segment = 'testing'))

  graph <- ggplot(data = spreads, mapping = aes(x= prevalence, fill = segment)) +
    geom_density(alpha = 0.35, colour = 'white') +
    theme_minimal() +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_line(size = 0.05),
          axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12),
          axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10)) +
    ylab(label = '\ndensity\n') +
    xlab(label = '\nprevalence\n')
  print(graph)


  # ... hence
  training$quotient <- NULL
  testing$quotient <- NULL
  return(list(training = training, testing = testing))

}