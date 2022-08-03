# Title     : SpatialSplitting.R
# Objective : Spatial Splitting
# Created by: greyhypotheses
# Created on: 01/08/2022



SpatialSplitting <- function (instances, step) {

  # arrange spatially
  T <- instances %>%
    dplyr::arrange(x, y)


  # training set: select an observation at every <step> point
  T$index <- 1:nrow(x = T)
  training <- T[(T$index %% step) == 0, ]


  # remove the training points in <T>
  T <- T[!(T$index %in% training$index), ]


  # Either
  set.seed(seed = 9, sample.kind = 'default')
  indices <- sample(x = T$index, size = floor(dim(instances)[1]/(2*step)), replace = FALSE)
  testing <- T[T$index %in% indices, ]


  # Or
  # testing set: select an observation at every <(2*step + 1)> point
  # testing <- T[(T$index %% (2*step + 1)) == 0, ]


  # clean-up
  training$index <- NULL
  testing$index <- NULL


  # the distributions of the splits
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


  # Distributions
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


  # hence
  return(list(training = training, testing = testing))

}
