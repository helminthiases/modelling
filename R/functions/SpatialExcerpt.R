# Title     : SpatialExcerpt.R
# Objective : Spatial excerpt
# Created by: greyhypotheses
# Created on: 30/07/2022


SpatialExcerpt <- function (data, step) {

  # arrange spatially
  T <- data %>%
    dplyr::arrange(x, y)


  # excerpt: select an observation at every <step> point
  T$index <- 1:nrow(x = T)
  excerpt <- T[(T$index %% step) == 0, ]
  

  # clean-up
  excerpt$index <- NULL


  # preview
  map <- tmap::tm_shape(data) +
    tm_layout(main.title = 'Distribution of Points', frame = FALSE) +
    tm_dots()
  print(map)

  map <- tmap::tm_shape(excerpt) +
    tm_layout(main.title = 'Distribution of Excerpt', frame = FALSE) +
    tm_dots()
  print(map)



  # spreads
  spreads <- rbind(data.frame(prevalence = data$prevalence, segment = 'all points'),
                   data.frame(prevalence = excerpt$prevalence, segment = 'excerpt'))
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
  return(list(excerpt = excerpt))

}