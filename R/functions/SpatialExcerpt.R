# Title     : SpatialExcerpt.R
# Objective : Spatial excerpt
# Created by: greyhypotheses
# Created on: 30/07/2022



#' Spatial Excerpt
#'
#' @param data: The modelling data set
#' @param step: Select every <step> record
#'
SpatialExcerpt <- function (data, step, part = 1) {

  # arrange spatially
  T <- data %>%
    dplyr::arrange(x, y)

  limit <- step * floor(nrow(T) / step)
  indices <- seq_len(limit)
  sections <- matrix(data = indices, ncol = step, byrow = TRUE)
  focus <- sections[, part]


  # excerpt: select an observation at every <step> point
  T$index <- 1:nrow(x = T)
  excerpt <- T[(T$index %in% focus), ]
  

  # clean-up
  excerpt$index <- NULL


  # spreads
  spreads <- rbind(data.frame(prevalence = data$prevalence, segment = 'all points'),
                   data.frame(prevalence = excerpt$prevalence, segment = 'excerpt'))
  ggplot(data = spreads, mapping = aes(x= prevalence, fill = segment)) +
    geom_density(alpha = 0.35, colour = 'white') +
    theme_minimal() +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_line(size = 0.05),
          axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12),
          axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10)) +
    ylab(label = '\ndensity\n') +
    xlab(label = '\nprevalence\n')


  # hence
  return(excerpt)

}