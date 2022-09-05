# Title     : EvaluationGraphs.R
# Objective : Evaluation Graphs
# Created by: greyhypotheses
# Created on: 31/07/2022


#' Empirical Variogram Graph
#'
#' @param points: Empirical variogram points
#' @param limit: Empirical variogram distance limits
#'
SpatialEvaluationGraphs <- function (points, limit) {

  # Graph
  map <- ggplot(data = points[points$distance < limit, ], mapping = aes(x = distance, y = estimate)) +
    scale_y_continuous(breaks = c(0.70, 0.9, 1.1), limits = c(0.6, 1.25)) +
    scale_x_continuous(breaks = c(0, 100, 200)) +
    geom_line() +
    geom_ribbon(mapping = aes(ymin = `estimate.lower`, ymax = `estimate.upper`), alpha = 0.3, linetype = 0) +
    geom_point(alpha = 0.65, size = 1) +
    geom_vline(xintercept = 0, alpha = 0.35, size = 0.25) +
    theme_minimal() +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_line(size = 0.05),
          plot.title = element_text(hjust = 0.25, size = 13, face = 'bold'),
          plot.caption = element_text(hjust = 0, size = 11, colour = 'darkgrey'),
          axis.title.x = element_text(size = 12, face = 'bold'), axis.text.x = element_text(size = 10),
          axis.title.y = element_text(size = 12, face = 'bold'), axis.text.y = element_text(size = 10)) +
    xlab(label = '\ndistance\n') +
    ylab(label = '\nvariogram\n')
  
  return(map)
  
}


#' Evaluation Diagonals: Training & Testing
#'
#' @description original prevalence vs. estimated prevalence
#'
#' @param training: The training data evaluation measures
#' @param testing: The testing data evaluation measures
DoubleDiagonalEvaluationGraphs <- function (training_, testing_) {

  estimates <- rbind(
    data.frame(prevalence = training_$prevalence, prediction = training_$prediction, segment = 'training'),
    data.frame(prevalence = testing_$prevalence, prediction = testing_$prediction, segment = 'testing')
  )

  map <- ggplot(data = estimates, mapping = aes(x = prediction, y = prevalence)) +
    geom_segment(mapping = aes(x = 0, y = 0, xend = 1, yend = 1), linetype = 'dotdash', alpha = 0.05, colour = 'lightgrey', size = 0.05) +
    geom_point(alpha = 0.35) +
    scale_y_continuous(breaks = c(0, 0.5, 1.0), limits = c(0, 1.0)) +
    scale_x_continuous(breaks = c(0, 0.5, 1.0), limits = c(0, 1.0)) +
    facet_wrap(~segment, labeller = ggplot2::as_labeller(c('training' = 'training\n data', 'testing' = 'testing\n data' ))) +
    theme_minimal() +
    theme(panel.spacing = unit(x = 3, units = 'lines'),
          panel.grid.minor = element_blank(), 
          panel.grid.major = element_line(size = 0.05),
          strip.text.x = element_text(size = 10),
          axis.text.x = element_text(size = 9), axis.text.y = element_text(size = 9),
          axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12)) +
    xlab(label = '\nprevalence: estimate\n') +
    ylab(label = '\nprevalence: original\n')
  
  return(map)
  
}


#' Evaluation Diagonal: Single data set
#'
#' @param data_: A list that has the set-up
#'                  list(prevalence = , prediction = )
#'               wherein <prediction> represents the prevalence predictions of a model.
#'
SingleDiagonalEvaluationGraphs <- function (data_) {

  estimates <- data.frame(prevalence = data_$prevalence, prediction = data_$prediction)

  map <- ggplot(data = estimates, mapping = aes(x = prediction, y = prevalence)) +
    geom_segment(mapping = aes(x = 0, y = 0, xend = 1, yend = 1), alpha = 0.2, colour = 'lightgrey', size = 0.01) +
    geom_point(alpha = 0.35) +
    theme_minimal() +
    theme(panel.spacing = unit(x = 3, units = 'lines'),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(size = 0.05),
          strip.text.x = element_text(size = 10),
          axis.text.x = element_text(size = 9), axis.text.y = element_text(size = 9),
          axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12)) +
    xlab(label = '\nprevalence: prediction\n') +
    ylab(label = '\nprevalence: original\n') +
    xlim(0, 1) +
    ylim(0, 1)

  return(map)

}