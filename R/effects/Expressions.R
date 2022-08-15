# Title     : Expressions.R
# Objective : Expressions
# Created by: greyhypotheses
# Created on: 12/08/2022


Expressions <- function () {

  initial <- list('1' = paste('improved_sewer + unimproved_sewer + piped_sewer + log(unpiped_sewer) ',
                        '+ surface_sewer + log(p_density.k) + elevation.km + (1|year)', collapse = NULL),
            '2' = 'piped_sewer + log(unpiped_sewer) + surface_sewer + log(p_density.k) + elevation.km + (1|year)',
            '3' = 'piped_sewer + log(p_density.k) + elevation.km + (1|year)',
            '4' = 'piped_sewer + I(piped_sewer^2) + surface_sewer + log(p_density.k) + elevation.km + (1|year)',
            '5' = 'piped_sewer + I(piped_sewer^2) + log(p_density.k) + elevation.km + (1|year)',
            '6' = 'piped_sewer + surface_sewer + log(p_density.k) + elevation.km + (1|year)',
            '7' = 'piped_sewer + I(piped_sewer^2) + elevation.km + (1|year)',
            '8' = 'piped_sewer + surface_sewer + elevation.km + (1|year)',
            '9' = 'piped_sewer + I(piped_sewer^2) + surface_sewer + elevation.km + (1|year)')

  simple <- list('1' = paste('improved_sewer + unimproved_sewer + piped_sewer + log(unpiped_sewer) ',
                              '+ surface_sewer + log(p_density.k) + elevation.km', collapse = NULL),
                  '2' = 'piped_sewer + log(unpiped_sewer) + surface_sewer + log(p_density.k) + elevation.km',
                  '3' = 'piped_sewer + log(p_density.k) + elevation.km',
                  '4' = 'piped_sewer + I(piped_sewer^2) + surface_sewer + log(p_density.k) + elevation.km',
                  '5' = 'piped_sewer + I(piped_sewer^2) + log(p_density.k) + elevation.km',
                  '6' = 'piped_sewer + surface_sewer + log(p_density.k) + elevation.km',
                  '7' = 'piped_sewer + I(piped_sewer^2) + elevation.km',
                  '8' = 'piped_sewer + surface_sewer + elevation.km',
                  '9' = 'piped_sewer + I(piped_sewer^2) + surface_sewer + elevation.km')

  excerpt <- list('1' = 'piped_sewer + log(unpiped_sewer) + surface_sewer + log(p_density.k) + elevation.km',
            '2' = 'piped_sewer + log(p_density.k) + elevation.km',
            '3' = 'piped_sewer + surface_sewer + log(p_density.k) + elevation.km',
            '4' = 'piped_sewer + I(piped_sewer^2) + elevation.km ',
            '5' = 'piped_sewer + surface_sewer + elevation.km',
            '6' = 'piped_sewer + I(piped_sewer^2) + surface_sewer + elevation.km')

  return(list('1' = initial, '2' = simple, '3' = excerpt))

}