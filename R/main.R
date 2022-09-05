# Title     : main.R
# Objective : Play ground
# Created by: greyhypotheses
# Created on: 29/07/2022


# importing the data files
source(file = 'R/data/import.R')

# conducting single country exploratory analysis graphs
source(file = 'R/reference/interface.R')

# preliminary investigation via generalised linear mixed modelling
source(file = 'R/effects/architectures/interface.R')

# case: piped_sewer + I(piped_sewer^2) + elevation.km
source(file = 'R/models/nugget/A/interface.R')

# case: piped_sewer + log(p_density.k) + elevation.km
source(file = 'R/models/nugget/B/interface.R')

# export this project's results to <manuscript>
source(file = 'R/export.R')
