# Title     : StandardisedResidual.R
# Objective : Standardised residual error
# Created by: greyhypotheses
# Created on: 31/07/2022


StandardisedResidual <- function (design, observed, estimated) {

  # differences/errors
  differences <- observed - estimated

  # Leverage
  L <- design %*% qr.solve(a = (t(design) %*% design)) %*% t(design)
  leverages <- as.numeric(diag(L))
  rm(L)

  # Residual Standard Error (Internal Standard/Student Residual)
  RSE <- sqrt( sum('^'(differences, 2)) / (nrow(design) - ncol(design)) )

  # Standardised/Studentised Residuals
  residues <- differences / (RSE * sqrt(1 - leverages))
  
  
  return(residues)


}