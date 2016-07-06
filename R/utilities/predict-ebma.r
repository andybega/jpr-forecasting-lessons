# EBMA ensemble prediction
#  
# Aggregate a set of predictions into an ensemble prediction using a fitted
# EBMA model. Recreates the functionality used to calculate test predictions
# during a EBMA calibration call.
# 
# ebma - A fitted EBMA object; only works with logit EBMA.
# inputs - Matrix or data frame in which each column is a series of 
#   probabilities.
# outcome - Vector of observed outcomes. Lenght must match rows in inputs.
# useModelParams - Should the constant/predictor parameters of EBMA be used, or 
#   only the bias reduction transformation? Both, by default. 

# Function for logit EBMA that will take input predictions and aggregate 
# them to an EBMA prediction.
predict.ebma <- function(ebma, inputs, outcome, useModelParams=TRUE) {
  if (!class(ebma)=="FDatFitLogit") stop("Only works with logit EBMA")
  
  inputs <- as.matrix(inputs)
  exp <- ebma@exp
  modelParams <- ebma@modelParams[, , 1]
  W <- ebma@modelWeights
  
  if (useModelParams == TRUE) {
    .adjPred <- .makeAdj(inputs, exp)
    inputsAdj <- array(NA, dim = c(nrow(inputs), ncol(inputs)))
    
    for (k in 1:ncol(inputs)) {
      inputsAdj[, k] <- affineTransform(.adjPred[, k], modelParams[, k])
      inputsAdj[, k] <- plogis(inputsAdj[, k])
    }
  }
  if (useModelParams == FALSE) {
    .adjPred <- .makeAdj(inputs)
    .adjPred[outcome == 0, ] <- (1 - plogis(.adjPred[outcome == 0, ]))
    .adjPred[outcome == 1, ] <- (plogis(.adjPred[outcome == 1, ]))
    inputsAdj <- .adjPred
  }
  # Weight inputs, with handling for missing predictions
  if (!any(is.na(inputsAdj))) {
    bma <- inputsAdj %*% W
  } else {
    # Weigh non-missing components, then adjust denominator to account for 
    # weights not used because of missing values.
    inputsAdjNA <- inputsAdj
    inputsAdjNA[is.na(inputsAdjNA)] <- 0
    bma <- inputsAdjNA %*% W
    denom <- (!is.na(inputsAdj) * 1) %*% W
    denom[denom==0] <- NA  # prevent /0 if entire row is NA
    bma <- bma/denom
  }
  as.vector(bma)
}

# Input adjustment
.makeAdj <- function(x, exp) {
  .adjPred <- qlogis(x)
  .negative <- .adjPred < 0
  .pos <- .adjPred > 1
  .adjPred <- ((1 + abs(.adjPred))^(1/exp)) - 1
  .miss <- is.na(.adjPred)
  .negative[.miss] <- FALSE
  .adjPred[.negative] <- .adjPred[.negative] * (-1)
  .adjPred[.miss] <- NA
  .adjPred
}

# Recreate glm() transformation
affineTransform <- function(x, b) {
  # Affine transform of y = b1 + x*b2
  y <- cbind(1, x) %*% b
  y
}