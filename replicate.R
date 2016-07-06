#
#   Replication script for:
#   
#   Ward, Michael D. and Andreas Beger, 2016, 
#
#
#


library("dplyr")
library("EBMAforecast")
library("spduration")
library("magrittr")
library("xtable")
library("countrycode")
library("lubridate")
library("cowplot")
library("ROCR")

# optional packages for Appendix replication
library("doMC")
library("glmnet")
library("randomForest")
library("separationplot")


setwd("~/Work/jpr-forecasting-lessons")

# Create output directories for figures and tables
invisible(sapply(c("figures", "tables"), function(d) {
  if (!dir.exists(d)) dir.create(d)
  }))


# Estimate models, EBMA; forecast -----------------------------------------
#
#   

source("R/estimate-and-forecast.R")


# Main figures/tables -----------------------------------------------------

#   Table II: EBMA and theme fit
#   _____________________________

source("R/table2.R")


#   Table III: Forecasts
#   ____________________

source("R/table3.R")


#   Figure 1a and b: ILC distributions
#   ____________________

source("R/ilc-summary.R")

#   Figure 3: EBMA and theme prediction violin plot and correlation matrix
#   Figure 5: EBMA weights vs model fit
#   Figure 6: EBMA and theme model barcode plots
#   ____________________

source("R/theme-summary.R")


#   Figure 7: Case-specific plots
#   ____________________

source("R/case-details.R")


# Appendix figures/tables -------------------------------------------------

#   Table A1: comparative model fit
#   Figure 1: ROC/PR plots
#   Figure 2: separation plots

source("R/ebma-alternatives.R")

#   Table A2: List of ILCs
#   Tables A3 to A9: theme model estimates

source("R/appdx-tables.R")

#   Table A10: comparison of regular, monthly test to 6-month test forecasts

source("R/6month-vs-test.R")



