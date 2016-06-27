



library(EBMAforecast)
library(dplyr)
library(spduration)
library(magrittr)
library(xtable)
library(countrycode)
library(lubridate)


setwd("~/Work/jpr-forecasting-lessons")

# Create output directories for figures and tables
invisible(sapply(c("figures", "tables"), function(d) {
  if (!dir.exists(d)) dir.create(d)
  }))



# Estimate models, EBMA; forecast -----------------------------------------



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


#   Figure 6
#   ____________________

#   Figure 7
#   ____________________


# Appendix LASSO/RF comparison --------------------------------------------




# Appendix figures/tables -------------------------------------------------

#   Table A1
#   Tables A2 to A8: theme model estimates


