#
#   Forecasts for Irregular Leadership Changes in Q1/Q2 2015
#   Andreas Beger & Michael D. Ward
#   10 April 2015
#
#   This file will estimate the input models and ensemble and use them
#   to calculate the test and live forecasts. At various points along the way, 
#   it will call on secondary files that examine particular aspects in more 
#   detail. These are optional for generating the forecasts themselves, and are 
#   separated in order to simplify and speed up the core content. 


rm(list=ls())

# Switch directory
user <- Sys.info()[["user"]]
node <- Sys.info()[["nodename"]]

if (user=="andybega" & node=="andybega-mbp.local") {
  setwd("~/jpr-ilc-2015")
} else if (user=="mw160") {
  setwd("~/git/PITF/ilc-2015-jpr")
} else if (user=="andybega") {
  setwd("~/jpr-ilc-2015")
}
rm(user, node)

#scp -r ~/Work/PITF/ilc-2015-jpr/R andybega@152.3.32.85:/Users/andybega/Work/PITF/ilc-2015-jpr
#scp ~/Work/PITF/ilc-2015-jpr/runme.r andybega@152.3.32.85:/Users/andybega/Work/PITF/ilc-2015-jpr

# Load required libraries
library(EBMAforecast)
library(dplyr)
library(spduration)
library(magrittr)
library(xtable)
library(countrycode)
library(lubridate)


# Helper functin to fill in forecast period data
source("R/utilities/fcast-funcs.r")
source("R/utilities/ebma-fcast.r")

# For tables
source("R/utilities/binary-fit.r")
source("R/utilities/prettyc.r")

# Code to eval theme model fit; not needed
source("R/utilities/theme-models.r")


# load data
load(file="data/ilc-data-v4.rda")


# Descriptive ILC stats ---------------------------------------------------

#source("R/ilc-summary.r", echo=TRUE)


# Theme models ------------------------------------------------------
#
#   Reduced models, some 90's data

# Dates:
# train = data used to estimate models
# calib + test = test data for theme model estimates
# calib = calibration period for ensemble
# test  = test period for ensemble
train_start <- as.Date("1991-03-01")
calib_start <- as.Date("2010-01-01")
test_start  <- as.Date("2012-05-01")
data_end    <- as.Date("2014-12-31")

ilc_data$polity_l1 <- with(ilc_data, DEMOC.l1 - AUTOC.l1)

# Recreate Global Instability regime measure
with(ilc_data, table(EXREC.l1, PARCOMP.l1))
attach(ilc_data) 
FullAut <- PARCOMP.l1 %in% c(1, 2) & EXREC.l1 %in% c(1:5)
FullAut <- FullAut | PARCOMP.l1==0 & EXREC.l1==0  # ok, treat 0 exreg as full aut as well; seem not covered in Polity coding scheme
PartAut <- (PARCOMP.l1 %in% c(1, 2) & EXREC.l1 > 5) | (PARCOMP.l1 > 1 & EXREC.l1 %in% c(1:5))
PartDemFact <- EXREC.l1 %in% c(6:8) & PARCOMP.l1==3
FullDem <- PARCOMP.l1==5 & EXREC.l1==8
PartDem <- PARCOMP.l1 > 1 & EXREC.l1 > 5 & !FullDem & !PartDemFact
detach(ilc_data)

ilc_data$gold_regime_l1 <- 
  ifelse(FullAut, "FullAut", 
         ifelse(PartAut, "PartAut",
                ifelse(PartDem, "PartDem",
                       ifelse(PartDemFact, "PartDemFact",
                              ifelse(FullDem, "FullDem", "Other")))))
table(ilc_data$gold_regime_l1)
# Other category is for undefined combination, including transitional regimes

# Normalize infant mortality by year
# It's not quite clear what this means. If you subtract and divide by annual
# average infant mortality, you will get negative values one cannot log.
# And logging first, then subtracting is very strange. 
ilc_data$year <- year(ilc_data$date)
mn_inf_mort <- ilc_data %>% 
  dplyr::group_by(year) %>%
  dplyr::summarize(SH.DYN.MORT.l1ml10 = mean(log10(SH.DYN.MORT.l1), na.rm=TRUE),
                   SH.DYN.MORT.l1m = mean(SH.DYN.MORT.l1, na.rm=TRUE))
ilc_data <- left_join(ilc_data, mn_inf_mort, by="year")
ilc_data <- mutate(ilc_data, 
                   SH.DYN.MORT.l1norm = (SH.DYN.MORT.l1 - SH.DYN.MORT.l1m)/SH.DYN.MORT.l1m,
                   SH.DYN.MORT.l1norml10 = (log10(SH.DYN.MORT.l1) - SH.DYN.MORT.l1ml10))


# Add duration variables first, then drop missing
train <- ilc_data %>% filter(date < calib_start) %>%
  add_duration(., "ilc", "gwcode", "date", ongoing=FALSE) %>%
  filter(date >= train_start & !drop)

calib <- ilc_data %>% filter(date < test_start) %>%
  add_duration(., "ilc", "gwcode", "date", ongoing=FALSE) %>%
  filter(date >= calib_start & !drop)

test <- ilc_data %>% filter(date <= data_end) %>%
  add_duration(., "ilc", "gwcode", "date", ongoing=FALSE) %>%
  filter(date >= test_start & !drop)

#   Start model estimation
#   ======================

# Leader characteristics
model1 <- spduration::spdur(
  duration ~ 1 + log10(i_matl_conf_DIStGOV_l1+1) +
    log10(i_matl_coop_GOVtGOV_l1+1) + ldr_age + log10(events_by_mth),
  atrisk ~ 1 + ldr_irregular + ldr_foreign + log10(mths_in_power+1) +
    log10(events_by_mth),
  data = train, silent = TRUE)

# Public discontent
model2 <- spduration::spdur(
  duration ~ 1 + log10(i_verb_coop_GOVtGOV_l1+1) +
    log10(i_verb_conf_GOVtDIS_l1+1) + log10(i_verb_conf_DIStGOV_l1+1) +
    log10(i_protest_tGOV_l1+1) + log10(events_by_mth),
  atrisk ~ 1 + IT.NET.USER.P2.l1 + IT.CEL.SETS.P2.l1 + log10(exclpop.l1+1) +
    AUTOC.l1,
  data = train, silent = TRUE)

# Global instability
model3 <- spduration::spdur(
  duration ~ 1 + W.knn4.std.eth.rel.h.count.l1 + W.knn4.std.cw.h.count.both.l1 + 
    log10(events_by_mth),
  atrisk ~ 1 + gold_regime_l1 + log10(exclpop.l1+1) + (SH.DYN.MORT.l1norml10),
  data = train, silent = TRUE)

model4 <- spduration::spdur(
  duration ~ 1 + eth.rel.l.count.l1 + reb.l.count.both.l1 + protest.tALL.l1 +
    W.gower.pol.reb.l.count.both.l1,
  atrisk ~ 1+ dom.cris.i.count.l1 + log10(MS.MIL.XPND.GD.ZS.l1) +
    log10(events_by_mth),
  data = train, silent = TRUE)

# model5 <- spduration::spdur(
#   duration ~ 1 + log10(W.centdist.std.opp_resistance.l1+1) +
#     log10(W.centdist.std.repression.l1+1),
#   atrisk ~ 1 + Amnesty.l1 + log10(ProxElection.l1+1) +
#     log10(opp_resistance.l1+1) + log10(SP.POP.TOTL.l1),
#   data = train, silent = TRUE)

model5 <- spduration::spdur(
  duration ~ 1 + log10(W.centdist.std.opp_resistance.l1+1) +
    log10(W.centdist.std.repression.l1+1) + log10(events_by_mth),
  atrisk ~ 1 + log10(SP.POP.TOTL.l1) + log10(NY.GDP.MKTP.KD.l1) +
    log10(opp_resistance.l1+1) + log10(events_by_mth),
  data = train, silent = TRUE)

# model6 <- spduration::spdur(
#   duration ~ 1 + log(intratension.l1 * i_protest_tGOV_l1*IT.CEL.SETS.P2.l1 + 1 ) +
#     log(intratension.l1+1) +
#     log(i_protest_tGOV_l1+1) +
#     log(IT.CEL.SETS.P2.l1+1),
#   atrisk ~ 1 + log10(NY.GDP.PCAP.KD.l1)  +ProxElection.l1 + AUTOC.l1,
#   data = train, silent = TRUE)

model6 <- spduration::spdur(
  duration ~ 1 + log(intratension.l1 * i_protest_tGOV_l1*IT.CEL.SETS.P2.l1 + 1 ) +
    log(intratension.l1+1) +
    log(i_protest_tGOV_l1+1) +
    log(IT.CEL.SETS.P2.l1+1) + log10(events_by_mth),
  atrisk ~ 1 + log10(NY.GDP.PCAP.KD.l1) + AUTOC.l1,
  data = train, silent = TRUE)

# model7 <- spduration::spdur(
#   duration ~  (FP.CPI.TOTL.ZG.l1>5) + food_price_idx_l1 + + eu_brent_oil_d1_l1,
#   atrisk ~ 1 + log10(NY.GDP.PCAP.KD.l1) + Amnesty.l1 + log10(ProxElection.l1+1) +
#     log10(opp_resistance.l1+1) + log10(SP.POP.TOTL.l1),
#   data = train, silent = TRUE)

model7 <- spduration::spdur(
  duration ~  (FP.CPI.TOTL.ZG.l1>5) + food_price_idx_l1 + + eu_brent_oil_d1_l1,
  atrisk ~ 1 + log10(NY.GDP.MKTP.KD.l1) + log10(SP.POP.TOTL.l1) +
    log10(opp_resistance.l1+1) + log10(events_by_mth),
  data = train, silent = TRUE)

# Set number of models and their names. We will need this several times
# for the code below.
n_models <- 7
model_names <- c("Leaders", "Public Disc.", "Global Instab.", "Protest",
                 "Contagion", "Int. Conflict", "Financial")

# Print theme model results for SI
for (i in 1:7) {
  xtbl <- xtable(get(paste0("model", i)), caption = model_names[i],
    label = paste0("model", i))
  colnames(xtbl) <- gsub("Estimate", "beta", colnames(xtbl))
  colnames(xtbl) <- gsub("StdErr", "SE", colnames(xtbl))
  print(xtbl, include.rownames=FALSE)
}

save(model1, model2, model3, model4, model5, model6, model7,
     file="data/models.rda")


# Ensemble ----------------------------------------------------------------


# Calculate theme model predictions
# Initilize matrices with columns for theme predictions and observed
pr_calib <- matrix(numeric(nrow(calib)*(n_models + 1)), ncol=(n_models + 1))
pr_calib[, 1] <- calib$failure
colnames(pr_calib) <- c("y", paste0("i", 1:n_models))

pr_test  <- matrix(numeric(nrow(test)*(n_models + 1)), ncol=(n_models + 1))
pr_test[, 1] <- test$failure
colnames(pr_test) <- c("y", paste0("i", 1:n_models))

# Loop through and fill in
for (i in 1:n_models) {
  model_i <- get(paste0("model", i))
  pr_calib[, i+1] <- predict(model_i, newdata=calib, type="conditional hazard")
  pr_test[, i+1]  <- predict(model_i, newdata=test, type="conditional hazard")
}

# fix exact 0's to slightly above 0, otherwise EBMA will not work
mod_idx <- grep("i[0-9]", colnames(pr_calib))
pr_calib[, mod_idx] <- replace(pr_calib[, mod_idx], pr_calib[, mod_idx] <= 0, 1e-19)
pr_test[, mod_idx]  <- replace(pr_test[, mod_idx], pr_test[, mod_idx] == 0, 1e-19)

# Create ensemble data object
ens_df <- makeForecastData(
  .predCalibration     = pr_calib[, mod_idx],
  .outcomeCalibration = pr_calib[, 1],
  .predTest   		    = pr_test[, mod_idx],
  .outcomeTest 	    	= pr_test[, 1],
  .modelNames = model_names
)

# Calibrate ensemble model
ensemble <- EBMAforecast:::calibrateEnsemble(ens_df, model="logit", maxIter=25000, exp=3,
                              const=0.001)

# Add ID info, theme preds, and ensemble pred
pr_calib <- cbind(calib[, c("gwcode", "date")], pr_calib, ebma=ensemble@predCalibration[, 1, 1])
pr_test <- cbind(test[, c("gwcode", "date")], pr_test, ebma=ensemble@predTest[, 1, 1])

save(pr_calib, file="data/pr_calib.rda")
save(pr_test, file="data/pr_test.rda")

# We will take a closer look at the ensemble below, after running the 6-month 
# rolling forecast tests.


# Test forecasts ----------------------------------------------------------
#
#   Rolling 6-month forecasts over test period


# Calculate covariate values for forecast period; carry forward
# We can use these here as test set for the ensemble
n_ahead <- 6



#   Start of rolling 6-month forecasts
#   ==================================

pr_test6 <- data.frame(NULL)

# Vector of dates from which to do forecasts;
# dates describe the month of data used in generating the forecast
# i.e.
# test start 2012-05 -> first forecast is 2012-06 to 2012-11
last_fcast <- max(ilc_data$date)
month(last_fcast) <- month(data_end) - 6
fcast_dates <- seq.Date(test_start, last_fcast, by = "month")

# For each forecast date,
#   calculate 6-month ahead forecast,
#   collapse 6-month forecast to one record, starting with first month of
#     forecast range as date,
#   re-record observed values for the 6-month forecast period
for (i in seq_along(fcast_dates)) {
  cat(paste0("Forecast from ", fcast_dates[i], "\n"))
  # Run fcast_ebma
  this_fcast <- fcast_ebma(fcast_dates[i], ilc_data, 6)

  # Fill in observed values
  f_min_dt <- unique(this_fcast$date)
  f_max_dt <- f_min_dt
  month(f_max_dt) <- month(f_max_dt) + n_ahead
  obs_y <- test %>%
    dplyr::select(date, gwcode, failure) %>%
    filter(date > f_min_dt & date <= f_max_dt) %>%
    group_by(gwcode) %>%
    dplyr::summarize(y = max(failure))

  this_fcast$y <- obs_y$y[obs_y$gwcode %in% this_fcast$gwcode]

  # record all in giant matrix
  pr_test6 <- rbind(pr_test6, this_fcast)
}

save(pr_test6, file="data/pr_test6.rda")

#
#   Summarize ensemble and input fit
#   _________________________________


# Combine fit statistics for forecast
tab_ebma <- data.frame(
  Model = c(model_names, "Ensemble"),
  W = c(round(ensemble@modelWeights, 2), ""),
  a0 = c(ensemble@modelParams[1, , 1], NA),
  a1 = c(ensemble@modelParams[2, , 1], NA),
  Brier    = apply(pr_calib[, grep("i[0-9]|ebma", colnames(pr_calib))], 2, brier,   obs=pr_calib$y),
  AUC_ROC  = apply(pr_calib[, grep("i[0-9]|ebma", colnames(pr_calib))], 2, auc_roc, obs=pr_calib$y),
  AUC_PR   = apply(pr_calib[, grep("i[0-9]|ebma", colnames(pr_calib))], 2, auc_pr,  obs=pr_calib$y),
  Brier2   = apply(pr_test6[, grep("i[0-9]|ebma", colnames(pr_test6))], 2, brier,   obs=pr_test6$y),
  AUC_ROC2 = apply(pr_test6[, grep("i[0-9]|ebma", colnames(pr_test6))], 2, auc_roc, obs=pr_test6$y),
  AUC_PR2  = apply(pr_test6[, grep("i[0-9]|ebma", colnames(pr_test6))], 2, auc_pr,  obs=pr_test6$y),
  row.names = NULL
)

ens_row <- match("Ensemble", tab_ebma$Model)
tab_ebma <- rbind(tab_ebma[ens_row, ], tab_ebma[-ens_row, ])

tab_ebma %>% xtable(digits=c(0, 0, 2, 2, 2, 5, 3, 3, 4, 3, 3)) %>% 
  print(include.rownames=FALSE)

source("R/theme-summary.r", echo=TRUE)


#
#   Compare 6 month rolling with test fit
#   ______________________________________


#source("R/6month-vs-test.r", echo=TRUE)


# Live forecasts ----------------------------------------------------------
#
#   Real-world forecasts


pr_fcast  <- fcast_ebma("2014-12-01", ilc_data, 6, collapse=FALSE)

# Collapse 6-month forecast and get top 10 table
pr_fcast_agg <- pr_fcast %>%
  group_by(gwcode) %>%
  dplyr::summarize(
    date = min(date),
    y = max(y),
    i1 = p_agg(i1),
    i2 = p_agg(i2),
    i3 = p_agg(i3),
    i4 = p_agg(i4),
    i5 = p_agg(i5),
    i6 = p_agg(i6),
    i7 = p_agg(i7),
    ebma = p_agg(ebma)
  ) %>%
  arrange(desc(ebma)) %>%
  transform(country = countrycode(gwcode, "cown", "country.name")) %>%
  transform(country = prettyc(country))

pr_fcast_agg %>% 
  slice(1:20) %>%
  mutate(country = countrycode(gwcode, "cown", "country.name")) %>%
  mutate(country = prettyc(country)) %>%
  dplyr::select(country, ebma) %>%
  xtable(digits=3) %>% print(include.rownames=TRUE)

save(pr_fcast, file="data/pr_fcast.rda")
save(pr_fcast_agg, file="data/pr_fcast_agg.rda")


# Metric driven forecast tables -------------------------------------------
#
#   Instead of presenting top 10 or 20, use the historic accuracy metrics
#   from the 6-month test period to create tables with expected recall/
#   precision levels
#

#source("R/table-select.r", echo=TRUE)


# Forecast heatmap --------------------------------------------------------
#
#   A table/heatmap that shows the ensemble forecast as well as the input
#   models, and which should help answer the question of how

#source("R/fcast-heatmap.r", echo=TRUE)


# Case examinations -------------------------------------------------------
#
#   Take a look at specific country predictions
#

#source("R/case-details.r")
#source("R/good-bad.r")


# ROC and PR curves -------------------------------------------------------

#source("R/roc-pr-plots.r")


# Compare EBMA and simple average -----------------------------------------

#source("R/ebma-vs-avg.r", echo=TRUE)


# Rolling rankings --------------------------------------------------------
#
#   Examine rankings in rolling 6-month test forecasts and whether related
#   to outcomes. Spaghetti plot of rankings over time.


