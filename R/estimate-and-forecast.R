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

# load data
load(file="data/ilc-data-2015-08.rda")


# Descriptive ILC stats ---------------------------------------------------

source("analysis/ilc-summary.r", echo=TRUE)


# Theme models ------------------------------------------------------

# Dates:
# train = data used to estimate models
# calib + test = test data for theme model estimates
# calib = calibration period for ensemble
# test  = test period for ensemble
train_start <- as.Date("1991-03-01")
calib_start <- as.Date("2010-01-01")
test_start  <- as.Date("2012-05-01")
data_end    <- as.Date(max(ilc_data$date))

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

#
#   Start model estimation
#   ######################
#

# Leader characteristics
model1 <- spduration::spdur(
  duration ~ 1 + log10(i_matl_conf_DIStGOV_l1+1) +
    log10(i_matl_coop_GOVtGOV_l1+1) + ldr_age + log10(events_by_mth_l1),
  atrisk ~ 1 + ldr_irregular + ldr_foreign + log10(mths_in_power+1) +
    log10(events_by_mth_l1),
  data = train, silent = TRUE)

# Public discontent
model2 <- spduration::spdur(
  duration ~ 1 + log10(i_verb_coop_GOVtGOV_l1+1) +
    log10(i_verb_conf_GOVtDIS_l1+1) + log10(i_verb_conf_DIStGOV_l1+1) +
    log10(i_protest_tGOV_l1+1) + log10(events_by_mth_l1),
  atrisk ~ 1 + IT.NET.USER.P2.l1 + IT.CEL.SETS.P2.l1 + log10(exclpop.l1+1) +
    AUTOC.l1,
  data = train, silent = TRUE)

# Global instability
model3 <- spduration::spdur(
  duration ~ 1 + W.knn4.std.eth.rel.h.count.l1 + W.knn4.std.cw.h.count.both.l1 + 
    log10(events_by_mth_l1),
  atrisk ~ 1 + gold_regime_l1 + log10(exclpop.l1+1) + (SH.DYN.MORT.l1norml10),
  data = train, silent = TRUE)

model4 <- spduration::spdur(
  duration ~ 1 + eth.rel.l.count.l1 + reb.l.count.both.l1 + protest.tALL.l1 +
    W.gower.pol.reb.l.count.both.l1,
  atrisk ~ 1+ dom.cris.i.count.l1 + log10(MS.MIL.XPND.GD.ZS.l1) +
    log10(events_by_mth_l1),
  data = train, silent = TRUE)

model5 <- spduration::spdur(
  duration ~ 1 + log10(W.centdist.std.opp_resistance.l1+1) +
    log10(W.centdist.std.repression.l1+1) + log10(events_by_mth_l1),
  atrisk ~ 1 + log10(SP.POP.TOTL.l1) + log10(NY.GDP.MKTP.KD.l1) +
    log10(opp_resistance.l1+1) + log10(events_by_mth_l1),
  data = train, silent = TRUE)

model6 <- spduration::spdur(
  duration ~ 1 + log(intratension.l1 * i_protest_tGOV_l1*IT.CEL.SETS.P2.l1 + 1 ) +
    log(intratension.l1+1) +
    log(i_protest_tGOV_l1+1) +
    log(IT.CEL.SETS.P2.l1+1) + log10(events_by_mth_l1),
  atrisk ~ 1 + log10(NY.GDP.PCAP.KD.l1) + AUTOC.l1,
  data = train, silent = TRUE)

model7 <- spduration::spdur(
  duration ~  (FP.CPI.TOTL.ZG.l1>5) + food_price_idx_l1 + + eu_brent_oil_d1_l1,
  atrisk ~ 1 + log10(NY.GDP.MKTP.KD.l1) + log10(SP.POP.TOTL.l1) +
    log10(opp_resistance.l1+1) + log10(events_by_mth_l1),
  data = train, silent = TRUE)

# Set number of models and their names. We will need this several times
# for the code below.
n_models <- 7
model_names <- c("Leaders", "Public Disc.", "Global Instab.", "Protest",
                 "Contagion", "Int. Conflict", "Financial")

save(model_names, model1, model2, model3, model4, model5, model6, model7,
     file="data/models.rda")

#
#   Print model estimates
#   #####################
#

source("analysis/prettyvar.R")

model_names_long <- c(
  "Leader characteristics",
  "Public discontent",
  "Global instability (Goldstone)",
  "Protest",
  "Contagion", 
  "Internal conflict",
  "Financial instability"
)

for (i in 1:7) {
  mdl <- as.data.frame(get(paste0("model", i)))
  mdl <- cbind(Variable=rownames(mdl), mdl)
  rownames(mdl) <- NULL
  mdl$Variable <- prettyvar(mdl$Variable)
  mdl$Variable <- gsub("\\.[0-9]", "", mdl$Variable)  # remove .# for duplicate names
  mdl$Variable <- gsub("(?<!\\\\)\\_", "\\\\\\_", mdl$Variable, perl=TRUE)  # escape remaining underscores for latex
  xtable(mdl, 
         caption=model_names_long[i],
         label=paste0("theme", i),
         align=c("l", "p{3in}", "r", "r", "r")) %>% 
    print(., comment=TRUE, booktabs=TRUE, sanitize.text.function=identity,
          include.rownames=FALSE, print.results=FALSE,
          table.placement="ht", caption.placement="top") %>%
    gsub("Risk eq\\.", "\\\\midrule Risk eq\\.", .) %>%
    cat()
}


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
pr_test[, mod_idx]  <- replace(pr_test[, mod_idx], pr_test[, mod_idx] <= 0, 1e-19)

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
                                             const=0.001, tol = 0.001); summary(ensemble)

save(ensemble, file = "data/ensemble.rda")

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
fcast_data_dates <- seq.Date(min(test$date), max(test$date), by = "month")

# For each forecast date,
#   calculate 6-month ahead forecast,
#   collapse 6-month forecast to one record, starting with first month of
#     forecast range as date,
#   re-record observed values for the 6-month forecast period
for (i in seq_along(fcast_data_dates)) {
  cat(paste0("Forecast from ", fcast_data_dates[i], "\n"))
  # You may wonder if the duration components should be rebuilt as well. 
  # They do not have to be. Although usually there would be the risk of future 
  # contamination if risk was coded based on a failure that is in the future and 
  # hence should be unobserved, the forecast data function will reset risk to 0
  # for all extrapolated cases. 
  
  # Run fcast_ebma
  this_fcast <- fcast_ebma(fcast_data_dates[i], test, n_ahead)
  
  # Fill in observed values
  fcast_window_start <- fcast_data_dates[i] %m+% months(1)
  fcast_window_end   <- fcast_window_start  %m+% months(n_ahead - 1)
  obs_y <- test %>%
    dplyr::select(date, gwcode, failure) %>%
    filter(date >= fcast_window_start & date <= fcast_window_end) %>%
    group_by(gwcode) %>%
    dplyr::summarize(y = max(failure))
  # For windows that include unobserved future dates, change 0 to NA
  # we can keep 1 because we know already that at least 1 ILC has occurred
  if (fcast_window_end > max(test$date)) {
    obs_y %<>% mutate(y = ifelse(y==0, NA, 1))
  }
  
  # Fill in observed y 
  this_fcast$y <- NULL
  this_fcast <- left_join(this_fcast, obs_y, by = "gwcode")
  
  # record all in giant matrix
  pr_test6 <- rbind(pr_test6, this_fcast)
}

save(pr_test6, file="data/pr_test6.rda")


#
#   Uniqueness plot, other summary plots of theme and ensemble predictions
#   _____________________________________
#

source("analysis/theme-summary.r", echo=TRUE)



# Live forecasts ----------------------------------------------------------
#
#   Real-world forecasts


pr_fcast  <- fcast_ebma(max(ilc_data$date), ilc_data, 6, collapse=FALSE)

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

save(pr_fcast, file="data/pr_fcast.rda")
save(pr_fcast_agg, file="data/pr_fcast_agg.rda")



#   Forecast heatmap/matrix
#   _________________________

source("analysis/fcast-heatmap.r", echo=TRUE)


#   Case examinations; including Figure 7a and c
#   ___________________________________

source("analysis/case-details.r")




#   Table 4: Uniqueness example
#   __________________


brier_w <- function(w) {
  y <- c(1, 1, 1, 1, 1)
  x <- matrix(c(rep(c(1, 1, 1, 0, 1), 2), c(0, 0, 0, 1, 1)), ncol=3)
  sum((y - x %*% w)^2)/length(y)
}

brier_w(c(.375, .375, .25))
