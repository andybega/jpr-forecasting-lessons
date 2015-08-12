# Functionality for extracting tables meeting a desired recall/precision 
# level

library(ROCR)
library(magrittr)

# Load data frame with rolling 6-month predictions from test
#load("data/pr_test6.rda")
#load("data/pr_fcast_agg.rda")

# Large overview:
# Use test forecasts to find threshold value for desired recall/precision
# cut forecast by threshold and present table
#
# needs function for recall/prec matching

# Find cutoff value for target metric
#
# p >= cutoff -> 1
t_select <- function(type=NULL, target=NULL, pred, obs) {
  if (is.null(type)) stop("Specify type: ?performance")
  pred_obj <- prediction(pred, obs)
  perf_obj <- performance(pred_obj, type, x.measure = "cutoff")
  stats   <- perf_obj@y.values[[1]]
  cutoffs <- perf_obj@x.values[[1]]  # >= is 1
  # Find closest value to desired target; print message if not = target
  idx <- which.min(abs(stats - target))
  if (!isTRUE(all.equal(stats[idx], target))) {
    message("Closest value to target is ", stats[idx])
  }
  cutoffs[idx]
}

# For a given cutoff, find performance
t_fit <- function(cutoff, want, pred, obs) {
  # Convert to factor so we don't lose 0 entries in confusion matrix
  obs <- factor(as.numeric(obs), levels=c(0, 1))
  pred <- factor(as.numeric(pred >= cutoff), levels=c(0, 1))
  cmat <- table(obs, pred)
  if (want=="tpr" | want=="rec") {
    res <- cmat[2, 2] / sum(cmat[2, ])
  } else if (want=="fpr") {
    res <- cmat[1, 2] / sum(cmat[1, ])
  } else if (want=="prec") {
    res <- cmat[2, 2] / sum(cmat[, 2])
  } else if (want=="fnr") {
    res <- cmat[2, 1] / sum(cmat[2, ])
  } else {
    stop("Unrecognized want '", want, "'")
  }
  res
}


t1 <- t_select("tpr", 0.5, pr_test6$ebma, pr_test6$y)
t_fit(t1, "prec", pr_test6$ebma, pr_test6$y)
t_fit(t1, "fpr", pr_test6$ebma, pr_test6$y)
# FNR is 1 - recall

pr_fcast_agg %>%
  filter(ebma >= t1) %>%
  mutate(country = prettyc(country)) %>%
  dplyr::select(country, ebma) %>%
  xtable(digits=3)

t1 <- t_select("tpr", 0.76, pr_test6$ebma, pr_test6$y)
t_fit(t1, "prec", pr_test6$ebma, pr_test6$y)
t_fit(t1, "fpr", pr_test6$ebma, pr_test6$y)

pr_fcast_agg %>%
  filter(ebma >= t1) %>%
  mutate(country = prettyc(country)) %>%
  dplyr::select(country, ebma) %>%
  xtable(digits=3)

