#
#   Summarize EBMA and theme model fit
#

model_names <- c("Leaders", "Public Disc.", "Global Instab.", "Protest",
                 "Contagion", "Int. Conflict", "Financial")

table2 <- function() {
  #
  #   Table of EBMA weights and EBMA + model fit
  #
  
  # NOTE: this code is also used in the weights-vs-fit calculation for 
  #       Figure 4
  
  library("EBMAforecast")
  library("xtable")
  library("magrittr")
  
  need_data <- list("ensemble.rda", "pr_calib.rda", "pr_test6.rda")
  env <- environment()
  sapply(need_data, function(x) load(file.path("data", x), envir = env))
  
  source("R/utilities/binary-fit.R", local = TRUE)
  
  # take out months with window edge in future
  pr_test6 <- pr_test6[!is.na(pr_test6$y), ]
  
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
    print(include.rownames=FALSE, file = "tables/table2.tex")
  
  invisible(NULL)
}

table2()


