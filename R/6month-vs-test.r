#   Compare regular out-of-sample testing with rolling 6-month test forecasts

library(xtable)
library(magrittr)


#   Comparison with add'l fit statistics
#   =====================================

# calib
tab_calib <- data.frame(
  Model = c(model_names, "Ensemble"),
  W = c(round(ensemble@modelWeights, 2), ""),
  AUC_ROC = apply(pr_calib[, grep("i[0-9]|ebma", colnames(pr_calib))], 2, auc_roc, obs=pr_calib$y),
  AUC_PR  = apply(pr_calib[, grep("i[0-9]|ebma", colnames(pr_calib))], 2, auc_pr, obs=pr_calib$y),
  max_f   = apply(pr_calib[, grep("i[0-9]|ebma", colnames(pr_test))], 2, max_f, obs=pr_calib$y),
  brier   = apply(pr_calib[, grep("i[0-9]|ebma", colnames(pr_test))], 2, brier, obs=pr_calib$y),
  row.names = NULL
)
ens_row <- match("Ensemble", tab_calib$Model)
tab_calib <- rbind(tab_calib[ens_row, ], tab_calib[-ens_row, ])

# test
tab_test <- data.frame(
  Model = c(model_names, "Ensemble"),
  W = c(round(ensemble@modelWeights, 2), ""),
  AUC_ROC = apply(pr_test[, grep("i[0-9]|ebma", colnames(pr_test))], 2, auc_roc, obs=pr_test$y),
  AUC_PR  = apply(pr_test[, grep("i[0-9]|ebma", colnames(pr_test))], 2, auc_pr, obs=pr_test$y),
  max_f   = apply(pr_test[, grep("i[0-9]|ebma", colnames(pr_test))], 2, max_f, obs=pr_test$y),
  brier   = apply(pr_test[, grep("i[0-9]|ebma", colnames(pr_test))], 2, brier, obs=pr_test$y),
  row.names = NULL
)
ens_row <- match("Ensemble", tab_test$Model)
tab_test <- rbind(tab_test[ens_row, ], tab_test[-ens_row, ])

# test 6
tab_test6 <- data.frame(
  Model = c(model_names, "Ensemble"),
  W = c(round(ensemble@modelWeights, 2), ""),
  AUC_ROC = apply(pr_test6[, grep("i[0-9]|ebma", colnames(pr_test6))], 2, auc_roc, obs=pr_test6$y),
  AUC_PR  = apply(pr_test6[, grep("i[0-9]|ebma", colnames(pr_test6))], 2, auc_pr, obs=pr_test6$y),
  max_f   = apply(pr_test6[, grep("i[0-9]|ebma", colnames(pr_test6))], 2, max_f, obs=pr_test6$y),
  brier   = apply(pr_test6[, grep("i[0-9]|ebma", colnames(pr_test6))], 2, brier, obs=pr_test6$y),
  row.names = NULL
)
ens_row <- match("Ensemble", tab_test6$Model)
tab_test6 <- rbind(tab_test6[ens_row, ], tab_test6[-ens_row, ])


# Compare these two
tab_test
tab_test6

cbind(tab_test[, 1:3], tab_test6[, 3], tab_test[, 4], tab_test6[, 4]) %>%
      xtable(digits=3) %>% print(include.rownames=FALSE)
