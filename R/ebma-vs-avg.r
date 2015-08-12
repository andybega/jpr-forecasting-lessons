# Compare EBMA to simple average, in both OOS test and 6-month
#
# This script picks up from runme.r after theme models and ensemble have
# been estimated already

# Combine fit statistics for forecast
fit_tab2 <- data.frame(
  Model = c(model_names, "Ensemble"),
  W = c(round(ensemble@modelWeights, 2), ""),
  AUC_ROC = apply(pr_calib[, grep("i[0-9]|ebma", colnames(pr_calib))], 2, auc_roc, obs=pr_calib$y),
  AUC_PR  = apply(pr_calib[, grep("i[0-9]|ebma", colnames(pr_calib))], 2, auc_pr, obs=pr_calib$y),
  max_f   = apply(pr_calib[, grep("i[0-9]|ebma", colnames(pr_test))], 2, max_f, obs=pr_calib$y),
  brier   = apply(pr_calib[, grep("i[0-9]|ebma", colnames(pr_test))], 2, brier, obs=pr_calib$y),
  AUC_ROC2 = apply(pr_test[, grep("i[0-9]|ebma", colnames(pr_test))], 2, auc_roc, obs=pr_test$y),
  AUC_PR2  = apply(pr_test[, grep("i[0-9]|ebma", colnames(pr_test))], 2, auc_pr, obs=pr_test$y),
  max_f2   = apply(pr_test[, grep("i[0-9]|ebma", colnames(pr_test))], 2, max_f, obs=pr_test$y),
  brier2   = apply(pr_test[, grep("i[0-9]|ebma", colnames(pr_test))], 2, brier, obs=pr_test$y),
  row.names = NULL
)

ens_row <- match("Ensemble", fit_tab2$Model)
fit_tab2 <- rbind(fit_tab2[ens_row, ], fit_tab2[-ens_row, ])

fit_tab2

#   Simple average, traditional OOS test
#   ====================================

pr_calib$sa <- rowMeans(pr_calib[, grep("i[0-9]", colnames(pr_calib))])
pr_test$sa  <- rowMeans(pr_test[, grep("i[0-9]", colnames(pr_test))])

fit_tab_sa <- data.frame(
  Model = c("Ensemble", "Simple Average"),
  AUC_ROC = apply(pr_calib[, grep("ebma|sa", colnames(pr_calib))], 2, auc_roc, obs=pr_calib$y),
  AUC_PR  = apply(pr_calib[, grep("ebma|sa", colnames(pr_calib))], 2, auc_pr, obs=pr_calib$y),
  max_f   = apply(pr_calib[, grep("ebma|sa", colnames(pr_test))], 2, max_f, obs=pr_calib$y),
  brier   = apply(pr_calib[, grep("ebma|sa", colnames(pr_test))], 2, brier, obs=pr_calib$y),
  AUC_ROC2 = apply(pr_test[, grep("ebma|sa", colnames(pr_test))], 2, auc_roc, obs=pr_test$y),
  AUC_PR2  = apply(pr_test[, grep("ebma|sa", colnames(pr_test))], 2, auc_pr, obs=pr_test$y),
  max_f2   = apply(pr_test[, grep("ebma|sa", colnames(pr_test))], 2, max_f, obs=pr_test$y),
  brier2   = apply(pr_test[, grep("ebma|sa", colnames(pr_test))], 2, brier, obs=pr_test$y),
  row.names = NULL, stringsAsFactors=FALSE
)

fit_tab_sa <- rbind(fit_tab_sa,
                    c("delta", -1*apply(fit_tab_sa[, 2:ncol(fit_tab_sa)], 2, diff)))

fit_tab2
fit_tab_sa


#   Combine tables for output
#   =========================

temp_tab <- fit_tab_sa[, c(1, 6:9)]
colnames(temp_tab) <- colnames(tab_roll_fcast_sa)
sa_comp_tab <- rbind(
  c("Calibration (in sample)", rep(0, 4)),
  fit_tab_sa[, c(1, 2:5)],
  c("Test (out of sample)", rep(0, 4)),
  temp_tab,
  c("Rolling 6-month forecasts (out of sample)", rep(0, 4)),
  tab_roll_fcast_sa)

sa_comp_tab[, 2:5] <- apply(sa_comp_tab[, 2:5], 2, as.numeric)

sa_comp_tab %>% xtable(digits=5) %>% print(include.rownames=FALSE)