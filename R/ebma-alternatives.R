#   Compare EBMA performance to logistic regression and random forest
#

library("dplyr")
library("doMC")
library("lubridate")
library("magrittr")
library("randomForest")
library("spduration")
library("ggplot2")
library("tidyr")
library("ROCR")
library("separationplot")
library("glmnet")
library("caTools")
library("xtable")

source("R/utilities/binary-fit.R")

registerDoMC(cores=2) 
set.seed(1234)


#   Setup data
#   ____________

load("data/ilc-data-2015-08.rda")
#load("data/ilc-data-2015-12.rda")

# One of the spatial lags, W.gower.pol, has missing values. Fix this mistake
# by dropping in correct values. 

load("data/W.gower.fix.rda")
idx <- match(W.gower.pol$id, ilc_data$id)

var_need <- "W.gower.pol.reb.l.count.both.l1"
ilc_data[, var_need][idx] <- ilc_df_has[, var_need]


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

# Rename variables
if (!"AUTOC.l1" %in% names(ilc_data)) {
  ilc_data <- transform(
    ilc_data,
    AUTOC.l1                         = autoc.l1,
    gold_regime_l1                   = gold_regime.l1,
    i_matl_conf_DIStGOV_l1           = DIStGOVmatconflictct.l1,
    i_matl_coop_GOVtGOV_l1           = GOVtGOVmatcoopct.l1,
    i_protest_tGOV_l1                = protest.tGOV.l1,
    i_verb_conf_DIStGOV_l1           = DIStGOVverbalconflictct.l1,
    i_verb_conf_GOVtDIS_l1           = GOVtDISverbalconflictct.l1,
    i_verb_coop_GOVtGOV_l1           = GOVtGOVverbalcoopct.l1,
    eu_brent_oil_d1_l1               = eu_brent_oil.d1.l1,
    food_price_idx_l1                = food_price_idx.l1,  
    W.centdist.std.opp_resistance.l1 = centdist_std.opp_resistance.l1,
    W.centdist.std.repression.l1     = centdist_std.repression.l1,
    W.gower.pol.reb.l.count.both.l1  = gower_pol.reb.l.count.both.l1,
    W.knn4.std.cw.h.count.both.l1    = knn4_std.cw.h.count.both.l1,
    W.knn4.std.eth.rel.h.count.l1    = knn4_std.eth.rel.h.count.l1
  )
}

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

# Superset of train + calib for non-EBMA models
train_calib <- ilc_data %>% filter(date < test_start) %>%
  add_duration(., "ilc", "gwcode", "date", ongoing=FALSE) %>%
  filter(date >= train_start & !drop)





#   EBMA prediction
#   __________

source("R/utilities/predict-ebma.R")

load("data/models.rda")
load("data/ensemble.rda")

n_models <- 7

# Calculate theme model predictions
# Initilize matrices with columns for theme predictions and observed
pr_train <- matrix(numeric(nrow(train_calib)*(n_models + 1)), ncol=(n_models + 1))
pr_train[, 1] <- train_calib$failure
colnames(pr_train) <- c("y", paste0("i", 1:n_models))

pr_test  <- matrix(numeric(nrow(test)*(n_models + 1)), ncol=(n_models + 1))
pr_test[, 1] <- test$failure
colnames(pr_test) <- c("y", paste0("i", 1:n_models))

# Loop through and fill in
for (i in 1:n_models) {
  model_i <- get(paste0("model", i))
  pr_train[, i+1] <- predict(model_i, newdata=train_calib, type="conditional hazard")
  pr_test[, i+1]  <- predict(model_i, newdata=test, type="conditional hazard")
}

# fix exact 0's to slightly above 0, otherwise EBMA will not work
mod_idx <- grep("i[0-9]", colnames(pr_train))
pr_train[, mod_idx] <- replace(pr_train[, mod_idx], pr_train[, mod_idx] <= 0, 1e-19)
pr_test[, mod_idx]  <- replace(pr_test[, mod_idx], pr_test[, mod_idx] <= 0, 1e-19)

ens_train <- predict.ebma(ensemble, pr_train[, mod_idx], train_calib$ilc)
ens_test  <- predict.ebma(ensemble, pr_test[, mod_idx], test$ilc)


#   Construct vector of unique variables used in EBMA models
#   _______________________

load("data/models.rda")

vars <- vector("character")
for (i in 1:7) {
  vars <- c(vars, attr(terms(get(paste0("model", i))$mf.risk), "term.labels"))
  vars <- c(vars, attr(terms(get(paste0("model", i))$mf.dur), "term.labels"))
}
vars <- unique(vars)
vars[vars=="FP.CPI.TOTL.ZG.l1 > 5"] <- "(FP.CPI.TOTL.ZG.l1 > 5)"  # glitch in glm otherwise
vars_no_mod <- vars
vars_no_mod <- gsub("(log10\\()|(log\\()", "", vars_no_mod)
vars_no_mod <- gsub(" \\+ 1\\)$", "", vars_no_mod)
vars_no_mod <- gsub("\\)$", "", vars_no_mod)
vars_no_mod <- gsub("^\\(", "", vars_no_mod)
vars_no_mod <- gsub(" > 5", "", vars_no_mod)
vars_no_mod <- vars_no_mod[!grepl("\\*", vars_no_mod)]


#   All-in logistic regression
#   ________________

f <- paste("ilc ~", paste(vars, collapse = " + "))
logit_mdl <- do.call("glm", list(as.formula(f), data = train_calib, family = "binomial"))

logit_train <- predict(logit_mdl, newdata = train_calib, type = "response")
logit_test  <- predict(logit_mdl, newdata = test, type = "response")


#   LASSO
#   ___________

x <- model.matrix(as.formula(paste("~ 0 + ", paste(vars, collapse = " + "))), data = train_calib)
x_test <- model.matrix(as.formula(paste("~ 0 + ", paste(vars, collapse = " + "))), data = test)

lasso_mdl <- glmnet(x, factor(train_calib$ilc), 
                    family = "binomial")

lasso_mdl2 <- cv.glmnet(x, factor(train_calib$ilc), 
                        family = "binomial",
                        type.measure = "auc")

lasso_mdl2$lambda.min
lasso_mdl2$lambda.1se

plot(lasso_mdl2)

plot(lasso_mdl, label = TRUE, xvar = "lambda")

lasso_train <- predict(lasso_mdl2, type = "response", newx = x)
lasso_test  <- predict(lasso_mdl2, type = "response", newx = x_test)

# What variables are retained at lambda.1se?
coefs <- as.matrix(coef(lasso_mdl2))
coefs <- subset(coefs, coefs[, 1] > 0)


#   Random forest
#   _______________

# Find optimal value for mtry
bestmtry <- tuneRF(x, factor(train_calib[, "ilc"]), 
                   ntreeTry=500, stepFactor=1.5, improve=0.01, 
                   trace=TRUE, plot=TRUE, dobest=FALSE)

rf_mdl <- randomForest(
  x = x, 
  y = factor(train_calib[, "ilc"]), 
  mtry=6, ntree=5000
)

rf_train <- predict(rf_mdl, type = "prob")[, 2]
rf_test  <- predict(rf_mdl, type = "prob", newdata = x_test)[, 2]

importance(rf_mdl)
varImpPlot(rf_mdl)

partialPlot(rf_mdl, x, `log10(W.centdist.std.opp_resistance.l1 + 1)`)


#   Compare month-level predictions
#   __________________


rocdf <- function(pred, obs, data=NULL, type=NULL) {
  # plot_type is "roc" or "pr"
  if (!is.null(data)) {
    pred <- eval(substitute(pred), envir=data)
    obs  <- eval(substitute(obs), envir=data)
  }
  
  rocr_xy <- switch(type, roc=c("tpr", "fpr"), pr=c("prec", "rec"))
  rocr_df <- prediction(pred, obs)
  rocr_pr <- performance(rocr_df, rocr_xy[1], rocr_xy[2])
  xy <- data.frame(rocr_pr@x.values[[1]], rocr_pr@y.values[[1]])
  
  # If PR, designate first (undefined) point as recall = 0, precision = x
  if (type=="pr") {
    xy[1, 2] <- 0
    #xy <- xy[!(rowSums(xy)==0), ]
  }
  
  colnames(xy) <- switch(type, roc=c("tpr", "fpr"), pr=c("rec", "prec"))
  return(xy)
}


perfxy <- function(data, model, plot, pred, obs) {
  # data: char label for data
  # model: char lable for model type
  # plot: char label for plot type, "roc" or "pr"
  # pred: vector predicted values
  # obs:  vector observed values
  # returns: long data frame
  res <- cbind(
    data = data, model = model, plot = plot,
    rocdf(pred, obs, type = plot)
    )
  colnames(res) <- c("data", "model", "plot", "x", "y")
  res
}

perf_data <- rbind(
  # Ensemble
  perfxy("train", "ensemble", "roc", ens_train, train_calib$ilc),
  perfxy("test", "ensemble",  "roc", ens_test,  test$ilc),
  perfxy("train", "ensemble", "pr", ens_train, train_calib$ilc),
  perfxy("test", "ensemble",  "pr", ens_test,  test$ilc),
  # LASSO
  perfxy("train", "lasso", "roc", lasso_train, train_calib$ilc),
  perfxy("test", "lasso",  "roc", lasso_test,  test$ilc),
  perfxy("train", "lasso", "pr", lasso_train, train_calib$ilc),
  perfxy("test", "lasso",  "pr", lasso_test,  test$ilc),
  # RF
  perfxy("train", "rf", "roc", rf_train, train_calib$ilc),
  perfxy("test", "rf",  "roc", rf_test,  test$ilc),
  perfxy("train", "rf", "pr", rf_train, train_calib$ilc),
  perfxy("test", "rf",  "pr", rf_test,  test$ilc)
)

p <- ggplot(perf_data) +
  geom_line(aes(x = x, y = y, colour = model)) + 
  facet_grid(plot ~ data) + 
  theme_bw()

ggsave(file = "figures/app-model-comparison-roc.jpeg", plot = p,
       height = 4, width = 6, dpi = 400)


#   Summary model fit (AUC, etc.)
#   _____________________________

preds_train <- list(
  ens   = ens_train,
  lasso = lasso_train,
  rf    = rf_train
)
preds_test  <- list(
  ens   = ens_test,
  lasso = lasso_test,
  rf    = rf_test
)

fit_tbl <- data.frame(
  Data = factor(c(rep("Train", 9), rep("Test", 9)), levels = c("Train", "Test")),
  Model = rep(c("Ensemble", "Lasso", "Random Forest"), 6),
  stat  = rep(c(rep("AUC-ROC", 3), rep("AUC-PR", 3), rep("Brier", 3)), 2),
  value = c(
    unlist(lapply(preds_train, auc_roc, obs = train_calib$ilc)),
    unlist(lapply(preds_train, auc_pr,  obs = train_calib$ilc)),
    unlist(lapply(preds_train, brier,   obs = train_calib$ilc)),
    unlist(lapply(preds_test, auc_roc,  obs = test$ilc)),
    unlist(lapply(preds_test, auc_pr,   obs = test$ilc)),
    unlist(lapply(preds_test, brier,    obs = test$ilc))
  )
)

tbl <- fit_tbl %>%
  spread(stat, value) 

xtable(tbl, digits=3,
       caption = "Relative model fit",
       label = "tab:fit") %>%
  print(include.rownames = FALSE, file = "tables/tableA1.tex",
        booktabs = TRUE)


#   Separationplots
#   _________________

jpeg("figures/app-model-comparison-sepplots.jpeg", height=1200, width=2048,
    pointsize = 12, res = 400)
par(mfrow = c(3, 2), mar = c(1, 1, 2, 1))
for (model in names(preds_train)) {
  for (d in c("train", "test")) {
    pred   <- as.vector(get(paste0("preds_", d))[[model]])
    actual <- get(switch(d, train = "train_calib", test = "test"))[, "ilc"]
    title  <- paste0(
      switch(model, ens = "Ensemble", rf = "Random Forest", lasso = "LASSO"),
      ", ",
      switch(d, train = "Train", test = "Test")
      )
    separationplot(pred, actual, newplot = FALSE, lwd1 = 1, lwd2 = 1)
    title(main = title)
  }
}
dev.off()

