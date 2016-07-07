#
#   Various tables for appendix
#   Table A2: list of ILCs
#   Tables A3-9: theme model estimates
#   Table A10: comparison of monthly vs. 6-month test fit
#

ilc_list <- function() {
  # Table of all ILCs in data
  
  library("dplyr")
  library("lubridate")
  library("xtable")
  
  source("R/utilities/prettyc.r")
  
  load("data/ilc-data-2015-08.rda")
  
  # List all cases of ILC
  all_ilcs <- ilc_data %>%
    filter(ilc==1 & date >= "1991-01-01") %>%
    select(country, date, goelname, irr_exit, irr_entry, mths_in_power)
  
  all_ilcs %<>% mutate(
    date = format(date, "%Y-%m"),
    mths_in_power = as.character(round(round(mths_in_power) / 12))
  ) %>%
    mutate(mths_in_power = gsub("^0", "<1", mths_in_power),
           country = prettyc(country)) %>%
    arrange(date, country)
  
  colnames(all_ilcs) <- c("Country", "Date", "Leader", "Irr. Exit", "Irr. Entry",
                          "Yrs. in power")
  print(xtable(all_ilcs, digits=0), include.rownames=TRUE, 
        file = "tables/tableA2.tex")
  
  invisible(NULL)
}

ilc_list()

theme_estimate_tables <- function() {
  library("spduration")
  library("xtable")
  library("magrittr")
  
  source("R/utilities/prettyvar.R")
  
  load("data/models.rda")
  
  model_names_long <- c(
    "Leader characteristics",
    "Public discontent",
    "Global instability (Goldstone)",
    "Protest",
    "Contagion", 
    "Internal conflict",
    "Financial instability"
  )
  
  flname <- file.path("tables", paste0("tableA3-A9.tex"))
  
  for (i in 1:7) {
    mdl <- as.data.frame(get(paste0("model", i)), row.names = FALSE)
    
    mdl$Parameter <- prettyvar(mdl$Parameter)
    mdl$Parameter <- gsub("(Dur|Risk)\\_", "", mdl$Parameter)
    
    colnames(mdl)[1] <- "Variable"
    mdl$`t value` <- NULL
    colnames(mdl)[4] <- "p"
    
    xtable(mdl, 
           caption=model_names_long[i],
           label=paste0("theme", i),
           align=c("l", "p{3in}", "r", "r", "r")) %>% 
      print(., comment=TRUE, booktabs=TRUE, sanitize.text.function=identity,
            include.rownames=FALSE, print.results=FALSE,
            table.placement="ht", caption.placement="top") %>%
      gsub("Risk eq\\.", "\\\\midrule Risk eq\\.", .) %>% 
      write(., file = flname, append = TRUE)
  }
  invisible(NULL)
}

theme_estimate_tables()


#   Table A10
#   Compare regular out-of-sample testing with rolling 6-month test forecasts
#   ____________________________________________________________________________

tableA10 <- function() {
  library("xtable")
  library("EBMAforecast")
  
  source("R/utilities/binary-fit.R")
  
  load("data/pr_calib.rda")
  load("data/pr_test.rda")
  load("data/pr_test6.rda")
  load("data/ensemble.rda")
  
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
  pr_test6 <- pr_test6[!is.na(pr_test6$y), ]
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
  
  out <- cbind(tab_test[, 1:3], tab_test6[, 3], tab_test[, 4], tab_test6[, 4]) 
  colnames(out) <- c("Model", "W", "ROC m", "ROC 6m", "PR m", "PR 6m")
  
  tbl <- xtable(out, digits=3, 
                caption = "Comparison of monthly (m) vs. 6 month (6m) test prediction fit",
                label = "tab:test6")
  print(tbl, include.rownames=FALSE, booktabs = TRUE,
        file = "tables/tableA10.tex")
  
  invisible(NULL)
}

tableA10()
