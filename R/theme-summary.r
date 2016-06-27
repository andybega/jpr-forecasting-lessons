#
#   Various summary plots of the EBMA and theme predictions
#

library(dplyr)
library(ggplot2)
library(scales)
library(tidyr)
library(magrittr)

load("data/pr_calib.rda")
load("data/pr_test.rda")

model_names <- c("Leaders", "Public Disc.", "Global Instab.", "Protest",
                 "Contagion", "Int. Conflict", "Financial")

figure3a <- function() {
  #
  #   Violin plot of predictive densities
  #
  
  library("ggplot2")
  library("dplyr")
  library("tidyr")
  
  load("data/pr_test.rda")
  
  model_names <- c("Leaders", "Public Disc.", "Global Instab.", "Protest",
                   "Contagion", "Int. Conflict", "Financial")
  
  ##########
  
  df <- pr_test %>%
    gather(model, p, i1:ebma)
  
  lvls <- c("ebma", paste0("i", 1:7))
  lbls <- c("EBMA", model_names)
  df$model <- factor(df$model, levels = lvls, labels = lbls)
  
  # Violin plots with logged axis; horizontal
  p <- ggplot(df, aes(x=p)) + 
    stat_density(aes(ymax = ..density..,  ymin = -..density..),
                 fill = "steelblue", colour = "steelblue",
                 geom = "ribbon", position = "identity") +
    facet_wrap(~ model, scales="free_y", ncol=1) +
    scale_x_log10("p", limits=c(quantile(df$p, c(0.01, 0.99)))) +
    scale_y_continuous("Density", breaks=NULL) +
    theme_bw() +
    theme(plot.title = element_text(size = 19),
          strip.text = element_text(size = 14)) +
    ggtitle("(a)")
  
  ggsave(p, file="figures/p-violin.jpeg", height=4,  width=3, scale=1.6, dpi=400)
  invisible(NULL)
}

figure3a()


figure3b <- function() {
  #
  #   Correlation matrix of EBMA and theme predictions
  #
  
  library("ggplot2")
  library("tidyr")
  
  load("data/pr_calib.rda")
  
  cor_dat <- as.matrix(cor(pr_calib[, grep("i[0-9]|ebma", colnames(pr_calib))]))
  cor_dat <- data.frame(Var1=rownames(cor_dat), round(cor_dat, digits=2))
  cor_dat <- gather(cor_dat, Var2, value, -Var1)
  cor_dat$Var2 <- as.factor(cor_dat$Var2)
  
  labels <- list("Leader char."="i1", "Public discontent"="i2",
                 "Global instability"="i3", "Protest"="i4", "Contagion"="i5",
                 "Internal conflict"="i6", "Financial"="i7", "Ensemble"="ebma")
  levels(cor_dat$Var1) <- rev(labels)
  levels(cor_dat$Var2) <- labels
  
  # Source: http://www.peterhaschke.com/r/2013/04/23/CorrelationMatrix.html
  p <- ggplot(cor_dat, aes(Var2, Var1, fill = value)) +
    geom_tile() +
    geom_text(aes(Var2, Var1, label = value), color = "black", size = 3) +
    scale_fill_gradient(name="Cor.", low = "#fdf6e3", high = "steelblue",
                        breaks=seq(0, 1, by = 0.2), limits = c(0, 1)) +
    labs(x = "", y = "") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
    ggtitle("(b)")
  
  ggsave(filename="figures/pred-cor.jpeg", plot=p, width=5, height=4, units="in",
         dpi=400)
  
  invisible(NULL)
}

figure3b()



figure5 <- function() {
  #
  #   EBMA weights vs model fit
  #
  
  # NOTE: the top portion of this code is also used to create the EBMA
  #       summary table, Table 2
  
  library("EBMAforecast")
  library("xtable")
  library("magrittr")
  library("dplyr")
  
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
  
  ########
  
  tab_ebma_long <- tab_ebma %>%
    mutate(W = c(NA, ensemble@modelWeights))
  tab_ebma_long %<>% gather(fit_stat, value, Brier:AUC_PR2)
  tab_ebma_long %<>% filter(Model != "Ensemble")
  tab_ebma_long %<>% filter(fit_stat %in% c("Brier", "AUC_ROC", "AUC_PR"))
  
  tab_ebma_long$fit_stat <- gsub("\\_", "-", tab_ebma_long$fit_stat)
  tab_ebma_long$fit_stat <- factor(tab_ebma_long$fit_stat, levels = unique(tab_ebma_long$fit_stat))
  
  p <- ggplot(tab_ebma_long, aes(x = W, y = value)) +
    geom_smooth(method="lm", se=FALSE, color="gray80", alpha=0.3) +
    geom_point(colour="gray80", alpha=0.3) +
    geom_text(aes(label=Model), size=3.5, alpha = 0.9, position="jitter") +
    facet_wrap( ~ fit_stat, scales="free_y") +
    theme_bw()
  p <- p + 
    scale_x_continuous(limits = c(0, 0.3)) + 
    labs(y = "")
  ggsave(plot=p, file="figures/weights-vs-fit.jpeg", width=6, height=2, scale=1.5,
         dpi = 400)
  
  invisible(NULL)
}

figure5()


#   Figure 6: uniqueness plots
#   _____________________________


uniplot1 <- function(preds, mdl_names = model_names, labels=FALSE) {
  # Mark dates and convert to long format
  df <- preds %>%
    ungroup() %>%
    arrange(y, ebma) %>%
    dplyr::mutate(x = seq_len(n()), 
                  label = paste(gwcode, substr(date, 1, 7))
    ) %>%
    gather(., model, p, i1:ebma)
  
  # Take out y=NA (test forecasts)
  df <- dplyr::filter(df, !is.na(y))
  
  # Colour by quantile rather than absolute p-values, as the 8 predictions have
  # very different densities.
  df %<>% 
    group_by(model) %>%
    dplyr::mutate(percentile = rank(p) / n())
  
  # Set pretty factor names for models
  lvls <- rev(c("ebma", paste0("i", 1:7)))
  lbls <- rev(c("EBMA", mdl_names))
  df$model <- factor(df$model, levels = lvls, labels = lbls)
  
  p <- ggplot(df, aes(x=x, y=model, fill=percentile)) +
    geom_tile() +
    scale_fill_gradient("Percentile", low="#fdf6e3", high="steelblue") +
    labs(x="", y="") + 
    facet_wrap( ~ y, scales="free_x") +
    scale_x_continuous(breaks=NULL) +
    theme_bw() +
    theme(axis.ticks.x=element_blank(), 
          axis.text.x = element_text(angle = 45, hjust=1)) 
  ## Labels
  # this doesn't work well yet, because y=0 will have some ugly forced labels
  # that are meant only for y=1 cases, because faceting assumes a common scale.
  if (isTRUE(labels)) {
    # Mark breaks by picking dates with observed ILC
    bks_df <- filter(df, model=="EBMA" & y==1)
    bks <- bks_df %>% .$x
    lbl <- with(bks_df, 
                paste(substr(date, 1, 7),  # year-month
                      gwcode               # GW code
                ))
    p <- p + scale_x_continuous(name="", breaks=bks, labels=lbl)
  }
  # done, return plot
  p
}

figure6 <- function() {
  #
  #   Uniqueness plots for calibration and 6-month rolling tests
  #
  
  library("ggplot2")
  library("dplyr")
  library("lubridate")
  
  model_names <- c("Leaders", "Public Disc.", "Global Instab.", "Protest",
                   "Contagion", "Int. Conflict", "Financial")
  
  # H x W for saving uniplots
  h <- 2.7
  w <- 8
  
  load("data/pr_calib.rda")
  load("data/pr_test6.rda")
  
  p <- uniplot1(pr_calib) + 
    ggtitle("(a) Calibration")
  ggsave(p, file="figures/ebma-uniplot-calib.jpeg", height=h,  width=w, dpi=400)
  
  p <- uniplot1(pr_test6) +
    ggtitle("(b) 6-month test forecasts")
  ggsave(p, file="figures/ebma-uniplot-test6.jpeg", height=h,  width=w, dpi=400)
  
  invisible(NULL)
}

figure6()
