#   Uniqueness plots, violin plots of predictive densities and bivariate 
#   correlation matrix

library(dplyr)
library(ggplot2)
library(scales)
library(tidyr)
library(magrittr)

load("data/pr_calib.rda")
load("data/pr_test.rda")


# Uniqueness plots --------------------------------------------------------


#   Function for uniquness plots
#   ______________________________

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

# # version of plot with fewer colors
# qn <- quantile(df$p[df$Model=="EBMA"], c(0.5, 0.8, 0.9, 0.95), na.rm = TRUE)
# qn01 <- sort(unique(c(qn, 0, 1)))
# 
# # Interpolate colors
# # For 
# x_end <- length(qn01) + (0 * length(qn01))
# x <- seq(1, x_end, length.out=length(qn01))
# x_out <- seq(1, x_end, by=1)
# #plot(approx(x=x, y=qn01, xout=x_out))
# #plot(density(df$p))
# #plot(ecdf(df$p))
# df$vals <- findInterval(df$p, qn01)
# cols <- colorRampPalette (c("#fdf6e3", "steelblue"))(length(unique(df$vals)))
# ps <- round(qn01, digits=3)
# labels <- paste(cbind(ps[1:(length(ps)-1)], round(ps[2:length(ps)])), collapse=" ", sep = " - ")
# df$vals <- factor(df$vals, levels=c(1:max(df$vals)), )
# 
# p <- ggplot(df, aes(x = x, y = model, fill = df$vals)) +
#   geom_tile() +
#   scale_fill_manual(name="P", values = cols) +
#   labs(x="", y="") + 
#   scale_x_continuous(name="", breaks=NULL) +
#   theme_bw() + facet_wrap( ~ y, scales="free_x")
# p
# 
# p <- ggplot(df, aes(model, country, fill = factor(df$vals))) +
#   geom_tile() +
#   scale_fill_manual(name="P", values=cols) +
#   labs(x="", y="") + 
#   theme_bw()
# p


# H x W for saving uniplots
h <- 2.5
w <- 8

#   Theme and ensemble uniqueness in calib
#   ______________________________________


p <- uniplot1(pr_calib)
p

ggsave(p, file="paper/figures/ebma-uniplot-calib.png", height=h,  width=w)


#   Theme and ensemble uniqueness in test
#   _____________________________________

p <- uniplot1(pr_test)
p

ggsave(p, file="paper/figures/ebma-uniplot-test.png", height=h,  width=w)


#   Theme and ensemble uniqueness in test 6
#   _______________________________________

p <- uniplot1(pr_test6)
p

ggsave(p, file="paper/figures/ebma-uniplot-test6.png", height=h,  width=w)



# Theme and ensemble bivariate cor matrix ---------------------------------


cor_dat <- as.matrix(cor(pr_calib[, grep("i[0-9]|ebma", colnames(pr_calib))]))
cor_dat <- data.frame(Var1=rownames(cor_dat), round(cor_dat, digits=2))
cor_dat <- gather(cor_dat, Var2, value, -Var1)

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
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) 

# Gradient fill:
#scale_fill_gradient(name="Cor.", low="white", high="gray70", 
#                    breaks=seq(0, 1, by = 0.2), limits = c(0, 1)) +
ggsave(filename="paper/figures/pred-cor.png", plot=p, width=5, height=4, units="in",
       dpi=300)



# Violin plots of predictive densities ------------------------------------

df <- pr_test %>%
  gather(model, p, i1:ebma)

lvls <- c("ebma", paste0("i", 1:7))
lbls <- c("EBMA", model_names)
df$model <- factor(df$model, levels = lvls, labels = lbls)

# Density plot with logged axis
ggplot(df, aes(x=p, fill=model)) + 
  stat_density() + 
  scale_x_log10(limits = c(1e-07, 0.2)) +
  theme_bw()
 
ggplot(df, aes(x=p, fill=model)) + 
  stat_density() + 
  scale_x_log10(limits = c(1e-07, 0.2)) +
  theme_bw()

# Violin plots with logged axis; vertical
p <- ggplot(df, aes(x=p)) + 
  stat_density(aes(ymax = ..density..,  ymin = -..density..),
    fill = "grey50", colour = "grey50",
    geom = "ribbon", position = "identity") +
  facet_grid(. ~ model) +
  scale_x_log10(limits=c(quantile(df$p, c(0.01, 0.99)))) +
  coord_flip() +
  theme_bw()

# Violin plots with logged axis; horizontal
p <- ggplot(df, aes(x=p)) + 
  stat_density(aes(ymax = ..density..,  ymin = -..density..),
               fill = "steelblue", colour = "steelblue",
               geom = "ribbon", position = "identity") +
  facet_wrap(~ model, scales="free_y", ncol=1) +
  scale_x_log10("p", limits=c(quantile(df$p, c(0.01, 0.99)))) +
  scale_y_continuous("Density", breaks=NULL) +
  theme_bw()

ggsave(p, file="paper/figures/p-violin.png", height=5,  width=4, scale=1.2)
