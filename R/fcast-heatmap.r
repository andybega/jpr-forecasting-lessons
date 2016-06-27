#  Heatmap of live forecasts, both ensemble and input models, to determine
#  what is driving a high prediction.

library(dplyr)
library(ggplot2)
library(magrittr)
library(tidyr)
library(scales)

source("R/utilities/prettyc.R")

load("data/pr_fcast.rda")
load("data/pr_fcast_agg.rda")

# Functions for plotting heatmap ------------------------------------------
#
#   How do color the probabilities? This is tricky, as the densities are
#   very different for EBMA and thematic models. Essentially, whether to do by
#   absolute, when we also need to interpolate colors non-linearly, or whether
#   to do by relative percentiles, which obscures difference.

fcast_heatmap1 <- function(df) {

  qn <- quantile(df$p, c(0.4, 0.6, 0.8, 0.9, 0.95), na.rm = TRUE)
  qn01 <- sort(unique(rescale(c(qn, range(df$p)))))
  
  # Interpolate colors
  # For 
  x_end <- length(qn01) + (0 * length(qn01))
  x <- seq(1, x_end, length.out=length(qn01))
  x_out <- seq(1, x_end, by=1)
  #plot(approx(x=x, y=qn01, xout=x_out))
  #plot(density(df$p))
  #plot(ecdf(df$p))
  vals <- approx(x=x, y=qn01, xout=x_out)$y
  
  p <- ggplot(df, aes(Model, country, fill = p)) +
    geom_tile() +
    scale_fill_gradientn(
      colours = colorRampPalette (c("#fdf6e3", "steelblue"))(x_end),
      values  = vals
    ) +
    labs(x="", y="") + 
    theme_bw()
  p
}

fcast_heatmap2 <- function(df) {
  # or in grayscale, with fewer colors:
  # ok, the scale needs work, probably need to change to scale_fill_manual
  # or something and pick colors based on intervals.
  qn <- quantile(df$p, c(0.5, 0.8, 0.9, 0.95), na.rm = TRUE)
  qn01 <- sort(unique(c(qn, 0, 1)))
  
  # Interpolate colors
  # For 
  x_end <- length(qn01) + (0 * length(qn01))
  x <- seq(1, x_end, length.out=length(qn01))
  x_out <- seq(1, x_end, by=1)
  #plot(approx(x=x, y=qn01, xout=x_out))
  #plot(density(df$p))
  #plot(ecdf(df$p))
  df$vals <- findInterval(df$p, qn01)
  cols <- colorRampPalette (c("white", "black"))(length(unique(df$vals)))
  ps <- round(qn01, digits=3)
  labels <- paste(cbind(ps[1:(length(ps)-1)], round(ps[2:length(ps)])), collapse=" ", sep = " - ")
  df$vals <- factor(df$vals, levels=c(1:max(df$vals)), )
  
  p <- ggplot(df, aes(Model, country, fill = factor(df$vals))) +
    geom_tile() +
    scale_fill_manual(name="P", values=cols) +
    labs(x="", y="") + 
    theme_bw()
  p
}


df <- pr_fcast_agg

df %<>% mutate(country = prettyc(as.character(country)))

lvls <- df$country[order(df$ebma, decreasing=FALSE)]
df$country <- factor(df$country, levels=lvls)

df %<>%
  dplyr::select(-date, -gwcode) %>%
  gather(Model, p, i1:ebma)

lvls <- c("ebma", paste0("i", 1:7))
lbls <- c("EBMA", model_names)
df$Model <- factor(df$Model, levels = lvls, labels = lbls)

fcast_heatmap1(df=df)
#ggsave(filename="graphics/fcast-table.png", plot=p, width=7, height=15, units="in",
#       dpi=300)

fcast_heatmap2(df=df)
#ggsave(filename="graphics/fcast-table-bw.png", plot=p, width=7, height=15, units="in",
#       dpi=300)

# by percentile
df %<>% 
  group_by(Model) %>%
  dplyr::mutate(percentile = rank(p) / n())

df %<>% slice(1:28)

p <- ggplot(df, aes(Model, country, fill = percentile)) +
  geom_tile() +
  scale_fill_gradient("Percentile", low="#fdf6e3", high="steelblue") +
  labs(x="", y="") + 
  theme_bw() +
  theme(legend.position="bottom")
p 
ggsave(filename="paper/figures/fcast-table.png", plot=p, width=5, height=5, units="in",
       dpi=300)
# cowplot for changing axis position
       
# old stuff below

# plot log(p) instead; doesn't work
x     <- log10(df$p)
min_x <- min(x[!is.infinite(x)])
df$log_p <- squish_infinite(x, range=c(min_x, 0))
p <- ggplot(df, aes(Model, country, fill = log_p)) +
  geom_tile() +
  scale_fill_gradient2(
    low="#fdf6e3", high="steelblue"
  ) +
  labs(x="", y="") + 
  theme_bw()
p


# Discrete scale by quantiles
qn <- quantile(df$p, c(0.4, 0.6, 0.8, 0.9, 0.95), na.rm = TRUE)
qn <- sort(unique(c(qn, range(df$p))))
qn <- ceiling(qn*10000)/10000

ncols <- length(qn) 
cols  <- colorRampPalette(c("#fdf6e3", "steelblue"))(ncols)

df$col <- as.factor(findInterval(df$p, qn))

p <- ggplot(df, aes(Model, country, fill = col)) +
  geom_tile() +
  scale_fill_manual(
    values = cols,
    breaks = levels(df$col),
    drop=FALSE
  ) +
  labs(x="", y="") + 
  theme_bw()
p