# Heatmap of live forecasts, both ensemble and input models, to determine
# what is driving a high prediction.

library(dplyr)
library(ggplot2)
library(magrittr)
library(tidyr)
library(scales)

load("data/pr_fcast.rda")
load("data/pr_fcast_agg.rda")

df <- pr_fcast_agg

lvls <- df$country[order(df$ebma, decreasing=FALSE)]
df$country <- factor(df$country, levels=lvls)

df %<>%
  dplyr::select(-date, -gwcode) %>%
  gather(Model, p, i1:ebma)

lvls <- c("ebma", paste0("i", 1:7))
lbls <- c("EBMA", model_names)
df$Model <- factor(df$Model, levels = lvls, labels = lbls)

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


ggsave(filename="graphics/fcast-table.png", plot=p, width=7, height=15, units="in",
       dpi=300)

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

ggsave(filename="graphics/fcast-table-bw.png", plot=p, width=7, height=15, units="in",
       dpi=300)
