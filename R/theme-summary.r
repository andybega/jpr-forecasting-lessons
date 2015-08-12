
library(dplyr)
library(ggplot2)
library(scales)
library(tidyr)
library(magrittr)


#   Theme and ensemble uniqueness
#   ______________________________

df <- pr_calib %>%
  arrange(y, ebma) %>%
  dplyr::mutate(x = seq_len(n()), 
                label = paste(gwcode, substr(date, 1, 7))
                ) %>%
  gather(., model, p, i1:ebma)

lvls <- rev(c("ebma", paste0("i", 1:7)))
lbls <- rev(c("EBMA", model_names))
df$model <- factor(df$model, levels = lvls, labels = lbls)

qn <- quantile(df$p[df$model=="EBMA"], c(0.01, 0.5, 0.99), na.rm = TRUE)
qn01 <- sort(unique(rescale(c(qn, range(df$p)))))

# Interpolate colors
# For 
x_end <- length(qn01) + (1 * length(qn01))
x <- seq(1, x_end, length.out=length(qn01))
x_out <- seq(1, x_end, by=1)
#plot(approx(x=x, y=qn01, xout=x_out))
#plot(density(df$p))
#plot(ecdf(df$p))
vals <- approx(x=x, y=qn01, xout=x_out)$y

bks <- c(max(df$x[df$y==0]), unique(df$x[df$y==1]))
lbs <- df$label[bks]

p <- ggplot(df, aes(x=x, y=model, fill=p)) +
  geom_tile() +
  scale_fill_gradientn(
    colours = colorRampPalette (c("#fdf6e3", "steelblue"))(x_end),
    values  = vals
  ) +
  labs(x="", y="") + 
  scale_x_continuous(name="", breaks=bks, labels=lbs) +
  theme_bw() + facet_wrap( ~ y, scales="free_x") +
  theme(axis.ticks.x=element_blank(), axis.text.x = element_text(angle = 45, hjust=1))
p

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

ggsave(p, file="graphics/ebma-uniplot-calib.png", height=3,  width=8)


#   Theme and ensemble uniqueness in test
#   =====================================

df <- pr_test %>%
  arrange(y, ebma) %>%
  dplyr::mutate(x = seq_len(n()), 
                label = paste(gwcode, substr(date, 1, 7))
  ) %>%
  gather(., model, p, i1:ebma)

lvls <- rev(c("ebma", paste0("i", 1:7)))
lbls <- rev(c("EBMA", model_names))
df$model <- factor(df$model, levels = lvls, labels = lbls)

qn <- quantile(df$p[df$model=="EBMA"], c(0.01, 0.5, 0.99), na.rm = TRUE)
qn01 <- sort(unique(rescale(c(qn, range(df$p)))))

# Interpolate colors
# For 
x_end <- length(qn01) + (0 * length(qn01))
x <- seq(1, x_end, length.out=length(qn01))
x_out <- seq(1, x_end, by=1)
#plot(approx(x=x, y=qn01, xout=x_out))
vals <- approx(x=x, y=qn01, xout=x_out)$y

p <- ggplot(df, aes(x=x, y=model, fill=p)) +
  geom_tile() +
  scale_fill_gradientn(
    colours = colorRampPalette (c("#fdf6e3", "steelblue"))(x_end),
    values  = vals
  ) +
  labs(x="", y="") + 
  scale_x_continuous(name="", breaks=NULL) +
  theme_bw() + facet_wrap( ~ y, scales="free_x")
p

ggsave(p, file="graphics/ebma-uniplot-test.png", height=3,  width=8)


#   Theme and ensemble uniqueness in test 6
#   =====================================

df <- pr_test6 %>%
  arrange(y, ebma) %>%
  dplyr::mutate(x = seq_len(n()), 
                label = paste(gwcode, substr(date, 1, 7))
  ) %>%
  gather(., model, p, i1:ebma)

lvls <- rev(c("ebma", paste0("i", 1:7)))
lbls <- rev(c("EBMA", model_names))
df$model <- factor(df$model, levels = lvls, labels = lbls)

qn <- quantile(df$p[df$model=="EBMA"], c(0.01, 0.5, 0.99), na.rm = TRUE)
qn01 <- sort(unique(rescale(c(qn, range(df$p)))))

# Interpolate colors
# For 
x_end <- length(qn01) + (0 * length(qn01))
x <- seq(1, x_end, length.out=length(qn01))
x_out <- seq(1, x_end, by=1)
#plot(approx(x=x, y=qn01, xout=x_out))
vals <- approx(x=x, y=qn01, xout=x_out)$y

p <- ggplot(df, aes(x=x, y=model, fill=p)) +
  geom_tile() +
  scale_fill_gradientn(
    colours = colorRampPalette (c("#fdf6e3", "steelblue"))(x_end),
    values  = vals
  ) +
  labs(x="", y="") + 
  scale_x_continuous(name="", breaks=NULL) +
  theme_bw() + facet_wrap( ~ y, scales="free_x")
p

ggsave(p, file="graphics/ebma-uniplot-test6.png", height=3,  width=8)


#   Theme and ensemble correlation
#   ______________________________

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
  geom_text(aes(Var2, Var1, label = value), color = "#073642", size = 3) +
  scale_fill_gradient(name="Cor.", low = "#fdf6e3", high = "steelblue",
                      breaks=seq(0, 1, by = 0.2), limits = c(0, 1)) +
  labs(x = "", y = "") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
ggsave(filename="graphics/pred-cor.png", plot=p, width=5, height=4, units="in",
       dpi=300)


#   Forecast distributions (violin plot)
#   ________________________


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

# Violin plots with logged axis
p <- ggplot(df, aes(x=p)) + 
  stat_density(aes(ymax = ..density..,  ymin = -..density..),
    fill = "grey50", colour = "grey50",
    geom = "ribbon", position = "identity") +
  facet_grid(. ~ model) +
  scale_x_log10(limits=c(quantile(df$p, c(0.01, 0.99)))) +
  coord_flip() +
  theme_bw()

ggsave(p, file="graphics/p-violin.png", height=3,  width=7, scale=1.3)
