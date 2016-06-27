library("countrycode")
library("cshapes")
library("magrittr")
library("dplyr")
library("countrycode")
library("lubridate")

source("analysis/prettyc.r")

load("data/pr_test6.rda")

#   Spaghetti plots
#   _________________

# Add rank to test6
df_t6 <- pr_test6 %>%
  group_by(date) %>%
  arrange(desc(ebma)) %>%
  mutate(rank = 1:n())

# Add country names and rank variables
df <- df_t6 %>%
  gather(model, p, ebma) %>%
  mutate(country = countrycode(gwcode, "cown", "country.name")) %>%
  mutate(country = prettyc(country)) %>%
  group_by(gwcode) %>%
  mutate(rank_sd = sd(rank), rank_min = min(rank), rank_mn = mean(rank),
         any_ilc = max(y, na.rm=TRUE))


#   Plot positive test outcomes
#   _____________________________

highlight <- df %>% filter(any_ilc==1)
# 6-month forecasts that included ILC in video; seperate by spell
fcast_ilc <- df %>% filter(y==1) %>%
  arrange(gwcode, date) %>%
  select(gwcode, date, rank, country)
fcast_ilc$spell <- 1
for (i in 2:nrow(fcast_ilc)) {
  exp_date <- fcast_ilc$date[i-1] %m+% months(1)
  if(fcast_ilc$date[i] != exp_date) {
    fcast_ilc$spell[i] <- fcast_ilc$spell[i-1] + 1 
  } else {
    fcast_ilc$spell[i] <- fcast_ilc$spell[i-1]
  }
}

max_date <- max(df$date)
ilc_date <- pr_test %>%
  filter(y==1 & date <= max_date)
ilc_date <- left_join(ilc_date, df[, c("gwcode", "date", "rank")], by=c("gwcode", "date"))
ilc_date %<>% 
  mutate(country = prettyc(countrycode(gwcode, "cown", "country.name")))


p <- ggplot(highlight, aes(x=date, y=rank, colour=factor(country))) + 
  geom_line(size=1, alpha=0.3, show_guide=FALSE) +
  scale_y_reverse(breaks=c(1, 20, 40, 60)) +
  scale_colour_brewer(name="Country", type="qual", palette=3, guide=FALSE) +
  labs(x="", y="Forecast Rank") +
  theme_bw()

p <- p + 
  # highlight forecasts that included ILC in window
  geom_line(data=fcast_ilc, size=1.5, aes(group=spell)) +
  # mark ILC dates
  geom_point(data=ilc_date, colour="black", shape=0, size=4) +
  geom_text(data=ilc_date, aes(label=country), show_guide=FALSE,
            hjust=c(1,1,1,1,1.3,-.1,1,-.2,0.5), 
            vjust=c(-.2,-.5,.5,-.2,1.5,1.2,-.4,1.2,1.3), 
            alpha=1, size=6) #, colour="gray30")
p

ggsave(plot=p, file="paper/figures/spaghetti-pos.png", width=6, height=3, scale=1.5)


#   Plot for false positives
#   _____________________________________

highlight <- df %>% filter(rank_mn < 10 & any_ilc==0)

p <- ggplot(df, aes(x=date, y=rank, group=gwcode)) + 
  geom_line(color="gray", alpha=0.7) +
  scale_y_reverse(limits = c(max(df$rank), 1)) +
  geom_line(data=highlight, aes(x=date, y=rank, colour=factor(country)), size=1.5) +
  scale_colour_brewer(name="Country", type="qual", palette=2) +
  labs(x="", y="Forecast rank") +
  theme_bw()

p

ggsave(plot=p, file="paper/graphics/spaghetti-fp.png", width=6, height=3, scale=1.5)


#   Plot for Yemen, likely fase negative
#   _____________________________________

highlight <- df %>% filter(gwcode==678)

p <- ggplot(df, aes(x=date, y=rank, group=gwcode)) + 
  geom_line(color="gray", alpha=0.7) +
  scale_y_reverse(limits = c(max(df$rank), 1)) +
  geom_line(data=highlight, aes(x=date, y=rank, colour=factor(country)), size=1.5, colour="red") +
  labs(x="", y="Forecast rank") +
  theme_bw()
p

ggsave(plot=p, file="paper/graphics/spaghetti-yemen.png", width=5, height=3, scale=1.5)

#   Unravel plot for Syria
#   _______________________

source("analysis/plot-country.ebma.r")
load("data/pr_test6.rda")

library(countrycode)
library(scales)

df <- pr_test6 %>%
  mutate(country = prettyc(countrycode(gwcode, "cown", "country.name"))) %>%
  filter(date >= "2015-01-01" & country=="Syria") %>%
  dplyr::select(-sa)

# Some adjustments since Syria includes 0 predictions
# Recode 0 values so log plot doesn't produce -Inf
# i2 and i4 are problematic and have 0 values. But i4 has non-zero preds as well
# so use this to set floor
y_min   <- min(df$i4[df$i4!=0])

df$i2[df$i2 < y_min] <- y_min * 0.95
df$i4[df$i4 < y_min] <- y_min * 0.95

p <- plot_components.ebma(
  country_name = "Syria",
  country_gwcode = NULL,
  df = df,
  model_names = c("Leaders", "Public Disc.", "Global Instab.", "Protest", "Contagion", "Int. Conflict", "Financial"),
  start = "2015-01-01",
  y_adj = c(0, 5e-2, 5e-3, 0, -1e-4, -2e-4, 0, 1e-10)
  )
p <- p + 
  scale_y_log10() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(plot=p, file="figures/unravel-syria.png", height=4, width=4)
