library(countrycode)
library(cshapes)

source("R/utilities/prettyc.r")


# Spaghetti plot ----------------------------------------------------------



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
         any_ilc = max(y))


#   Plot positive test outcomes
#   _____________________________

highlight <- df %>% filter(any_ilc==1)
fcast_ilc <- df %>% filter(y==1)
max_date <- max(df2$date)
ilc_date <- pr_test %>%
  filter(y==1 & date <= max_date)
ilc_date <- left_join(ilc_date, df2[, c("gwcode", "date", "rank")], by=c("gwcode", "date"))

p <- ggplot(df, aes(x=date, y=rank, group=gwcode)) + 
  geom_line(color="gray", alpha=0.7) +
  scale_y_reverse(limits = c(max(df$rank), 1)) +
  geom_line(data=highlight, aes(x=date, y=rank, colour=factor(country)), size=1.5) +
  scale_colour_brewer(name="Country", type="qual", palette=3) +
  geom_point(data=fcast_ilc, colour="black", shape=16, size=2) +
  geom_point(data=ilc_date, colour="black", shape=0, size=4) +
  labs(x="", y="Forecast Rank") +
  theme_bw()
ggsave(plot=p, file="graphics/spaghetti-pos.png", width=6, height=3, scale=1.5)


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
ggsave(plot=p, file="graphics/spaghetti-fp.png", width=6, height=3, scale=1.5)


#   Plot for Yemen, likely fase negative
#   _____________________________________

highlight <- df %>% filter(gwcode==678)

p <- ggplot(df, aes(x=date, y=rank, group=gwcode)) + 
  geom_line(color="gray", alpha=0.7) +
  scale_y_reverse(limits = c(max(df$rank), 1)) +
  geom_line(data=highlight, aes(x=date, y=rank, colour=factor(country)), size=1.5, colour="red") +
  labs(x="", y="Forecast rank") +
  theme_bw()
ggsave(plot=p, file="graphics/spaghetti-yemen.png", width=5, height=3, scale=1.5)



# Map top false positives -------------------------------------------------


# Add rank to test6
df_t6 <- pr_test6 %>%
  group_by(date) %>%
  arrange(desc(ebma)) %>%
  mutate(rank = 1:n())

# Add rank variables
df <- df_t6 %>%
  gather(model, p, ebma) %>%
  group_by(gwcode) %>%
  summarize(rank_sd = sd(rank), rank_min = min(rank), rank_mn = mean(rank),
         any_ilc = max(y)) 

# Mark top false positives
df %<>% filter(any_ilc==0) %>% arrange(rank_mn) %>%
  mutate(rank_fp = 1:n()) %>%
  mutate(
    category = ifelse(rank_fp <=5, "top 5", 
               ifelse(rank_fp > 5 & rank_fp <= 10, "6 - 10",
               ifelse(rank_fp > 10 & rank_fp <= 20, "11 - 20",
                      "other")))) %>%
  as.data.frame
  


# Plot
dpi <- 300
png("graphics/fp-map.png", width=3*dpi, height=1.26*dpi, pointsize=20)
data <- df
id <- "gwcode"
x  <- "category"
cats <- c("top 5", "6 - 10", "11 - 20")
nval <- 4
world <- cshp(date=as.Date("2012-06-30"))
world@data <- data.frame(world@data, data[match(world@data[, 'GWCODE'], data[, id]), ])


# Set fill colors
colorpal <- rev(brewer.pal(nval, 'Reds'))
colors <- ifelse(is.na(world@data[, x]) | world@data[, x]=="other", 
                 "gray90", 
                 colorpal[match(world@data[, x], cats)])

# Plot map
par(mar=c(1, 1, 1, 1))
plot(world, col='gray30', border='gray30', lwd=1)
plot(world, col=colors, border=F, add=T)

# Legend
legend.text <- c(cats, "na")
legend(x=-170, y=15, legend=legend.text, fill=c(colorpal[1:3], "gray90"),
       bty='n')
dev.off()




# Country plots -----------------------------------------------------------

pr_all <- rbind(pr_calib, pr_test, cbind(pr_fcast, sa=NA))

## Israel

df <- pr_all %>%
  filter(gwcode==666)

df_theme <- df %>%
  select(date, grep("i[0-9]", colnames(.))) %>%
  gather(., Theme, prob, -date) %>%
  transform(Theme = factor(Theme, labels = model_names) ) %>%
  filter(Theme %in% c("Leaders", "Public Disc.", "Global Instab.", "Contagion"))

p <- ggplot(df) + geom_line(aes(x=date, y=ebma), col="darkgrey", size=2) +
  geom_line(data=df_theme, aes(x=date, y=prob, col=Theme), alpha=0.5) +
  theme_bw() + ylab("") + xlab("") +
  annotate("rect", xmin=min(pr_fcast$date), xmax=max(pr_fcast$date), ymin=-Inf, ymax=Inf, col="gray", alpha=0.1)

ggsave("graphics/israel.png", plot=p)

## Burkina Faso

df <- pr_all %>%
  filter(gwcode==439)

df_theme <- df %>%
  select(date, grep("i[0-9]", colnames(.))) %>%
  gather(., Theme, prob, -date) %>%
  transform(Theme = factor(Theme, labels = model_names) ) %>%
  filter(Theme %in% c("Leaders", "Public Disc.", "Global Instab.", "Contagion"))

p <- ggplot(df) + geom_line(aes(x=date, y=ebma), col="darkgrey", size=2) +
  geom_line(data=df_theme, aes(x=date, y=prob, col=Theme), alpha=0.5) +
  theme_bw() + ylab("") + xlab("") +
  annotate("rect", xmin=min(pr_fcast$date), xmax=max(pr_fcast$date), ymin=-Inf, ymax=Inf, col="gray", alpha=0.1)

ggsave("graphics/burkina-faso.png", plot=p)

## Ukraine

df <- pr_all %>%
  filter(gwcode==369)

df_theme <- df %>%
  select(date, grep("i[0-9]", colnames(.))) %>%
  gather(., Theme, prob, -date) %>%
  transform(Theme = factor(Theme, labels = model_names) ) %>%
  filter(Theme %in% c("Leaders", "Public Disc.", "Global Instab.", "Contagion"))

p <- ggplot(df) + geom_line(aes(x=date, y=ebma), col="darkgrey", size=2) +
  geom_line(data=df_theme, aes(x=date, y=prob, col=Theme), alpha=0.5) +
  theme_bw() + ylab("") + xlab("") +
  annotate("rect", xmin=min(pr_fcast$date), xmax=max(pr_fcast$date), ymin=-Inf, ymax=Inf, col="gray", alpha=0.1)

ggsave("graphics/ukraine.png", plot=p)




