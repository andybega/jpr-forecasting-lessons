# Plot of ICEWS totals by month for presentation

library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyr)

source("~/Work/event_db/mysql_setup.R")
con <- mysql_setup(dbname="my_tables")


sql <- "
SELECT  month, 
d6.count AS d6_count,
d5.count AS d5_count,
d4.count AS d4_count
FROM    icews_by_month_drop6 AS d6
LEFT JOIN icews_by_month_drop5 AS d5 USING (month)
LEFT JOIN icews_by_month_drop4 AS d4 USING (month)
ORDER   BY month;"

by_month_raw <- as.tbl(dbGetQuery(con, sql)) 

# Fix dates and convert to long format
by_month <- by_month_raw %>%
  mutate(month = ymd(paste0(month, "01"))) %>%
  rename(
  	drop6 = d6_count,
  	drop5 = d5_count,
  	drop4 = d4_count) %>%
  filter(month > ymd(19900101))

# Plot most recent drop only
bks <- c(1e3, 3e3, 5e3, 7e3, 9e3, 1e4, 3e4, 5e4, 7e4, 9e4, 1e5)
small_bks <- c(rep(1:10, 3) * 10^c(3:5), 2e5)
p <- ggplot(by_month, aes(x=month, y=drop6)) +
  geom_line(color="darkblue") +
  theme_bw() +
  labs(title="ICEWS events by month", x="", y="") +
  geom_vline(xintercept=as.numeric(as.POSIXct("2001-03-01")), col="coral") + 
  geom_vline(xintercept=as.numeric(as.POSIXct("1991-01-01")), col="coral")



ggsave(p, file="~/Desktop/icews-drop6-monthly.png", height=3, width=6)


df <- ilc_data %>%
  filter(date > "1991-01-01") %>%
  group_by(date) %>%
  summarize(events = unique(events_by_mth)) 

bks <- c(1500, 2500, 5000, 10000, 25000, 50000, 100000, 150000)
p <- ggplot(df, aes(x = date, y = events)) + geom_line(size=1) +
  scale_y_log10(breaks=bks) + 
  labs(x=NULL, y=NULL) +
  theme_bw()

ggsave(p, file="graphics/icews-monthly.png", height=3, width=6, scale=0.8)
