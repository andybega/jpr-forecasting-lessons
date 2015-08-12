#   


library(cshp)
library(dplyr)


source("R/utilities/worldMap.r")


#
#   Map of ILCs 
#   _____________


# Aggregate by country
ilc_by_country <- ilc_data %>%
  filter(date >= "1991-01-01") %>%
  group_by(gwcode) %>%
  dplyr::summarize(ilcs = sum(ilc)) %>%
  as.data.frame

# Plot
dpi <- 300
png("graphics/ilc-map.png", width=3*dpi, height=1.26*dpi, pointsize=20)
data <- ilc_by_country
id <- "gwcode"
x  <- "ilcs"
nval <- length(unique(data[, x]))
world <- cshp(date=as.Date("2012-01-01"))
world@data <- data.frame(world@data, data[match(world@data[, 'GWCODE'], data[, id]), ])

# Set fill colors
colorpal <- rev(brewer.pal(nval, 'Reds'))
colors <- ifelse(is.na(world@data[, x])==T, '#B0B0B0', colorpal[match(world@data[, x], sort(unique(world@data[, x]), decreasing=T))])

# Plot map
par(mar=c(1, 1, 1, 1))
plot(world, col='gray30', border='gray30', lwd=1)
plot(world, col=colors, border=F, add=T)

# Legend
legend.text <- c('No data', rev(unlist(dimnames(table(world@data[, x])))))
legend(x=-170, y=15, legend=legend.text, fill=c('#B0B0B0', colorpal),
       bty='n')
dev.off()

#
#   ILCs by year 
#   ______________


ilc_by_yr <- ilc_data %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  dplyr::summarize(ilcs = sum(ilc)) %>%
  filter(year >= 1991)

p <- ggplot(ilc_by_yr, aes(x = year, y = ilcs)) + geom_point() +
  stat_smooth() +
  labs(x = "Year", y = "ILCs") +
  theme_bw()

ggsave(plot=p, file="graphics/ilc-by-year.png", width=3, height=2)


#
#   Table of all ILCs in data 
#   ____________________________

# List all cases of ILC
all_ilcs <- ilc_data %>%
  filter(ilc==1 & date >= "1991-01-01") %>%
  select(country, date, goelname, irr_exit, irr_entry, mths_in_power)

all_ilcs %<>% mutate(
  date = format(date, "%Y-%m"),
  mths_in_power = as.character(round(round(mths_in_power) / 12))
) %>%
  mutate(mths_in_power = gsub("^0", "<1", mths_in_power)) %>%
  arrange(date, country)

colnames(all_ilcs) <- c("Country", "Date", "Leader", "Irr. Exit", "Irr. Entry",
                        "Yrs. in power")
print(xtable(all_ilcs, digits=0), include.rownames=TRUE)


