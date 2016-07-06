#
#   Figures 1 (a) and (b): summaries of past ILCs
#

figure1a <- function() {
  #
  # Map of ILCs
  #
  
  library("cshapes")
  library("dplyr")
  library("RColorBrewer")
  library("lubridate")
  
  source("R/utilities/prettyc.r")
  
  load("data/ilc-data-2015-08.rda")
  
  # Aggregate by country
  ilc_by_country <- ilc_data %>%
    filter(date >= "1991-01-01") %>%
    group_by(gwcode) %>%
    dplyr::summarize(ilcs = sum(ilc)) %>%
    as.data.frame
  
  # Plot
  dpi <- 400
  jpeg("figures/ilc-map.jpeg", width=3*dpi, height=1.26*dpi, pointsize=20)
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
  legend(x=-170, y=0, legend=legend.text, fill=c('#B0B0B0', colorpal),
         bty='n')
  dev.off()
  
  invisible(NULL)
}

figure1a()


figure1b <- function() {
  #
  # ILCs by year
  #
  
  library("dplyr")
  library("lubridate")
  
  source("R/utilities/prettyc.r")
  
  load("data/ilc-data-2015-08.rda")
  
  ilc_by_yr <- ilc_data %>%
    mutate(year = year(date)) %>%
    group_by(year) %>%
    dplyr::summarize(ilcs = sum(ilc)) %>%
    filter(year >= 1991)
  
  p <- ggplot(ilc_by_yr, aes(x = year, y = ilcs)) + geom_point() +
    stat_smooth() +
    labs(x = "Year", y = "ILCs") +
    theme_bw()
  
  ggsave(plot=p, file="figures/ilc-by-year.jpeg", width=7, height=2, dpi=400)
  
  invisible(NULL)
}

figure1b()

