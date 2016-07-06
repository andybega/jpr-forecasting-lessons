#
#   Figure 7
#   Various country-specific plots for presentation
#

figure7a <- function() {
  #
  #   Spaghetti plot for top fcast countries rankings
  #
  
  library("dplyr")
  library("ggplot2")
  library("tidyr")
  library("countrycode")
  
  source("R/utilities/prettyc.R")
  
  load("data/pr_test6.rda")
  
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
    geom_line(size=1, alpha=0.3, show.legend=FALSE) +
    scale_y_reverse(breaks=c(1, 20, 40, 60)) +
    scale_colour_brewer(name="Country", type="qual", palette=3, guide=FALSE) +
    labs(x="", y="Forecast Rank") +
    theme_bw()
  
  p <- p + 
    # highlight forecasts that included ILC in window
    geom_line(data=fcast_ilc, size=1.5, aes(group=spell)) +
    # mark ILC dates
    geom_point(data=ilc_date, colour="black", shape=0, size=4) +
    geom_text(data=ilc_date, aes(label=country), show.legend=FALSE,
              hjust=c(1.4,1,1,1,1.3,-.1,1,0.5,0.5), 
              vjust=c(6.5,-.3,1.5,-.2,1.7,1.2,1.5,-3.5,1.3), 
              alpha=1, size=6) + 
    theme(plot.title = element_text(size = 18))
  
  #ggsave(plot=p, file="figures/spaghetti-pos.tiff", width=6, height=3, scale=1.5,
  #       dpi = 400)
  #invisible(NULL)
  
  p
}

figure7b <- function() {
  #
  #   Forecast table heatmap 
  #
  
  library("dplyr")
  library("ggplot2")
  library("cowplot")  # to switch axis position in ggplot2
  
  source("R/utilities/prettyc.R")
  
  load("data/pr_fcast_agg.rda")
  
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
  
  df %<>% 
    group_by(Model) %>%
    dplyr::mutate(percentile = rank(p) / n())
  
  df %<>% slice(1:20)
  
  p <- ggplot(df, aes(Model, country, fill = percentile)) +
    geom_tile() +
    scale_fill_gradient("Percentile", low="#fdf6e3", high="steelblue") +
    labs(x="", y="") + 
    theme_bw() +
    theme(legend.position="bottom", 
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) 
  
  p <- ggdraw(switch_axis_position(p, axis = "x"))
  
  #ggsave(filename="figures/fcast-table.tiff", plot=p, width=5, height=5,
  #       dpi=400)
  #invisible(NULL)
  p
}

figure7c <- function() {
  #
  #   Single country EBMA and theme probabilities
  #
  
  library("dplyr")
  library("ggplot2")
  library("countrycode")
  library("scales")
  
  source("R/utilities/plot_components_ebma.R")
  load("data/pr_test6.rda")
  
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
  
  p <- plot_components_ebma(
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
  
  #ggsave(plot=p, file="figures/unravel-syria.tiff", height=4, width=4, dpi=400)
  #invisible(NULL)
  p
}

figure7 <- function() {
  #
  #   Combine plots and save
  #
  
  spaghetti <- figure7a()
  heatmap   <- figure7b()
  unravel   <- figure7c()
  
  p <- ggdraw() +
    draw_plot(spaghetti, 0, .5, 1, .5) + 
    draw_plot(heatmap, 0, 0, .5, .5) +
    draw_plot(unravel, .5, 0, .5, .5) +
    draw_plot_label(c("(a)", "(b)", "(c)"), c(0, 0, 0.5), c(1, 0.5, 0.5), size = 15, family = "serif")
  
  ggsave("figures/figure7.jpeg", p, height=7, width=8, units="in", dpi=400)
  
  invisible(NULL)
}


# Main --------------------------------------------------------------------

figure7()

