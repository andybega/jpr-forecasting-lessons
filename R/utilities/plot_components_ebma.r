# Plot the ensemble and component forecasts for one country

library(ggplot2)
library(dplyr)
library(tidyr)

plot_components_ebma <- function(country_gwcode, country_name, start, df, 
                              model_names, y_adj) {
  # Plot EBMA and component predictions for a country over time
  # Add country rankings in forecast
  df <- df %>%
    gather(model, p, i1:ebma) %>%
    group_by(date, model) %>%
    dplyr::mutate(rank = n() - rank(p) + 1)
  
  this_country <- df %>%
    filter(country == country_name) %>%
    filter(date >= start)
  
  mdl_lbls <- ungroup(this_country) %>% 
    filter(date == max(date)) %>%
    dplyr::select(model, p)
  mdl_lbls$labels <- c(model_names, "EBMA")
  mdl_lbls %<>% ungroup() %>% arrange(desc(p))
  
  # Adjust lables y position
  mdl_lbls$p <- mdl_lbls$p + y_adj
  
  p <- ggplot(this_country, aes(x=date, y=p, color=model, size=model=="ebma")) +
    geom_line() +
    scale_colour_brewer(type="qual", palette="Set2", 
                        name="Model", labels = c(model_names, "EBMA"), guide =FALSE) +
    labs(y = "6-month Prob.") +
    scale_size_manual(values = c(1, 2.4), guide = FALSE) +
    scale_x_date("",
                 limits = c(min(this_country$date), max(this_country$date + 150))) +
    theme_bw(base_size = 16) + 
    theme(legend.key.size = grid::unit(4, "mm"))
  
  # add model names
  p <- p + geom_text(data=mdl_lbls, 
                aes(x = max(df$date) + 10, y=p, label=labels),
                hjust = 0, size = 5)
  # done
  p
}



