

appdx_table <- function() {
  # Table of all ILCs in data
  
  library("dplyr")
  library("lubridate")
  
  source("R/utilities/prettyc.r")
  
  load("data/ilc-data-2015-08.rda")
  
  # List all cases of ILC
  all_ilcs <- ilc_data %>%
    filter(ilc==1 & date >= "1991-01-01") %>%
    select(country, date, goelname, irr_exit, irr_entry, mths_in_power)
  
  all_ilcs %<>% mutate(
    date = format(date, "%Y-%m"),
    mths_in_power = as.character(round(round(mths_in_power) / 12))
  ) %>%
    mutate(mths_in_power = gsub("^0", "<1", mths_in_power),
           country = prettyc(country)) %>%
    arrange(date, country)
  
  colnames(all_ilcs) <- c("Country", "Date", "Leader", "Irr. Exit", "Irr. Entry",
                          "Yrs. in power")
  print(xtable(all_ilcs, digits=0), include.rownames=TRUE)
}
