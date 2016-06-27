# Helper functions for forecast data extrapolation for forecast period


# Convenience function to extract year-month from R date
ym <- function(date) {
  format(date, format="%Y-%m")
}

# This function will extrapolate a month's worth of data, with limited
# updating for the duration counters and leader age/tenure.
# We will use predict() instead of forecast() with this new data.
fcast_data <- function(last_month, data, n_ahead) {
  # Carry-forward with limited updating for ILC data
  # Data should already have passed through add_duration
  if ( !"duration" %in% names(data) ) {
    warning(quote(data), " does not appear to have passed through `add_duration`\n",
            "attempting now")
    data <- add_duration(data, "ilc", "gwcode", "date", ongoing=FALSE)
  }
  last_month <- data %>%
    filter(ym(date) == ym(last_month))
  
  df <- data.frame(NULL)
  
  for (i in 1:n_ahead) {
    step <- last_month
    
    # Advance date
    month(step$date) <- month(step$date) + i
    
    # Update duration
    step$duration <- step$duration + i
    
    # Reset failure and risk to 0, in case a unit had a failure last month
    step$atrisk <- 0
    step$failure <- 0
    step$ilc <- 0
    
    # other variable updates
    # Functions to determine months between two dates
    monnb <- function(d) {
      lt <- as.POSIXlt(d)
      lt$year*12 + lt$mon
    }
    monthDiff <- function(d1, d2) {
      abs(monnb(d2) - monnb(d1))
    }
    step$mths_in_power <- monthDiff(step$date, step$goesdate)
    step$ldr_age <- round(monthDiff(step$date, step$goebdate)/12)
    
    df <- rbind(df, step)
  }
  
  return(df)
}



# Helper function to aggregate probabilities
p_agg <- function(x) {
  1 - prod(1 - x)
}



# This function will take data from a given month from `ilc_data`,
# extrapolate it n-months ahead and calculate theme and ensemble forecasts.
fcast_ebma <- function(last_month, data, n_ahead, collapse=TRUE) {
  # Hard coded:
  # ensemble
  # 7 theme models
  # ilc_data
  # pr_calib  - d.f. for calibration data
  obs_df <- ilc_data %>% filter(date <= last_month) %>%
    add_duration(., "ilc", "gwcode", "date", ongoing=FALSE)
  
  # Drop missing
  obs_df <- filter(obs_df, !drop)
  
  # Create data over forecast period
  fcast_df <- fcast_data(max(obs_df$date), obs_df, n_ahead)
  
  # Matrix of input predictions
  pr_fcast  <- matrix(numeric(nrow(fcast_df)*(n_models + 1)), ncol=(n_models + 1))
  pr_fcast[, 1] <- 0  # set outcome to 0...ASSUMPTION
  colnames(pr_fcast) <- c("y", paste0("i", 1:n_models))
  
  # Loop through and fill in
  for (i in 1:n_models) {
    model_i <- get(paste0("model", i))
    type <- switch(class(model_i)[1], 
                   spdur="conditional hazard", 
                   glm="response")
    pr_fcast[, i+1] <- predict(model_i, newdata=fcast_df, type=type)
  }
  
  # Calculate forecasts
  ebma_fcast <- predict.ebma(ensemble, pr_fcast[ ,mod_idx], pr_fcast[, "y"])
  sa_fcast <- rowMeans(pr_fcast[, mod_idx])
  
  # Add ID info, theme preds, and ensemble pred
  pr_fcast <- cbind(fcast_df[, c("gwcode", "date")], pr_fcast, ebma=ebma_fcast,
                    sa=sa_fcast)
  
  # Collapse 6-month forecast into one aggregate slice
  if (isTRUE(collapse)) {
    pr_fcast <- pr_fcast %>%
      group_by(gwcode) %>%
      dplyr::summarize(
        date = min(date),
        y = max(y),
        i1 = p_agg(i1),
        i2 = p_agg(i2),
        i3 = p_agg(i3),
        i4 = p_agg(i4),
        i5 = p_agg(i5),
        i6 = p_agg(i6),
        i7 = p_agg(i7),
        #i8 = p_agg(i8),
        ebma = p_agg(ebma),
        sa = p_agg(sa)
      ) %>% as.data.frame
  }
  # switch this to summarize each 
  
  pr_fcast
}