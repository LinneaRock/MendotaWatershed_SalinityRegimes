#estimating chloride concentration from continuous logger specific conductivity data 
#and linear regression equations

source("Code/Functions/regression_stats_functions.R")

#function to calculate a timeseries at 30 min timesteps of chloride concentration and loading mass
#4 visualization functions follow
chloride_ts_mass <- function(field_data, logger_data, discharge_data) {

  combined <- logger_data %>%
    left_join(discharge_data, by = "dateTime")
  
  sl <- slope(field_data, logger_data) #get slope value
  int <- intercept(field_data, logger_data) #intercept value 
  minobs <- min(field_data$chloride_mgL)
  
  
  ts_load <- combined %>%
    mutate(timestep = dateTime - lag(dateTime)) %>% #timestep in minutes
    mutate(chloride_estimated_mgL = (sl * MovingAverage_SpCond_uScm) + int) %>% #estimate chloride [mg L^-1] for each specific conductivity measure
    mutate(chloride_estimated_mgL = ifelse(chloride_estimated_mgL <= 0, minobs, chloride_estimated_mgL)) %>% #if concentration falls to or below zero, use the minimum observed value
    mutate(chloride_mass_Mg = ((chloride_estimated_mgL * MovingAverage_dis_cms) * timestep) / 1000000) %>% #load in metric tonnes [Mg]
    mutate(chloride_mass_Mg  = ifelse(timestep > 5000, NA, chloride_mass_Mg )) %>% #loggers were removed for a week 
    filter(timestep > 0)
  
  return(ts_load)
  
}

