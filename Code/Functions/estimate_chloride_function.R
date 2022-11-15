#estimating chloride concentration from continuous logger specific conductivity data 
#and linear regression equations

source('Code/Functions/join_field_cond_function.R')

#function to calculate a timeseries at 30 min timesteps of chloride concentration and loading mass
chloride_ts_mass <- function(field_data, logger_data, discharge_data) {

  # Set up for linear regression
  a = join_for_linreg(field_data, logger_data) |> 
    dplyr::select(date, chloride_mgL, SpCond_uScm.x, MovingAverage_SpCond_uScm) |> 
    group_by(date) |> 
    summarise_if(is.numeric, mean, na.rm = TRUE) |> 
    filter(!is.na(SpCond_uScm.x) | !is.na(MovingAverage_SpCond_uScm)) |> 
    pivot_longer(cols = SpCond_uScm.x: MovingAverage_SpCond_uScm, values_to = 'MovingAverage_SpCond_uScm') |> 
    arrange(chloride_mgL)
    
  
  # Linear fit 
  fit = lm(log10(chloride_mgL) ~ log10(MovingAverage_SpCond_uScm), data = a)
  # summary(fit)
  
  # Join for EC & Discharge 
  site <- logger_data %>% filter(!is.na(MovingAverage_SpCond_uScm)) |> 
    left_join(discharge_data, by = "dateTime")

  minobs <- min(field_data$chloride_mgL) # lowest recorded chloride value
  
  site[1,]
  predict(fit, newdata = site[1,], interval = 'prediction', level = 0.95)
  
  # predictions from linear regression 
  site.pred = data.frame(predict(fit, newdata = site, interval = 'prediction', level = 0.95)) |> 
    mutate_if(is.numeric, funs(10^.)) %>% #if concentration less than 0, set to 0 |> 
    mutate_if(is.numeric, funs(ifelse(. < 0, 0, .))) %>% #if concentration less than 0, set to 0 |> 
    rename(chloride_estimated_mgL = fit, chloride_estimated_mgL_high = upr, chloride_estimated_mgL_low = lwr) 
  
  # bind predictions and calculate load 
  site.load = site |> bind_cols(site.pred) |> 
    mutate(timestep = as.numeric(difftime(dateTime, lag(dateTime)), units="secs")) %>% #timestep in seconds
    mutate(timestep  = ifelse(timestep > 5000, NA, timestep )) %>% #loggers were removed for a week and I don't want to calculate for that time. 
    mutate(chloride_mass_Mg = ((chloride_estimated_mgL * MovingAverage_dis_cms) * timestep) / 1000000) %>% #load in metric tonnes [Mg]
    mutate(chloride_mass_Mg_high = ((chloride_estimated_mgL_high * MovingAverage_dis_cms) * timestep) / 1000000) %>% #load in metric tonnes [Mg]
    mutate(chloride_mass_Mg_low = ((chloride_estimated_mgL_low * MovingAverage_dis_cms) * timestep) / 1000000) %>% #load in metric tonnes [Mg]
    filter(timestep > 0)
  
  return(site.load)
}
