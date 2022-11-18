#bulk stormflow event cq relationships#####
#function to obtain a dataframe with bulk stormflow cQ slopes 

source("Code/Functions/cQ_stats_functions.R")


bulk_event_cq <- function(events_bf, name) {
  
  #identify months and seasons
  df <- events_bf %>%
    mutate(event.flag = ifelse(event.flag < 0, event.flag * -1, event.flag)) %>%
    mutate(mon = months.POSIXt(dateTime)) %>%
    mutate(season = NA) %>%
    mutate(season = ifelse(
      mon == "October" |
        mon == "November" |
        mon == "December" , "Oct-Dec", season),
      season =  ifelse(
        mon == "January" |
          mon == "February" |
          mon == "March", "Jan-Mar", season),
      season = ifelse(
        mon == "April" |
          mon == "May" |
          mon == "June", "Apr-Jun", season),
      season = ifelse(
        mon == "July" |
          mon == "August" |
          mon == "September", "Jul-Sep", season),
    ) %>%
    drop_na(event.flag) %>%
    mutate(trib = name)%>%
    mutate(timestep = (dateTime - lag(dateTime)) *  60) %>% #timestep in seconds
    mutate(vol_water = event_flow_cms * timestep)
  
  
  
  df <- df %>%
    drop_na(event_chloride_mgL) %>% #drop NAs and anything below 0 (will not work with log)
    drop_na(event_SpC_uScm) %>%
    filter(event_flow_cms > 0) %>%
    filter(event_chloride_mgL > 0) %>%
    filter(event_SpC_uScm > 0) %>%
    mutate(discharge = log10(event_flow_cms)) %>% #calculate logs
    mutate(chloride = log10(event_chloride_mgL)) %>%
    mutate(SpC = log10(event_SpC_uScm))
  
  
  
  #get regression information into dataframes
  stormflow_fit_Cl <- summary(lm(chloride~discharge, data = df))
  stormflow_oct_Cl <- summary(lm(chloride~discharge, data = df %>% filter(season == "Oct-Dec")))
  stormflow_jan_Cl <- summary(lm(chloride~discharge, data = df %>% filter(season == "Jan-Mar"))) #comment out for running YR-O
  stormflow_apr_Cl <- summary(lm(chloride~discharge, data = df %>% filter(season == "Apr-Jun")))
  stormflow_jul_Cl <- summary(lm(chloride~discharge, data = df %>% filter(season == "Jul-Sep"))) #comment out for running YR-O
  
  stormflow_fit_SpC <- summary(lm(SpC~discharge, data = df))
  stormflow_oct_SpC <- summary(lm(SpC~discharge, data = df %>% filter(season == "Oct-Dec")))
  stormflow_jan_SpC <- summary(lm(SpC~discharge, data = df %>% filter(season == "Jan-Mar"))) #comment out for running YR-O
  stormflow_apr_SpC <- summary(lm(SpC~discharge, data = df %>% filter(season == "Apr-Jun")))
  stormflow_jul_SpC <- summary(lm(SpC~discharge, data = df %>% filter(season == "Jul-Sep"))) #comment out for running YR-O
  
  
  #total volume of water discharged over full study period, plus seasonal volumes (limited to 2020)
  discharge_annual <- sum(df$vol_water, na.rm = TRUE)
  discharge_OctDec <- sum((df %>% filter(year(dateTime) == 2020, season == "Oct-Dec"))$vol_water, na.rm = TRUE)
  discharge_JanMar <- sum((df %>% filter(year(dateTime) == 2020, season == "Jan-Mar"))$vol_water, na.rm = TRUE)
  discharge_AprJun <- sum((df %>% filter(year(dateTime) == 2020, season == "Apr-Jun"))$vol_water, na.rm = TRUE)
  discharge_JulSep <- sum((df %>% filter(year(dateTime) == 2020, season == "Jul-Sep"))$vol_water, na.rm = TRUE)
  
  #total mass of chloride over full study period, plus seasonal masses (limited to 2020)
  chloride_mass_annual <- sum(df$event_chloride_Mg, na.rm = TRUE)
  chloride_mass_OctDec <- sum((df %>% filter(year(dateTime) == 2020, season == "Oct-Dec"))$event_chloride_Mg, na.rm = TRUE)
  chloride_mass_JanMar <- sum((df %>% filter(year(dateTime) == 2020, season == "Jan-Mar"))$event_chloride_Mg, na.rm = TRUE)
  chloride_mass_AprJun <- sum((df %>% filter(year(dateTime) == 2020, season == "Apr-Jun"))$event_chloride_Mg, na.rm = TRUE)
  chloride_mass_JulSep <- sum((df %>% filter(year(dateTime) == 2020, season == "Jul-Sep"))$event_chloride_Mg, na.rm = TRUE)
  
  
  stormflow_fits <- data.frame(
    trib = c(name, name, name, name, name), #comment out for running YR-O
   # trib = c(name, name, name), #use for running YR-O
    season = c("Annual", "Oct-Dec", "Jan-Mar", "Apr-Jun", "Jul-Sep"), #comment out for running YR-O
   # season = c("Annual", "Oct-Dec", "Apr-Jun"), #use for running YR-O
    slope_Cl = c(
      slope_cq(stormflow_fit_Cl),
      slope_cq(stormflow_oct_Cl),
      slope_cq(stormflow_jan_Cl), #comment out for running YR-O
      slope_cq(stormflow_apr_Cl),
      slope_cq(stormflow_jul_Cl) #comment out for running YR-O
    ),
    intercept_Cl = c(intercept_cq(stormflow_fit_Cl),
                  intercept_cq(stormflow_oct_Cl),
                  intercept_cq(stormflow_jan_Cl), #comment out for running YR-O
                  intercept_cq(stormflow_apr_Cl),
                  intercept_cq(stormflow_jul_Cl) #comment out for running YR-O
    ),
   slope_SpC = c(
     slope_cq(stormflow_fit_SpC),
     slope_cq(stormflow_oct_SpC),
     slope_cq(stormflow_jan_SpC), #comment out for running YR-O
     slope_cq(stormflow_apr_SpC),
     slope_cq(stormflow_jul_SpC) #comment out for running YR-O
   ),
   intercept_SpC = c(intercept_cq(stormflow_fit_SpC),
                    intercept_cq(stormflow_oct_SpC),
                    intercept_cq(stormflow_jan_SpC), #comment out for running YR-O
                    intercept_cq(stormflow_apr_SpC),
                    intercept_cq(stormflow_jul_SpC) #comment out for running YR-O
   ),
    water_volume_cm = c(discharge_annual,
                        discharge_OctDec,
                        discharge_JanMar, #comment out for running YR-O
                        discharge_AprJun,
                        discharge_JulSep #comment out for running YR-O
    ),
    total_chloride_mass_Mg = c(chloride_mass_annual,
                               chloride_mass_OctDec,
                               chloride_mass_JanMar,  #comment out for running YR-O
                               chloride_mass_AprJun,
                               chloride_mass_JulSep #comment out for running YR-O
    )
  )
  
  return(stormflow_fits)
  
}


bulk_event_cq_special_YRO <- function(events_bf, name) {
  
  #identfiy months and seasons
  df <- events_bf %>%
    mutate(event.flag = ifelse(event.flag < 0, event.flag * -1, event.flag)) %>%
    mutate(mon = months.POSIXt(dateTime)) %>%
    mutate(season = NA) %>%
    mutate(season = ifelse(
      mon == "October" |
        mon == "November" |
        mon == "December" , "Oct-Dec", season),
      season =  ifelse(
        mon == "January" |
          mon == "February" |
          mon == "March", "Jan-Mar", season),
      season = ifelse(
        mon == "April" |
          mon == "May" |
          mon == "June", "Apr-Jun", season),
      season = ifelse(
        mon == "July" |
          mon == "August" |
          mon == "September", "Jul-Sep", season),
    ) %>%
    drop_na(event.flag) %>%
    mutate(trib = name)%>%
    mutate(timestep = (dateTime - lag(dateTime)) *  60) %>% #timestep in seconds
    mutate(vol_water = event_flow_cms * timestep)
  
  
  
  df <- df %>%
    drop_na(event_chloride_mgL) %>% #drop NAs and anything below 0 (will not work with log)
    drop_na(event_SpC_uScm) %>%
    filter(event_flow_cms > 0) %>%
    filter(event_chloride_mgL > 0) %>%
    filter(event_SpC_uScm > 0) %>%
    mutate(discharge = log10(event_flow_cms)) %>% #calculate logs
    mutate(chloride = log10(event_chloride_mgL)) %>%
    mutate(SpC = log10(event_SpC_uScm))
  
  
  
  #get regression information into dataframes
  stormflow_fit_Cl <- summary(lm(chloride~discharge, data = df))
  stormflow_oct_Cl <- summary(lm(chloride~discharge, data = df %>% filter(season == "Oct-Dec")))
  # stormflow_jan_Cl <- summary(lm(chloride~discharge, data = df %>% filter(season == "Jan-Mar"))) #comment out for running YR-O
  stormflow_apr_Cl <- summary(lm(chloride~discharge, data = df %>% filter(season == "Apr-Jun")))
  # stormflow_jul_Cl <- summary(lm(chloride~discharge, data = df %>% filter(season == "Jul-Sep"))) #comment out for running YR-O
  
  stormflow_fit_SpC <- summary(lm(SpC~discharge, data = df))
  stormflow_oct_SpC <- summary(lm(SpC~discharge, data = df %>% filter(season == "Oct-Dec")))
  # stormflow_jan_SpC <- summary(lm(SpC~discharge, data = df %>% filter(season == "Jan-Mar"))) #comment out for running YR-O
  stormflow_apr_SpC <- summary(lm(SpC~discharge, data = df %>% filter(season == "Apr-Jun")))
  # stormflow_jul_SpC <- summary(lm(SpC~discharge, data = df %>% filter(season == "Jul-Sep"))) #comment out for running YR-O
  
  
  #total volume of water discharged over full study period, plus seasonal volumes (limited to 2020)
  discharge_annual <- sum(df$vol_water, na.rm = TRUE)
  discharge_OctDec <- sum((df %>% filter(year(dateTime) == 2020, season == "Oct-Dec"))$vol_water, na.rm = TRUE)
  discharge_JanMar <- sum((df %>% filter(year(dateTime) == 2020, season == "Jan-Mar"))$vol_water, na.rm = TRUE)
  discharge_AprJun <- sum((df %>% filter(year(dateTime) == 2020, season == "Apr-Jun"))$vol_water, na.rm = TRUE)
  discharge_JulSep <- sum((df %>% filter(year(dateTime) == 2020, season == "Jul-Sep"))$vol_water, na.rm = TRUE)
  
  #total mass of chloride over full study period, plus seasonal masses (limited to 2020)
  chloride_mass_annual <- sum(df$event_chloride_Mg, na.rm = TRUE)
  chloride_mass_OctDec <- sum((df %>% filter(year(dateTime) == 2020, season == "Oct-Dec"))$event_chloride_Mg, na.rm = TRUE)
  chloride_mass_JanMar <- sum((df %>% filter(year(dateTime) == 2020, season == "Jan-Mar"))$event_chloride_Mg, na.rm = TRUE)
  chloride_mass_AprJun <- sum((df %>% filter(year(dateTime) == 2020, season == "Apr-Jun"))$event_chloride_Mg, na.rm = TRUE)
  chloride_mass_JulSep <- sum((df %>% filter(year(dateTime) == 2020, season == "Jul-Sep"))$event_chloride_Mg, na.rm = TRUE)
  
  
  stormflow_fits <- data.frame(
    #trib = c(name, name, name, name, name), #comment out for running YR-O
    trib = c(name, name, name), #use for running YR-O
    #season = c("Annual", "Oct-Dec", "Jan-Mar", "Apr-Jun", "Jul-Sep"), #comment out for running YR-O
    season = c("Annual", "Oct-Dec", "Apr-Jun"), #use for running YR-O
    slope_Cl = c(
      slope_cq(stormflow_fit_Cl),
      slope_cq(stormflow_oct_Cl),
      # slope_cq(stormflow_jan_Cl), #comment out for running YR-O
      slope_cq(stormflow_apr_Cl)
      # slope_cq(stormflow_jul_Cl) #comment out for running YR-O
    ),
    intercept_Cl = c(intercept_cq(stormflow_fit_Cl),
                     intercept_cq(stormflow_oct_Cl),
                     #intercept_cq(stormflow_jan_Cl), #comment out for running YR-O
                     intercept_cq(stormflow_apr_Cl)
                     # intercept_cq(stormflow_jul_Cl) #comment out for running YR-O
    ),
    slope_SpC = c(
      slope_cq(stormflow_fit_SpC),
      slope_cq(stormflow_oct_SpC),
      # slope_cq(stormflow_jan_SpC), #comment out for running YR-O
      slope_cq(stormflow_apr_SpC)
      #  slope_cq(stormflow_jul_SpC) #comment out for running YR-O
    ),
    intercept_SpC = c(intercept_cq(stormflow_fit_SpC),
                      intercept_cq(stormflow_oct_SpC),
                      #intercept_cq(stormflow_jan_SpC), #comment out for running YR-O
                      intercept_cq(stormflow_apr_SpC)
                      #intercept_cq(stormflow_jul_SpC) #comment out for running YR-O
    ),
    water_volume_cm = c(discharge_annual,
                        discharge_OctDec,
                        # discharge_JanMar, #comment out for running YR-O
                        discharge_AprJun
                        #  discharge_JulSep #comment out for running YR-O
    ),
    total_chloride_mass_Mg = c(chloride_mass_annual,
                               chloride_mass_OctDec,
                               #chloride_mass_JanMar,  #comment out for running YR-O
                               chloride_mass_AprJun
                               #  chloride_mass_JulSep #comment out for running YR-O
    )
  )
  
  return(stormflow_fits)
  
}
