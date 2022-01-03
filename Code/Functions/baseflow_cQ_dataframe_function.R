#Baseflow cq relationships#####
#function to obtain a dataframe with the baseflow cQ slopes 

source("Code/Functions/cQ_stats_functions.R")

baseflow <- function(events_bf, name) {
  
  df <- events_bf %>%
    filter(is.na(event.flag)) %>%
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
    mutate(timestep = (dateTime - lag(dateTime)) *  60) %>% #timestep in seconds
    mutate(vol_water = bf_cms * timestep)
  
  
  df1 <- df %>%
    drop_na(bf_chloride_mgL) %>%
    drop_na(bf_SpC_uScm) %>%
    filter(bf_SpC_uScm > 0) %>%
    filter(bf_cms > 0) %>%
    filter(bf_chloride_mgL > 0) %>%
    mutate(discharge = log10(bf_cms)) %>%
    mutate(chloride = log10(bf_chloride_mgL)) %>%
    mutate(trib = "name")  %>%
    mutate(SpC = log10(bf_SpC_uScm))
  
  #get regression information into dataframes
  bf_fit_Cl <- summary(lm(chloride~discharge, data = df1))
  bf_oct_Cl <- summary(lm(chloride~discharge, data = df1 %>% filter(season == "Oct-Dec")))
  bf_jan_Cl <- summary(lm(chloride~discharge, data = df1 %>% filter(season == "Jan-Mar")))
  bf_apr_Cl <- summary(lm(chloride~discharge, data = df1 %>% filter(season == "Apr-Jun")))
  bf_jul_Cl <- summary(lm(chloride~discharge, data = df1 %>% filter(season == "Jul-Sep")))
  
  bf_fit_SpC <- summary(lm(SpC~discharge, data = df1))
  bf_oct_SpC <- summary(lm(SpC~discharge, data = df1 %>% filter(season == "Oct-Dec")))
  bf_jan_SpC <- summary(lm(SpC~discharge, data = df1 %>% filter(season == "Jan-Mar")))
  bf_apr_SpC <- summary(lm(SpC~discharge, data = df1 %>% filter(season == "Apr-Jun")))
  bf_jul_SpC <- summary(lm(SpC~discharge, data = df1 %>% filter(season == "Jul-Sep")))
  
  #total volume of water discharged over full study period, plus seasonal volumes (limited to 2020)
  discharge_annual <- sum(df1$vol_water, na.rm = TRUE)
  discharge_OctDec <- sum((df1 %>% filter(year(dateTime) == 2020, season == "Oct-Dec"))$vol_water, na.rm = TRUE)
  discharge_JanMar <- sum((df1 %>% filter(year(dateTime) == 2020, season == "Jan-Mar"))$vol_water, na.rm = TRUE)
  discharge_AprJun <- sum((df1 %>% filter(year(dateTime) == 2020, season == "Apr-Jun"))$vol_water, na.rm = TRUE)
  discharge_JulSep <- sum((df1 %>% filter(year(dateTime) == 2020, season == "Jul-Sep"))$vol_water, na.rm = TRUE)
  
  #total mass of chloride over full study period, plus seasonal masses (limited to 2020)
  chloride_mass_annual <- sum(df1$bf_chloride_Mg, na.rm = TRUE)
  chloride_mass_OctDec <- sum((df1 %>% filter(year(dateTime) == 2020, season == "Oct-Dec"))$bf_chloride_Mg, na.rm = TRUE)
  chloride_mass_JanMar <- sum((df1 %>% filter(year(dateTime) == 2020, season == "Jan-Mar"))$bf_chloride_Mg, na.rm = TRUE)
  chloride_mass_AprJun <- sum((df1 %>% filter(year(dateTime) == 2020, season == "Apr-Jun"))$bf_chloride_Mg, na.rm = TRUE)
  chloride_mass_JulSep <- sum((df1 %>% filter(year(dateTime) == 2020, season == "Jul-Sep"))$bf_chloride_Mg, na.rm = TRUE)
  
  
  baseflow_fits <- data.frame(
    trib = c(name, name, name, name, name),
    season = c("Annual", "Oct-Dec", "Jan-Mar", "Apr-Jun", "Jul-Sep"),
    slope_Cl = c(
      slope_cq(bf_fit_Cl),
      slope_cq(bf_oct_Cl),
      slope_cq(bf_jan_Cl),
      slope_cq(bf_apr_Cl),
      slope_cq(bf_jul_Cl)
    ),
    intercept_Cl = c(intercept_cq(bf_fit_Cl),
                  intercept_cq(bf_oct_Cl),
                  intercept_cq(bf_jan_Cl),
                  intercept_cq(bf_apr_Cl),
                  intercept_cq(bf_jul_Cl)
    ),
    slope_SpC = c(
      slope_cq(bf_fit_SpC),
      slope_cq(bf_oct_SpC),
      slope_cq(bf_jan_SpC),
      slope_cq(bf_apr_SpC),
      slope_cq(bf_jul_SpC)
    ),
    intercept_SpC = c(intercept_cq(bf_fit_SpC),
                  intercept_cq(bf_oct_SpC),
                  intercept_cq(bf_jan_SpC),
                  intercept_cq(bf_apr_SpC),
                  intercept_cq(bf_jul_SpC)
    ),
    water_volume_cm = c(discharge_annual,
                        discharge_OctDec,
                        discharge_JanMar,
                        discharge_AprJun,
                        discharge_JulSep),
    total_chloride_mass_Mg = c(chloride_mass_annual,
                               chloride_mass_OctDec,
                               chloride_mass_JanMar,
                               chloride_mass_AprJun,
                               chloride_mass_JulSep)
  )
  
  return(baseflow_fits)

}
