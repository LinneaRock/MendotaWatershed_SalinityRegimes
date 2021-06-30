#Individual stormflow event cq relationships#####
#function to obtain a dataframe with the stormflow cQ slopes 

source("Code/Functions/cQ_stats_functions.R")


each_event_cq <- function(df, name) {
  
  df <- YRI_events_bf %>%
    mutate(event.flag = ifelse(event.flag < 0, event.flag * -1, event.flag)) %>%
    mutate(mon = months.POSIXt(date)) %>%
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
    mutate(trib = "name")
  
  
  
  df1 <- df %>%
    drop_na(event_conc) %>%
    filter(event_flow > 0) %>%
    filter(event_conc > 0) %>%
    mutate(discharge = log10(event_flow)) %>%
    mutate(chloride = log10(event_conc))
  
  
  
  #get regression information into dataframes
  stormflow_fit <- summary(lm(chloride~discharge, data = df1))
  stormflow_oct <- summary(lm(chloride~discharge, data = df1 %>% filter(season == "Oct-Dec")))
  stormflow_jan <- summary(lm(chloride~discharge, data = df1 %>% filter(season == "Jan-Mar")))
  stormflow_apr <- summary(lm(chloride~discharge, data = df1 %>% filter(season == "Apr-Jun")))
  stormflow_jul <- summary(lm(chloride~discharge, data = df1 %>% filter(season == "Jul-Sep")))
  
  
  stormflow_fits <- data.frame(
    trib = c(name, name, name, name, name),
    season = c("Annual", "Oct-Dec", "Jan-Mar", "Apr-Jun", "Jul-Sep"),
    slope = c(
      slope_cq(stormflow_fit),
      slope_cq(stormflow_oct),
      slope_cq(stormflow_jan),
      slope_cq(stormflow_apr),
      slope_cq(stormflow_jul)
    ),
    intercept = c(intercept_cq(stormflow_fit),
                  intercept_cq(stormflow_oct),
                  intercept_cq(stormflow_jan),
                  intercept_cq(stormflow_apr),
                  intercept_cq(stormflow_jul)
    )
  )
  
  return(stormflow_fits)
  
}