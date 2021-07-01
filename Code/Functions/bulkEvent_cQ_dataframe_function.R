#bulk stormflow event cq relationships#####
#function to obtain a dataframe with bulk stormflow cQ slopes 

source("Code/Functions/cQ_stats_functions.R")


bulk_event_cq <- function(events_bf, name) {
  
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
    mutate(trib = name)
  
  
  
  df <- df %>%
    drop_na(event_chloride_mgL) %>% #drop NAs and anything below 0 (will not work with log)
    filter(event_flow_cms > 0) %>%
    filter(event_chloride_mgL > 0) %>%
    mutate(discharge = log10(event_flow_cms)) %>% #calculate logs
    mutate(chloride = log10(event_chloride_mgL))
  
  
  
  #get regression information into dataframes
  stormflow_fit <- summary(lm(chloride~discharge, data = df))
  stormflow_oct <- summary(lm(chloride~discharge, data = df %>% filter(season == "Oct-Dec")))
 # stormflow_jan <- summary(lm(chloride~discharge, data = df %>% filter(season == "Jan-Mar")))
  stormflow_apr <- summary(lm(chloride~discharge, data = df %>% filter(season == "Apr-Jun")))
  #stormflow_jul <- summary(lm(chloride~discharge, data = df %>% filter(season == "Jul-Sep")))
  
  
  stormflow_fits <- data.frame(
    trib = c(name, name, name, name, name), #comment out for running YR-O
    #trib = c(name, name, name), #use for running YR-O
    season = c("Annual", "Oct-Dec", "Jan-Mar", "Apr-Jun", "Jul-Sep"), #comment out for running YR-O
    #season = c("Annual", "Oct-Dec", "Apr-Jun"), #use for running YR-O
    slope = c(
      slope_cq(stormflow_fit),
      slope_cq(stormflow_oct),
      slope_cq(stormflow_jan), #comment out for running YR-O
      slope_cq(stormflow_apr),
      slope_cq(stormflow_jul) #comment out for running YR-O
    ),
    intercept = c(intercept_cq(stormflow_fit),
                  intercept_cq(stormflow_oct),
                  intercept_cq(stormflow_jan), #comment out for running YR-O
                  intercept_cq(stormflow_apr),
                  intercept_cq(stormflow_jul) #comment out for running YR-O
    )
  )
  
  return(stormflow_fits)
  
}
