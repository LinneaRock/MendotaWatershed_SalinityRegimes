#Individual stormflow event cq relationships#####
#function to obtain a dataframe with the stormflow cQ slopes 

source("Code/Functions/cQ_stats_functions.R")


each_event_cq <- function(events_bf, name) {
 
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
    mutate(trib = name)
  
  
  
  df <- df %>%
    drop_na(event_chloride_mgL) %>% #drop NAs and anything below 0 (will not work with log)
    filter(event_flow_cms > 0) %>%
    filter(event_chloride_mgL > 0) %>%
    mutate(discharge = log10(event_flow_cms)) %>% #calculate logs
    mutate(chloride = log10(event_chloride_mgL)) %>%
    group_by(event.flag) %>% #grouping by events for individual stormflow event slopes
    mutate(slope = slope_cq(summary(lm(chloride~discharge, data = df %>% group_by(event.flag))))) %>%
    mutate(intercept = intercept_cq(summary(lm(chloride~discharge, data = df %>% group_by(event.flag))))) %>%
    dplyr::select(trib, event.flag, mon, season, slope, intercept) %>%
    distinct() %>%
    mutate(new = ifelse(event.flag == lag(event.flag), "X", event.flag)) %>% #sometimes an event may cross months, next lines ensure these are not double counted
    dplyr::select(-new)

  
}