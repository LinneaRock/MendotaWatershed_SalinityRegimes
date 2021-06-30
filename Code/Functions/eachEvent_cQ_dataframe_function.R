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
    
  
  df2 <- df1 %>%
    group_by(event.flag) %>%
    mutate(slope = slope_cq(summary(lm(chloride~discharge, data = df1 %>% group_by(event.flag)))))

  
}
