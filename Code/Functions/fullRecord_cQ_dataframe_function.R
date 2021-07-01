#Full record cq relationships#####
#function to obtain a dataframe with the full record cQ slopes 

source("Code/Functions/cQ_stats_functions.R")

fullRecord <- function(events_bf, name) {

  #identify months and seasons
  df1 <- events_bf %>%
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
    )
  
  #calculate log of the full record of chloride concentration and discharge 
  df2 <- df1 %>%
    mutate(discharge = all_dis_cms) %>%
    mutate(chloride = all_chloride_mgL) %>%
    filter(chloride > 0) %>%
    filter(discharge > 0) %>%
    mutate(chloride = log10(chloride)) %>%
    mutate(discharge = log10(discharge))
  
  #get regression information into dataframes
  full_fit <- summary(lm(chloride~discharge, data = df2))
  full_oct <- summary(lm(chloride~discharge, data = df2 %>% filter(season == "Oct-Dec")))
  full_jan <- summary(lm(chloride~discharge, data = df2 %>% filter(season == "Jan-Mar")))
  full_apr <- summary(lm(chloride~discharge, data = df2 %>% filter(season == "Apr-Jun")))
  full_jul <- summary(lm(chloride~discharge, data = df2 %>% filter(season == "Jul-Sep")))
 
   
  fullRecord_fits <- data.frame(
    trib = c(name, name, name, name, name),
    season = c("Annual", "Oct-Dec", "Jan-Mar", "Apr-Jun", "Jul-Sep"),
    slope = c(
      slope_cq(full_fit),
      slope_cq(full_oct),
      slope_cq(full_jan),
      slope_cq(full_apr),
      slope_cq(full_jul)
    ),
    intercept = c(intercept_cq(full_fit),
                  intercept_cq(full_oct),
                  intercept_cq(full_jan),
                  intercept_cq(full_apr),
                  intercept_cq(full_jul)
    )
  )
  
  return(fullRecord_fits)
  
}
