#Baseflow cq relationships#####
#function to obtain a dataframe with the baseflow cQ slopes 

source("Code/Functions/cQ_stats_functions.R")

baseflow <- function(df, name) {
  
  df <- df %>%
    filter(is.na(event.flag)) %>%
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
    ) 
  
  
  df1 <- df %>%
    drop_na(bf_conc) %>%
    filter(bf > 0) %>%
    filter(bf_conc > 0) %>%
    mutate(discharge = log10(bf)) %>%
    mutate(chloride = log10(bf_conc)) %>%
    mutate(trib = "name")
  
  #get regression information into dataframes
  bf_fit <- summary(lm(chloride~discharge, data = df1))
  bf_oct <- summary(lm(chloride~discharge, data = df1 %>% filter(season == "Oct-Dec")))
  bf_jan <- summary(lm(chloride~discharge, data = df1 %>% filter(season == "Jan-Mar")))
  bf_apr <- summary(lm(chloride~discharge, data = df1 %>% filter(season == "Apr-Jun")))
  bf_jul <- summary(lm(chloride~discharge, data = df1 %>% filter(season == "Jul-Sep")))
  
  
  baseflow_fits <- data.frame(
    trib = c(name, name, name, name, name),
    season = c("Annual", "Oct-Dec", "Jan-Mar", "Apr-Jun", "Jul-Sep"),
    slope = c(
      slope_cq(bf_fit),
      slope_cq(bf_oct),
      slope_cq(bf_jan),
      slope_cq(bf_apr),
      slope_cq(bf_jul)
    ),
    intercept = c(intercept_cq(bf_fit),
                  intercept_cq(bf_oct),
                  intercept_cq(bf_jan),
                  intercept_cq(bf_apr),
                  intercept_cq(bf_jul)
    )
  )
  
  return(baseflow_fits)

}
