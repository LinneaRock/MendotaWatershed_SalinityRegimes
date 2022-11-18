#functions to get cQ regression statistics

#slope
slope_cq <- function(fit) {
  round(coef(fit)[2,1], 2)
}

pval_cq <- function(fit) {
  summary(fit)$coefficients[2,4] 
}

#intercept
intercept_cq <- function(fit) {
  round(coef(fit)[1,1], 2)
}


# Add season
addSeason <- function(df) {
  df = df |> 
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
  df$season = factor(df$season,
                                levels = c( "Jan-Mar", "Apr-Jun", "Jul-Sep", "Oct-Dec"))
  return(df)
}

#functions to count types of stormflow events

#cQ slopes < -0.05
count_dilution_events <- function(df, name) {
  
  df <- df %>%
    filter(trib == name) %>%
    filter(slope_SpC < -0.05)
  
  nrow(df)
  
}

#cQ slopes between or equal to -0.05 & 0.05
count_chemostatic_events <- function(df, name) {
  
  df <- df %>%
    filter(trib == name) %>%
    filter(slope_SpC >= -0.05 & slope_SpC <= 0.05)
  
  nrow(df)
  
}

#cQ slopes > 0.05
count_mobilization_events <- function(df, name) {
  
  df <- df %>%
    filter(trib == name) %>%
    filter(slope_SpC > 0.05)
  
  nrow(df)
  
}