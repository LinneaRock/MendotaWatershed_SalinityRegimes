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
