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