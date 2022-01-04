#function to calculate flow-normalized chloride and z-score
#first calculate daily means then normalize, etc.

flow_normalize <- function(ts_mass, id) {
  
  ts1 <- ts_mass %>%
    mutate(date = as.Date(dateTime)) %>%
    group_by(date) %>%
    summarise(daily_discharge_cms = mean(MovingAverage_dis_cms, na.rm = TRUE), 
              daily_chloride_mgL = mean(chloride_estimated_mgL, na.rm = TRUE)) %>%
    mutate(normalized_chloride = daily_chloride_mgL / daily_discharge_cms) %>%
    mutate(zscore_chloride = (normalized_chloride - mean(normalized_chloride, na.rm = TRUE)) / sd(normalized_chloride, na.rm = TRUE)) %>%
    mutate(ID = id)
}