#function to join field and logger data for linear regression prep
join_for_linreg <- function(field_data, logger_data) {
  data <- field_data %>%
    mutate(date = as.POSIXct(round_date(dateTime, unit = "30 minutes"))) %>%
    left_join(logger_data, by = c("date" = "dateTime")) #%>%
    # mutate(SpCond_uScm.x = ifelse(is.na(SpCond_uScm.x), MovingAverage_SpCond_uScm, SpCond_uScm.x))
}
