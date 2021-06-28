#script calls in conductivity data, sets local timezone, detects, removes outliers, and adds moving average


library(tidyverse)
library(zoo)
library(lubridate)


#read data, set timezone
YRI_cond <- read_csv("Data/Conductivity/YR-I_cond.csv") %>%
  mutate(dateTime = with_tz(dateTime, tzone = "America/Chicago")) %>%
  mutate(ID = "YR-I")
SMC_cond <- read_csv("Data/Conductivity/SMC_cond.csv") %>%
  mutate(dateTime = with_tz(dateTime, tzone = "America/Chicago"))%>%
  mutate(ID = "SMC")
DC_cond <- read_csv("Data/Conductivity/DC_cond.csv") %>%
  mutate(dateTime = with_tz(dateTime, tzone = "America/Chicago"))
PB_cond <- read_csv("Data/Conductivity/PB_cond.csv") %>%
  mutate(dateTime = with_tz(dateTime, tzone = "America/Chicago"))%>%
  mutate(ID = "PB")
YRO_cond <- read_csv("Data/Conductivity/YR-O_cond.csv") %>%
  mutate(dateTime = with_tz(dateTime, tzone = "America/Chicago"))%>%
  mutate(ID = "YR-O")


#function to detect outliers
find_outliers <- function(ConductivityData) {
  #add column for 6 hour moving average and get the total average specific conductivity of the dataset
  cond_data1 <- ConductivityData %>%
    mutate(runningmean = rollmean(SpCond_uScm, 13, fill = NA, na.rm = TRUE)) %>% #use zoo::rollmean over 13 rows (6 hours - 3 before and 3 after each point)
    mutate(runningmean = ifelse(row_number() <= 6, mean(SpCond_uScm[1:6]), runningmean)) %>% # rollmean leaves empty rows at beginning and end of dataset. This line and the one below uses the mean of those empty rows
    mutate(runningmean = ifelse(row_number() >= (nrow(ConductivityData) - 5), mean(SpCond_uScm[(nrow(ConductivityData) - 5):nrow(ConductivityData)]), runningmean))
  
  #outliers identified as datapoints greater than 1.05x the moving average or less than 0.95x the moving average
  cond_data2 <- cond_data1 %>%
    mutate(outlier = ifelse(SpCond_uScm > (1.05 * runningmean), "Y", "N")) %>% #detect outliers as 1.05x greater than running average
    mutate(outlier = ifelse(SpCond_uScm < (0.95 * runningmean), "Y", outlier)) #or as 0.95x less than running average
  
  #write new dataset that includes all original data and the data with outliers removed and the moving average
  cond_data3 <- cond_data2 %>%
    dplyr::select(
      dateTime, #original data: date the conductivivty was measured
      EC_lowRange_uScm, #original data: low range conductivity
      EC_highRange_uScm, #original data: full range conductivity
      Temp_C, #original data: water temperature
      SpCond_uScm, #original data (first modified): specific conductivity calculated
      ID, #original data: lake ID
      outlier #Y or N, identifies outliers
    ) 
 
  #now, remove outliers and calculate the moving average over this dataset
  cond_data4 <- cond_data3 %>%
    mutate(SpCond_uScm = ifelse(outlier == "Y", NA, SpCond_uScm)) %>%
    mutate(runningmean = rollmean(SpCond_uScm, 13, fill = NA, na.rm = TRUE)) %>% #use zoo::rollmean over 13 rows (6 hours - 3 before and 3 after each point)
    mutate(runningmean = ifelse(row_number() <= 6, mean(SpCond_uScm[1:6]), runningmean)) %>% # rollmean leaves empty rows at beginning and end of dataset. This line and the one below uses the mean of those empty rows
    mutate(runningmean = ifelse(row_number() >= (nrow(ConductivityData) - 5), mean(SpCond_uScm[(nrow(ConductivityData) - 5):nrow(ConductivityData)]), runningmean)) %>%
    rename(MovingAverage_SpCond_uScm = runningmean)
    
  
  return(cond_data4)
}

#finalized datasets
YRI_cond <- find_outliers(YRI_cond) #Yahara River inflow
SMC_cond <- find_outliers(SMC_cond) #Sixmile Creek
DC_cond <- find_outliers(DC_cond) #Dorn Creek
PB_cond <- find_outliers(PB_cond) #Pheasant Branch Creek
YRO_cond <- find_outliers(YRO_cond) #Yahara River outlfow

#gets percentage outliers detected 
# (count(DC_cond %>% filter(outlier == "Y"))/nrow(DC_cond)) * 100
# (count(YRI_cond %>% filter(outlier == "Y"))/nrow(YRI_cond)) * 100
# (count(SMC_cond %>% filter(outlier == "Y"))/nrow(SMC_cond)) * 100
# (count(PB_cond %>% filter(outlier == "Y"))/nrow(PB_cond)) * 100
# (count(YRO_cond %>% filter(outlier == "Y"))/nrow(YRO_cond)) * 100
