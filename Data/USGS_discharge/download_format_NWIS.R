library(tidyverse)
library(zoo)
library(dataRetrieval)
library(data.table)
library(lubridate)

#code to download and prepare USGS discharge data for analyses
format <- function(d) {
  d %>%
    rename(discharge_cfs = X_00060_00000) %>%
    mutate(discharge_cms = discharge_cfs * 0.028316847) %>% #convert discharge in cfs to cms
    select(dateTime, discharge_cms)
}


##Get discharge data from NWIS
#Yahara River inlet to Lake Mendota
d.YRI <- readNWISuv("05427850", "00060", "2019-12-18", "2021-04-11", tz = "America/Chicago") %>%
  format()
#Sixmile Creek 
d.SMC <- readNWISuv("05427910", "00060", "2019-12-18", "2021-04-11", tz = "America/Chicago") %>%
  format()
#Dorn Creek
d.DC <- readNWISuv("05427930", "00060", "2019-12-18", "2021-04-11", tz = "America/Chicago") %>%
  format()
#Pheasant Branch Creek
d.PB <- readNWISuv("05427948", "00060", "2019-12-18", "2021-04-11", tz = "America/Chicago") %>%
  format()
#Yahara River outlet of Lake Mendota
d.YRO <- readNWISuv("05428500", "00060", "2019-12-18", "2021-04-11", tz = "America/Chicago") %>%
  format()
#Spring Harbor
d.SH <- readNWISuv("05427965", "00060", "2019-12-18", "2021-04-11", tz = "America/Chicago") %>%
  format()



#function to calculate moving average for discharge data
rolling_ave_discharge <- function(cond_data, discharge_data) {
  
  #join discharge with 30-minute conductivity to keep only the datetimes of discharge collection necessary
  d.x <- cond_data %>%
    mutate(dateTime = with_tz(dateTime, tzone = "America/Chicago")) %>%
    mutate(join_time = dateTime) %>%
    mutate(dateTime = dateTime) %>%
    mutate(check = dateTime) %>%
    data.table()
  
  d.y <- discharge_data %>%
    mutate(join_time = dateTime) %>%
    mutate(check = dateTime) %>%
    data.table()
  
  setkey(d.x, join_time)
  setkey(d.y, join_time)
  
  qsc <- d.y[d.x, roll = "nearest"]%>%
    drop_na(i.dateTime) 
  
  dis_data <- qsc %>%
    select(i.dateTime, discharge_cms) %>%
    rename(dateTime = i.dateTime) %>%
    mutate(MovingAverage_dis_cms = rollmean(discharge_cms, 13, fill = NA, na.rm = TRUE)) %>% #use zoo::rollmean over 13 rows (6 hours - 3 before and 3 after each point)
    mutate(MovingAverage_dis_cms = ifelse(row_number() <= 6, mean(discharge_cms[1:6]), MovingAverage_dis_cms)) %>% # rollmean leaves empty rows at beginning and end of dataset. This line and the one below uses the mean of those empty rows
    mutate(MovingAverage_dis_cms = ifelse(row_number() >= (nrow(qsc) - 5), mean(discharge_cms[(nrow(qsc) - 5):nrow(qsc)]), MovingAverage_dis_cms)) %>%
    mutate(MovingAverage_dis_cms = ifelse(MovingAverage_dis_cms <= 0, 0, MovingAverage_dis_cms))
    
  return(dis_data) #returns dataframe with dateTime, discharge_cms, MovingAverage_dis_cms
}


#get running mean for discharge and write data to .csv files
#Yahara River inlet to Lake Mendota
YRI_discharge <- rolling_ave_discharge(read_csv("Data/Conductivity/YR-I_cond.csv"), d.YRI)
write_csv(YRI_discharge, "Data/USGS_discharge/YR-I_discharge.csv")
#Sixmile Creek
SMC_discharge <- rolling_ave_discharge(read_csv("Data/Conductivity/SMC_cond.csv"), d.SMC)
write_csv(SMC_discharge, "Data/USGS_discharge/SMC_discharge.csv")
#Dorn Creek
DC_discharge <- rolling_ave_discharge(read_csv("Data/Conductivity/DC_cond.csv"), d.DC)
write_csv(DC_discharge, "Data/USGS_discharge/DC_discharge.csv")
#Pheasant Branch Creek
PB_discharge <- rolling_ave_discharge(read_csv("Data/Conductivity/PB_cond.csv"), d.PB)
write_csv(PB_discharge, "Data/USGS_discharge/PB_discharge.csv")
#Yahara River outlet of Lake Mendota
YRO_discharge <- rolling_ave_discharge(read_csv("Data/Conductivity/YR-O_cond.csv"), d.YRO)
write_csv(YRO_discharge, "Data/USGS_discharge/YR-O_discharge.csv")
#Spring Harbor
# import USGS conductivity
SH_cond = read_tsv('Data/USGS_SH/SH_SPC.txt') |> 
  rename(Temp_C = `158191_00010`, SpCond_uScm = `158192_00095`, 
         Discharge = `158188_00060`) |> 
  mutate(EC_lowRange_uScm = NA, EC_highRange_uScm = NA, ID = 'SH') |> 
  select(dateTime = datetime, EC_lowRange_uScm, EC_highRange_uScm,
         Temp_C, SpCond_uScm, ID)

#function to calculate moving average
mov_ave <- function(ConductivityData, span = 13) {
  #add column for 6 hour moving average and get the total average specific conductivity of the dataset
  Cond_data <- ConductivityData %>%
    mutate(MovingAverage_SpCond_uScm = rollmean(SpCond_uScm, span, fill = NA, na.rm = TRUE)) %>% #use zoo::rollmean over 13 rows (6 hours - 3 before and 3 after each point)
    mutate(MovingAverage_SpCond_uScm = ifelse(row_number() <= 6, mean(SpCond_uScm[1:6]), MovingAverage_SpCond_uScm)) %>% # rollmean leaves empty rows at beginning and end of dataset. This line and the one below uses the mean of those empty rows
    mutate(MovingAverage_SpCond_uScm = ifelse(row_number() >= (nrow(ConductivityData) - 5), mean(SpCond_uScm[(nrow(ConductivityData) - 5):nrow(ConductivityData)]), MovingAverage_SpCond_uScm))
}

#finalized datasets
SH_cond <- mov_ave(SH_cond) #Yahara River inflow

SH_discharge <- rolling_ave_discharge(SH_cond, d.SH)
write_csv(SH_discharge, "Data/USGS_discharge/SH_discharge.csv")
