#script calls in conductivity data, sets local timezone, and adds moving average of specific conductivity


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

YRS_cond <- read_csv("Data/Conductivity/YS_cond.csv") %>% 
  force_tz(dateTime, tzone = "America/Chicago")%>%
  mutate(ID = "YR-S")
SW_cond <- read_csv("Data/Conductivity/SW_cond.csv") %>% 
  force_tz(dateTime, tzone = "America/Chicago")%>%
  mutate(ID = "SW")
ME_cond <- read_csv("Data/Conductivity/ME_cond.csv") %>% 
  force_tz(dateTime, tzone = "America/Chicago")%>%
  mutate(ID = "ME")
MO_cond <- read_csv("Data/Conductivity/MO_cond.csv") %>% 
  force_tz(dateTime, tzone = "America/Chicago")%>%
  mutate(ID = "MO")

#function to calculate moving average
mov_ave <- function(ConductivityData) {
  #add column for 6 hour moving average and get the total average specific conductivity of the dataset
  Cond_data <- ConductivityData %>%
    mutate(MovingAverage_SpCond_uScm = rollmean(SpCond_uScm, 13, fill = NA, na.rm = TRUE)) %>% #use zoo::rollmean over 13 rows (6 hours - 3 before and 3 after each point)
    mutate(MovingAverage_SpCond_uScm = ifelse(row_number() <= 6, mean(SpCond_uScm[1:6]), MovingAverage_SpCond_uScm)) %>% # rollmean leaves empty rows at beginning and end of dataset. This line and the one below uses the mean of those empty rows
    mutate(MovingAverage_SpCond_uScm = ifelse(row_number() >= (nrow(ConductivityData) - 5), mean(SpCond_uScm[(nrow(ConductivityData) - 5):nrow(ConductivityData)]), MovingAverage_SpCond_uScm))
  

}

#finalized datasets
YRI_cond <- mov_ave(YRI_cond) #Yahara River inflow
SMC_cond <- mov_ave(SMC_cond) #Sixmile Creek
DC_cond <- mov_ave(DC_cond) #Dorn Creek
PB_cond <- mov_ave(PB_cond) #Pheasant Branch Creek
YRO_cond <- mov_ave(YRO_cond) #Yahara River outlfow
YRS_cond <- mov_ave(YRS_cond) #Yahara River outflow
SW_cond <- mov_ave(SW_cond) #Starkweather
ME_cond <- mov_ave(ME_cond) #Mendota
MO_cond <- mov_ave(MO_cond) #Monona

