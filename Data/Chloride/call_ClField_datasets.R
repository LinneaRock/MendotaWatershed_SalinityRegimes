library(tidyverse)
library(lubridate)
#script to call in grab sampling chloride and conductivity data (field data)

YRI_cl <- read_csv("Data/Chloride/YR-I_cl.csv") %>% #Yahara River inflow 
  force_tz(dateTime, tzone = "America/Chicago")%>%
  mutate(ID = "YR-I")
SMC_cl <- read_csv("Data/Chloride/SMC_cl.csv") %>% #Sixmile Creek
  force_tz(dateTime, tzone = "America/Chicago")
DC_cl <- read_csv("Data/Chloride/DC_cl.csv") %>% #Dorn Creek
  force_tz(dateTime, tzone = "America/Chicago")
PB_cl <- read_csv("Data/Chloride/PB_cl.csv") %>% #Pheasant Branch Creek
  force_tz(dateTime, tzone = "America/Chicago")%>%
  mutate(ID = "PB")
YRO_cl <- read_csv("Data/Chloride/YR-O_cl.csv") %>% #Yahara River outflow
  force_tz(dateTime, tzone = "America/Chicago")%>%
  mutate(ID = "YR-O")

YRS_cl <- read_csv("Data/Chloride/YS_cl.csv") %>% #Yahara River outflow from Monona 
  force_tz(dateTime, tzone = "America/Chicago")%>%
  mutate(ID = "YR-S")
SW_cl <- read_csv("Data/Chloride/SW_cl.csv") %>% #Starkweather Creek
  force_tz(dateTime, tzone = "America/Chicago")%>%
  mutate(ID = "SW")
ME_cl <- read_csv("Data/Chloride/ME_cl.csv") %>% #Lake Mendota
  force_tz(dateTime, tzone = "America/Chicago")%>%
  mutate(ID = "ME")
MO_cl <- read_csv("Data/Chloride/MO_cl.csv") %>% #Lake Monona 
  force_tz(dateTime, tzone = "America/Chicago")%>%
  mutate(ID = "MO")
