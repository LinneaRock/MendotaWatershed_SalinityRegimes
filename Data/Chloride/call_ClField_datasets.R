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
  force_tz(dateTime, tzone = "America/Chicago")
YRO_cl <- read_csv("Data/Chloride/YR-O_cl.csv") %>% #Yahara River outlfow
  force_tz(dateTime, tzone = "America/Chicago")%>%
  mutate(ID = "YR-O")


