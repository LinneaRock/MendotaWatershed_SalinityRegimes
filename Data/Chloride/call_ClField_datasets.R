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


# import USGS conductivity
SH_cond = read_tsv('Data/USGS_SH/SH_SPC.txt') |> 
  rename(Temp_C = `158191_00010`, SpCond_uScm = `158192_00095`, 
         Discharge = `158188_00060`) |> 
  mutate(EC_lowRange_uScm = NA, EC_highRange_uScm = NA, ID = 'SH') |> 
  dplyr::select(dateTime = datetime, EC_lowRange_uScm, EC_highRange_uScm,
         Temp_C, SpCond_uScm, ID)

# import USGS chloride
SH_cl = read_tsv('Data/USGS_SH/SH_Cl.txt') %>% 
  dplyr::select(sample_dt, sample_tm, chloride_mgL = p00940) %>% 
  mutate(datetime = ymd_hms(paste(sample_dt,sample_tm))) %>% 
  mutate(dateTime = lubridate::round_date(datetime, "15 minutes")) |> 
  # mutate(ID = 'SH', EC_uScm = NA, Temp_C = NA, SpCond_uScm = NA) |>
  dplyr::select(dateTime, chloride_mgL) |> 
  left_join(SH_cond) |> 
  dplyr::select(dateTime, chloride_mgL, EC_uScm = EC_highRange_uScm, Temp_C, SpCond_uScm,  ID) |> 
  filter(!is.na(chloride_mgL))


