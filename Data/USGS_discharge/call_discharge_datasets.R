#call discharge datasets in


#Yahara River inlet to Lake Mendota
YRI_discharge <- read_csv("Data/USGS_discharge/YR-I_discharge.csv") %>%
  mutate(dateTime = with_tz(dateTime, tzone = "America/Chicago"))
#Sixmile Creek
SMC_discharge <- read_csv("Data/USGS_discharge/SMC_discharge.csv") %>%
  mutate(dateTime = with_tz(dateTime, tzone = "America/Chicago"))
#Dorn Creek
DC_discharge <- read_csv("Data/USGS_discharge/DC_discharge.csv") %>%
  mutate(dateTime = with_tz(dateTime, tzone = "America/Chicago"))
#Pheasant Branch Creek
PB_discharge <- read_csv("Data/USGS_discharge/PB_discharge.csv") %>%
  mutate(dateTime = with_tz(dateTime, tzone = "America/Chicago"))
#Yahara River outlet of Lake Mendota
YRO_discharge <- read_csv("Data/USGS_discharge/YR-O_discharge.csv") %>%
  mutate(dateTime = with_tz(dateTime, tzone = "America/Chicago"))