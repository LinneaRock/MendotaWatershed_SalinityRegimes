#Script to get estimated chloride concentrations and loading 

#load field data, logger data, and discharge data
source("Data/Chloride/call_Clfield_datasets.R")
source("Data/Conductivity/call_cond_datasets.R")
source("Data/USGS_discharge/call_discharge_datasets.R")

#load functions
source("Code/Functions/estimate_chloride_function.R")


#run function to get chloride timeseries
#Yahara River inflow
YRI_ts_mass <- chloride_ts_mass(YRI_cl, YRI_cond, YRI_discharge)
#Sixmile Creek
SMC_ts_mass <- chloride_ts_mass(SMC_cl, SMC_cond, SMC_discharge)
#Dorn Creek
DC_ts_mass <- chloride_ts_mass(DC_cl, DC_cond, DC_discharge)
#Pheasant Branch Creek
PB_ts_mass <- chloride_ts_mass(PB_cl, PB_cond, PB_discharge)
#Yahara River outflow
YRO_ts_mass <- chloride_ts_mass(YRO_cl, YRO_cond, YRO_discharge)

all_ts_mass <- bind_rows(YRI_ts_mass, SMC_ts_mass, DC_ts_mass, PB_ts_mass, YRO_ts_mass)

#plot to look at chloride concentration timeseries
library(wesanderson)

ggplot(all_ts_mass) +
  geom_line(aes(dateTime, chloride_estimated_mgL, color = ID)) +
  facet_wrap(~ID, scales = "free_y")  +
  theme_minimal() + theme(legend.position = "none") +
  labs(x = "",
       y = "Chloride Concentration"~(mg~L^-1)) +
  scale_color_manual(values = wes_palette("Darjeeling1", n = 5, type = "discrete"))
  
  
