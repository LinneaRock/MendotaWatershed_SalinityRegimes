#script to separate storm events in discharge and chloride timeseries data

#call in chloride and discharge data:
source("Code/Estimated_Chloride_Conc_Mass.R") 


##Get eckhardt baseflow for each river using GlobalBaseflow code from Zipper (2018)####
source("Code/Functions/baseflow_separation_functions_Zipper2018.R")

#filter out NA and anything below 0 cms
YRI_d <- YRI_discharge %>%
  filter(!is.na(MovingAverage_dis_cms)) %>%
  filter(MovingAverage_dis_cms >= 0)
#calculate eckhardt
YRI_d <- get_eckhardt_bf("05427850", YRI_d)
calc_bfi(YRI_d) #bfi = 42%

#filter out NA and anything below 0 cms
SMC_d <- SMC_discharge %>%
  filter(!is.na(MovingAverage_dis_cms)) %>%
  filter(MovingAverage_dis_cms >= 0)
#calculate eckhardt
SMC_d <- get_eckhardt_bf("05427910", SMC_d)
calc_bfi(SMC_d) #bfi = 83%

#filter out NA and anything below 0 cms
DC_d <- DC_discharge %>%
  filter(!is.na(MovingAverage_dis_cms)) %>%
  filter(MovingAverage_dis_cms >= 0)
#calculate eckhardt
DC_d <- get_eckhardt_bf("05427930", DC_d)
calc_bfi(DC_d) #bfi = 77%

#filter out NA and anything below 0 cms
PB_d <- PB_discharge %>%
  filter(!is.na(MovingAverage_dis_cms)) %>%
  filter(MovingAverage_dis_cms >= 0)
#calculate eckhardt
PB_d <- get_eckhardt_bf("05427948", PB_d)
calc_bfi(PB_d) #bfi = 49%

#filter out NA and anything below 0 cms
YRO_d <- YRO_discharge %>%
  filter(!is.na(MovingAverage_dis_cms)) %>%
  filter(MovingAverage_dis_cms >= 0)
#calculate eckhardt
YRO_d <- get_eckhardt_bf("05428500", YRO_d)
calc_bfi(YRO_d) #bfi = 76%




##Identify stormflow events using EventPicker code from Gorski (2020)####
source("Code/Functions/EventPicker_functions_Gorski2020.R")

YRI_events_bf <- find.peaks(YRI_d, YRI_ts_mass)
SMC_events_bf <- find.peaks(SMC_d, SMC_ts_mass)
DC_events_bf <- find.peaks(DC_d, DC_ts_mass)
PB_events_bf <- find.peaks(PB_d, PB_ts_mass)
YRO_events_bf <- find.peaks(YRO_d, YRO_ts_mass)

