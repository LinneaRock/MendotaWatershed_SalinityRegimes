#script to separate storm events in hydrographs and chemographs

#call in chloride and discharge data:
source("Code/estimated_chloride_conc_mass.R")




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




##Create plots of chemographs and hydrographs####
source("Code/Functions/chemo_hydro_graph_grid_function.R")

chemo_hydro_graph(YRI_d, YRI_ts_mass, "Yahara River Inflow")
ggsave("Figures/Supplemental/FigureS2_YR-I_grid.png", width = 6.25, height = 5.5, units = "in")
chemo_hydro_graph(SMC_d, SMC_ts_mass, "Sixmile Creek")
ggsave("Figures/Supplemental/FigureS3_SMC_grid.png", width = 6.25, height = 5.5, units = "in")
chemo_hydro_graph(DC_d, DC_ts_mass, "Dorn Creek")
ggsave("Figures/Supplemental/FigureS4_DC_grid.png", width = 6.25, height = 5.5, units = "in")
chemo_hydro_graph(PB_d, PB_ts_mass, "Pheasant Branch Creek")
ggsave("Figures/Supplemental/FigureS5_PB_grid.png", width = 6.25, height = 5.5, units = "in")
chemo_hydro_graph(YRO_d, YRO_ts_mass, "Yahara River Outflow")
ggsave("Figures/Supplemental/FigureS6_YR-O_grid.png", width = 6.25, height = 5.5, units = "in")


##Full record cQ slopes####
source("Code/Functions/fullRecord_cQ_dataframe_function.R")

YRI_fullrecord <- fullRecord(YRI_events_bf, "YR-I")
SMC_fullrecord <- fullRecord(SMC_events_bf, "SMC")
DC_fullrecord <- fullRecord(DC_events_bf, "DC")
PB_fullrecord <- fullRecord(PB_events_bf, "PB")
YRO_fullrecord <- fullRecord(YRO_events_bf, "YR-O")

#bind all data together
all_full <- bind_rows(YRI_fullrecord, SMC_fullrecord, DC_fullrecord, PB_fullrecord, YRO_fullrecord)
#save dataset for easy loading later
saveRDS(all_full, "Data/cQ_slopes/fullRecord.rds")


##Baseflow cQ slopes####
source("Code/Functions/baseflow_cQ_dataframe_function.R")

YRI_baseflow <- baseflow(YRI_events_bf, "YR-I")
SMC_baseflow <- baseflow(SMC_events_bf, "SMC")
DC_baseflow <- baseflow(DC_events_bf, "DC")
PB_baseflow <- baseflow(PB_events_bf, "PB")
YRO_baseflow <- baseflow(YRO_events_bf, "YR-O")

#bind all data together
all_baseflow <- bind_rows(YRI_baseflow, SMC_baseflow, DC_baseflow, PB_baseflow, YRO_baseflow)
#save dataset for easy loading later
saveRDS(all_baseflow, "Data/cQ_slopes/baseflow.rds")


##individual stormflow cQ slopes####
source("Code/Functions/eachEvent_cQ_dataframe_function.R")

 YRI_events <- each_event_cq(YRI_events_bf, "YR-I")
 SMC_events <- each_event_cq(SMC_events_bf, "SMC")
 DC_events <- each_event_cq(DC_events_bf, "DC")
 PB_events <- each_event_cq(PB_events_bf, "PB")
 YRO_events <- each_event_cq(YRO_events_bf, "YR-O")

 #bind all data together
 all_individual_events <- bind_rows(YRI_events, SMC_events, DC_events, PB_events, YRO_events)
 #save dataset for easy loading later
 saveRDS(all_individual_events, "Data/cQ_slopes/individualEvents.rds")
 

 ##bulk stormflow cQ slopes####
 source("Code/Functions/bulkEvent_cQ_dataframe_function.R")
 
 YRI_bulkstorm <- bulk_event_cq(YRI_events_bf, "YR-I")
 SMC_bulkstorm <- bulk_event_cq(SMC_events_bf, "SMC")
 DC_bulkstorm <- bulk_event_cq(DC_events_bf, "DC")
 PB_bulkstorm <- bulk_event_cq(PB_events_bf, "PB")
 ##Before running the next line, the function in bulkEvent_cQ_dataframe_function.R needs to be ammended
 ##The outlet only has a few events that occur in Apr-Jun and Oct-Dec. Need to comment out the other 
 ##two seasons before running. Notes in the function for what to comment out and include. 
 YRO_bulkstorm <- bulk_event_cq(YRO_events_bf, "YR-O")
 
 #bind all data together 
 all_bulkstorm <- bind_rows(YRI_bulkstorm, SMC_bulkstorm, DC_bulkstorm, PB_bulkstorm, YRO_bulkstorm)
 #save dataset for easy loading later
 saveRDS(all_bulkstorm, "Data/cQ_slopes/bulkStormflow.rds")
 
