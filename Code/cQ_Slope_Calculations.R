#script to calculate cQ Slopes
#slope datasets are saved under the Data folder and can be loaded via sourcing Data/cQ_slopes/call_cQslope_datasets.R

source("Code/Baseflow_Events_Separation.R")

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
 
