#script to calculate cQ Slopes
#slope datasets are saved under the Data folder and can be loaded via sourcing Data/cQ_slopes/call_cQslope_datasets.R
source("Code/Functions/cQslopes_function.R")

#### #### Full record cQ slopes #### #### 
YRI_fullrecord <- cQslopes_function(YRI_events_bf, "YR-I", use = 'full')
SMC_fullrecord <- cQslopes_function(SMC_events_bf, "SMC", use = 'full')
DC_fullrecord <- cQslopes_function(DC_events_bf, "DC", use = 'full')
PB_fullrecord <- cQslopes_function(PB_events_bf, "PB", use = 'full')
YRO_fullrecord <- cQslopes_function(YRO_events_bf, "YR-O", use = 'full')
SH_fullrecord <- cQslopes_function(SH_events_bf, "SH", use = 'full')

#bind all data together
all_full <- bind_rows(YRI_fullrecord, SMC_fullrecord, DC_fullrecord, PB_fullrecord, YRO_fullrecord, SH_fullrecord)
#save dataset for easy loading later
saveRDS(all_full, "Data/cQ_slopes/fullRecord.rds")

#### #### Baseflow cQ slopes #### #### 
YRI_baseflow <- cQslopes_function(YRI_events_bf, "YR-I", use = 'baseflow')
SMC_baseflow <- cQslopes_function(SMC_events_bf, "SMC", use = 'baseflow')
DC_baseflow <- cQslopes_function(DC_events_bf, "DC", use = 'baseflow')
PB_baseflow <- cQslopes_function(PB_events_bf, "PB", use = 'baseflow')
YRO_baseflow <- cQslopes_function(YRO_events_bf, "YR-O", use = 'baseflow')
SH_baseflow <- cQslopes_function(SH_events_bf, "SH", use = 'baseflow')

#bind all data together
all_baseflow <- bind_rows(YRI_baseflow, SMC_baseflow, DC_baseflow, PB_baseflow, YRO_baseflow, SH_baseflow)
#save dataset for easy loading later
saveRDS(all_baseflow, "Data/cQ_slopes/baseflow.rds")

#### #### bulk stormflow cQ slopes #### #### 
YRI_bulkstorm <- cQslopes_function(YRI_events_bf, "YR-I", use = 'bulk')
SMC_bulkstorm <- cQslopes_function(SMC_events_bf, "SMC", use = 'bulk')
DC_bulkstorm <- cQslopes_function(DC_events_bf, "DC", use = 'bulk')
PB_bulkstorm <- cQslopes_function(PB_events_bf, "PB", use = 'bulk')
SH_bulkstorm <- cQslopes_function(SH_events_bf, "SH", use = 'bulk')
##Use the amended function in bulkEvent_cQ_dataframe_function.R for YRO
##The outlet only has a few events that occur in Apr-Jun and Oct-Dec. Need to comment out the other 
##two seasons. Notes in the function for what to comment out and include. 
YRO_bulkstorm <- bulk_event_cq_special_YRO(YRO_events_bf, "YR-O")

#bind all data together 
all_bulkstorm <- bind_rows(YRI_bulkstorm, SMC_bulkstorm, DC_bulkstorm, PB_bulkstorm, YRO_bulkstorm, SH_bulkstorm)
#save dataset for easy loading later
saveRDS(all_bulkstorm, "Data/cQ_slopes/bulkStormflow.rds")
 

#### #### individual stormflow cQ slopes #### #### 
source("Code/Functions/eachEvent_cQ_dataframe_function.R")

YRI_events <- each_event_cq(events_bf = YRI_events_bf, name =  "YR-I")
SMC_events <- each_event_cq(SMC_events_bf, "SMC")
DC_events <- each_event_cq(DC_events_bf, "DC")
PB_events <- each_event_cq(PB_events_bf, "PB")
YRO_events <- each_event_cq(YRO_events_bf, "YR-O")
SH_events <- each_event_cq(SH_events_bf, "SH")

#bind all data together
all_individual_events <- bind_rows(YRI_events, SMC_events, DC_events, PB_events, YRO_events, SH_events)
#save dataset for easy loading later
saveRDS(all_individual_events, "Data/cQ_slopes/individualEvents.rds")

# Negative cQ slopes ($<$ -0.05) implied chemodynamic dilution behavior, 
# as concentration decreases with increasing discharge, and positive cQ slopes ($>$ 0.05) 
# implied chemodynamic mobilization behavior
