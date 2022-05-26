#script to create a df dedicated to looking at a few variables for running regressions - values reported in manuscript


#load in datasets needed to create tests df
source("Data/cQ_slopes/call_cQslope_datasets.R")
source("Code/Baseflow_Events_Separation.R")

tests <- data.frame(
  site = c("YR-I", "SMC", "DC", "PB", "YR-O", "SH"),
  median_chloride_mgL = c(median(YRI_ts_mass$chloride_estimated_mgL, na.rm = TRUE), median(SMC_ts_mass$chloride_estimated_mgL, na.rm = TRUE), median(DC_ts_mass$chloride_estimated_mgL, na.rm = TRUE), median(PB_ts_mass$chloride_estimated_mgL, na.rm = TRUE), median(YRO_ts_mass$chloride_estimated_mgL, na.rm = TRUE), median(SH_ts_mass$chloride_estimated_mgL, na.rm = TRUE)),
  total_chloride_mass_Mg = c(sum(YRI_ts_mass$chloride_mass_Mg, na.rm = TRUE),sum(SMC_ts_mass$chloride_mass_Mg, na.rm = TRUE),sum(DC_ts_mass$chloride_mass_Mg, na.rm = TRUE),sum(PB_ts_mass$chloride_mass_Mg, na.rm = TRUE), sum(YRO_ts_mass$chloride_mass_Mg, na.rm = TRUE), sum(SH_ts_mass$chloride_mass_Mg, na.rm = TRUE)),
  mass_chloride_baseflow_Mg = c(sum(YRI_events_bf$bf_chloride_Mg, na.rm = TRUE),sum(SMC_events_bf$bf_chloride_Mg, na.rm = TRUE),sum(DC_events_bf$bf_chloride_Mg, na.rm = TRUE),sum(PB_events_bf$bf_chloride_Mg, na.rm = TRUE), sum(YRO_events_bf$bf_chloride_Mg, na.rm = TRUE), sum(SH_events_bf$bf_chloride_Mg, na.rm = TRUE)),
  mass_chloride_events_Mg = c(sum(YRI_events_bf$event_chloride_Mg, na.rm = TRUE),sum(SMC_events_bf$event_chloride_Mg, na.rm = TRUE),sum(DC_events_bf$event_chloride_Mg, na.rm = TRUE),sum(PB_events_bf$event_chloride_Mg, na.rm = TRUE), sum(YRO_events_bf$event_chloride_Mg, na.rm = TRUE), sum(SH_events_bf$event_chloride_Mg, na.rm = TRUE)),
  development = c(14.69, 12.12, 7.55, 38.46, 20.45, 81.50), #percentage developed land in subwatershed
  agriculture = c(75.66, 77.84, 83.84, 55.7, 63.32, 5.10), #percentage agricultural land in subwatershed
  road_denisty_mha = c(32.41, 42.04, 24.18, 112.8, 57.84, 305.83), #road density in subwatershed (m/ha)
  watershed_size_ha = c(29408.4, 12532.03, 3270.64, 4750.4, 60805.84, 585.78), #waterhsed area (ha)
  BFI = c(42, 82, 77, 49, 76, 20), #BFI from cQ_Slope_Calculations.R
  Ave_discharge_cms = c(mean(YRI_discharge$MovingAverage_dis_cms, na.rm = TRUE),mean(SMC_discharge$MovingAverage_dis_cms, na.rm = TRUE),mean(DC_discharge$MovingAverage_dis_cms, na.rm = TRUE),mean(PB_discharge$MovingAverage_dis_cms, na.rm = TRUE), mean(YRO_discharge$MovingAverage_dis_cms, na.rm = TRUE), mean(SH_discharge$MovingAverage_dis_cms, na.rm = TRUE))
)

median(SW_cl$chloride_mgL, na.rm = TRUE)
mean(SW_cl$chloride_mgL, na.rm = TRUE)
sd(SW_cl$chloride_mgL, na.rm = TRUE)

mean(SH_ts_mass$chloride_estimated_mgL, na.rm = TRUE)
sd(SH_ts_mass$chloride_estimated_mgL, na.rm = TRUE)

tests <- tests %>%
  mutate(bf_mass_percent = (mass_chloride_baseflow_Mg/total_chloride_mass_Mg) * 100) %>% #percentage of chloride mass from baseflow
  mutate(event_mass_percent = (mass_chloride_events_Mg/total_chloride_mass_Mg)*100) %>% #percentage of chloride mass from stormflow
  mutate(yield = total_chloride_mass_Mg/watershed_size_ha) #watershed size-normalized yield of chloride mass 


plot(median_chloride_mgL~development, tests)

summary(lm(median_chloride_mgL~development, tests)) #p = 6.842e-05; r = 0.9832
summary(lm(median_chloride_mgL~road_denisty_mha, tests)) #p = 9.097e-06, r =  0.9938 
summary(lm(total_chloride_mass_Mg ~watershed_size_ha, tests)) #p = 2.645e-06, r =0.9967
summary(lm(yield~development, tests)) #p =  0.001582, r =  0.9197
summary(lm(yield~road_denisty_mha, tests))#p = 0.0002223, r = 0.9697
summary(lm(total_chloride_mass_Mg~Ave_discharge_cms, tests)) #p =8.896e-05, r =  0.9808

summary(lm(total_chloride_mass_Mg ~ development, tests)) #p = 0.5304, r = -0.11
summary(lm(total_chloride_mass_Mg ~ road_denisty_mha, tests)) #p = 0.4867, r = -0.09




             
             

        