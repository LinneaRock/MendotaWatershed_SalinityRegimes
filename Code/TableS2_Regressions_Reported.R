#script to create a df dedicated to looking at a few variables for running regressions - values reported in manuscript


#load in datasets needed to create tests df
source("Data/cQ_slopes/call_cQslope_datasets.R")

tests <- data.frame(
  site = c("YR-I", "SMC", "DC", "PB", "YR-O", "SH"),
  median_chloride_mgL = c(median(YRI_ts_mass$chloride_estimated_mgL, na.rm = TRUE), median(SMC_ts_mass$chloride_estimated_mgL, na.rm = TRUE), median(DC_ts_mass$chloride_estimated_mgL, na.rm = TRUE), median(PB_ts_mass$chloride_estimated_mgL, na.rm = TRUE), median(YRO_ts_mass$chloride_estimated_mgL, na.rm = TRUE), median(SH_ts_mass$chloride_estimated_mgL, na.rm = TRUE)),
  total_chloride_mass_Mg = c(sum(YRI_ts_mass$chloride_mass_Mg, na.rm = TRUE),sum(SMC_ts_mass$chloride_mass_Mg, na.rm = TRUE),sum(DC_ts_mass$chloride_mass_Mg, na.rm = TRUE),sum(PB_ts_mass$chloride_mass_Mg, na.rm = TRUE), sum(YRO_ts_mass$chloride_mass_Mg, na.rm = TRUE), sum(SH_ts_mass$chloride_mass_Mg, na.rm = TRUE)),
  mass_chloride_baseflow_Mg = c(sum(YRI_events_bf$bf_chloride_Mg, na.rm = TRUE),sum(SMC_events_bf$bf_chloride_Mg, na.rm = TRUE),sum(DC_events_bf$bf_chloride_Mg, na.rm = TRUE),sum(PB_events_bf$bf_chloride_Mg, na.rm = TRUE), sum(YRO_events_bf$bf_chloride_Mg, na.rm = TRUE), sum(SH_events_bf$bf_chloride_Mg, na.rm = TRUE)),
  mass_chloride_events_Mg = c(sum(YRI_events_bf$event_chloride_Mg, na.rm = TRUE),sum(SMC_events_bf$event_chloride_Mg, na.rm = TRUE),sum(DC_events_bf$event_chloride_Mg, na.rm = TRUE),sum(PB_events_bf$event_chloride_Mg, na.rm = TRUE), sum(YRO_events_bf$event_chloride_Mg, na.rm = TRUE), sum(SH_events_bf$event_chloride_Mg, na.rm = TRUE)),
  development = c(17.09, 15.91, 9.39, 41.03, 22.95, 86.30), #percentage developed land in subwatershed
  agriculture = c(71.68, 73.99, 82.80 , 53.34, 60.22, 4.09), #percentage agricultural land in subwatershed
  road_denisty_mha = c(34.61, 28.82, 20.30, 56.64, 38.01, 106.71), #road density in subwatershed (m/ha)
  watershed_size_ha = c(29405.99, 12530.32, 3269.92, 4750.24, 60800.41, 912.15), #waterhsed area (ha)
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

summary(lm(median_chloride_mgL ~ development, tests)) #p = 6.842e-05; r = 0.9898
summary(lm(median_chloride_mgL ~ road_denisty_mha, tests)) #p = 9.097e-06, r =  0.9771 

summary(lm(total_chloride_mass_Mg ~ watershed_size_ha, tests)) #p = 2.645e-06, r =0.9971
summary(lm(total_chloride_mass_Mg ~ Ave_discharge_cms, tests)) #p =8.896e-05, r =  0.9812
summary(lm(total_chloride_mass_Mg ~ development, tests)) #p = 0.5207, r = -0.11
summary(lm(total_chloride_mass_Mg ~ road_denisty_mha, tests)) #p = 0.5618, r = -0.1366


summary(lm(yield ~ development, tests)) #p =  0.001582, r =  0.9945
summary(lm(yield ~ road_denisty_mha, tests))#p = 5.844e-05, r = 0.9844




             
             

        