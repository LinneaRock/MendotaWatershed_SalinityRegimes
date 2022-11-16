#script to separate storm events in hydrographs and chemographs and create graphs


#call in chloride and discharge data:
source("Code/DataLoad/Estimated_Chloride_Conc_Mass.R") 

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

#filter out NA and anything below 0 cms
SH_d <- SH_discharge %>%
  filter(!is.na(MovingAverage_dis_cms)) %>%
  filter(MovingAverage_dis_cms >= 0)
#calculate eckhardt
SH_d <- get_eckhardt_bf("05427965", SH_d)
calc_bfi(SH_d) #bfi = 20%


##function to create plots of chemographs and hydrographs####
source("Code/Functions/chemo_hydro_graph_grid_function.R")

chemo_hydro_graph(DC_d, DC_ts_mass, "Dorn Creek")
ggsave("Figures/Supplemental/FigureS5_DC_grid.png", width = 6.25, height = 5.5, units = "in", dpi = 500)

chemo_hydro_graph(PB_d, PB_ts_mass, "Pheasant Branch Creek")
ggsave("Figures/Supplemental/FigureS6_PB_grid.png", width = 6.25, height = 5.5, units = "in", dpi = 500)

chemo_hydro_graph(SH_d, SH_ts_mass, "Spring Harbor")
ggsave("Figures/Supplemental/FigureS7_SH_grid.png", width = 6.25, height = 5.5, units = "in", dpi = 500)

chemo_hydro_graph(SMC_d, SMC_ts_mass, "Sixmile Creek")
ggsave("Figures/Supplemental/FigureS8_SMC_grid.png", width = 6.25, height = 5.5, units = "in", dpi = 500)

chemo_hydro_graph(YRI_d, YRI_ts_mass, "Yahara River Inflow")
ggsave("Figures/Supplemental/FigureS9_YR-I_grid.png", width = 6.25, height = 5.5, units = "in", dpi = 500)

chemo_hydro_graph(YRO_d, YRO_ts_mass, "Yahara River Outflow")
ggsave("Figures/Supplemental/FigureS10_YR-O_grid.png", width = 6.25, height = 5.5, units = "in", dpi = 500)

