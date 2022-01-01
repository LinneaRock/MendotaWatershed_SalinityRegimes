#script to calculate and plot z-score of flow-normalized chloride concentration

#load datasets with chloride timeseries
source("Code/estimated_chloride_conc_mass.R")
#load function to flow-normalize and calculate z-score
source("Code/Functions/flow_normalize_zscore_function.R")

# #flow-normalized chloride in each river binded to a single dataframe
# All_flow_norm <- flow_normalize(YRI_ts_mass, "YR-I") %>%
#   bind_rows(flow_normalize(DC_ts_mass, "DC"),
#             flow_normalize(SMC_ts_mass, "SMC"),
#             flow_normalize(PB_ts_mass, "PB"),
#             flow_normalize(YRO_ts_mass, "YR-O"))

#bind river conductivity data together
all_river_cond <- bind_rows(YRI_cond, PB_cond, DC_cond, SMC_cond, YRO_cond, 
                              SW_cond, YRS_cond) 

#bind chloride data together
all_river_cl <- bind_rows(YRI_cl, PB_cl, DC_cl, SMC_cl, YRO_cl, 
                            SW_cl, YRS_cl)

#create the z-score plot
library(colorblindr)
library(scales)

p1 = ggplot(all_river_cond) +
  geom_line(mapping = aes(dateTime, MovingAverage_SpCond_uScm, 
                                          group = ID, color = ID)) +
  scale_color_OkabeIto() +
  theme_minimal(base_size = 8) + theme(legend.title = element_blank()) +
  scale_x_datetime(date_breaks = 'month', labels = date_format("%b")) +
  labs(y = "SpC"~(µS~cm^-1), x = "") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        legend.position = 'none')

# Same plot zoomed in on y-axis without SW and PB
p2 = ggplot(all_river_cond |> filter(!ID %in% c('SW', 'PB'))) +
  geom_line(mapping = aes(dateTime, MovingAverage_SpCond_uScm, 
                                          group = ID, color = ID)) +
  scale_color_OkabeIto(order = c(1,3,5,6,7)) +
  theme_minimal(base_size = 8) + theme(legend.title = element_blank()) +
  scale_x_datetime(date_breaks = 'month', labels = date_format("%b")) +
  labs(y = "SpC"~(µS~cm^-1), x = "") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        legend.position = 'none')

p3 = ggplot(all_river_cl) +
  geom_point(mapping = aes(dateTime, chloride_mgL, 
                          group = ID, color = ID)) +
  geom_path(mapping = aes(dateTime, chloride_mgL, 
                           group = ID, color = ID), size = 0.2, linetype = 2) +
  scale_color_OkabeIto() +
  theme_minimal(base_size = 8) + theme(legend.title = element_blank()) +
  scale_x_datetime(date_breaks = 'month', labels = date_format("%b")) +
  labs(y = "Chloride"~(mg~L^-1), x = "") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1), 
        legend.position = 'bottom', 
        legend.title=element_blank(),
        legend.margin = margin(0, 0, 0, 0)) +
  guides(colour = guide_legend(nrow = 1))

p1 / p2 / p3 +
  plot_annotation(tag_levels = 'a', tag_suffix = ')') & 
  theme(plot.tag = element_text(size = 8))
# Save combo plot
ggsave('Figures/FX_timeseries.png', width = 6.5, height = 6, units = 'in', dpi = 500)

# ggplot() +
#   geom_vline(aes(xintercept = as.Date('2020-03-22')), linetype = 2) +
#   geom_vline(aes(xintercept = as.Date('2021-03-20')), linetype = 2) +
#   geom_line(All_flow_norm %>% filter(ID != "YR-O"), mapping = aes(date, zscore_chloride, group = ID, color = ID)) +
#   geom_line(All_flow_norm %>% filter(ID == "YR-O"), mapping = aes(date, zscore_chloride, group = ID, color = ID), size = 1.25) +
#   scale_color_OkabeIto() +
#   #scale_color_manual(values = wes_palette("Darjeeling1", n = 5, type = "discrete")) +
#   theme_minimal() + theme(legend.title = element_blank()) +
#   scale_x_date(date_breaks = 'month', labels = date_format("%b")) +
#   labs(y = "Z-score of flow-normalized chloride", x = "") +
#   theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

# ggsave("Figures/F2_flow_norm_cl_zscore.png", width = 6.25, height = 4.25, units = "in", dpi = 500)
  

