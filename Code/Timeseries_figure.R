#script to create timeseries figure

#load datasets with chloride timeseries
source("Code/estimated_chloride_conc_mass.R")


#bind river conductivity data together
all_river_cond <- bind_rows(YRI_cond, PB_cond, DC_cond, SMC_cond, YRO_cond, 
                              SW_cond, YRS_cond) 
all_river_cond$ID = factor(all_river_cond$ID,
                               levels = c("DC", "PB", "SMC", "YR-I", "YR-O", "SW", "YR-S"))

#bind chloride data together
all_river_cl <- bind_rows(YRI_cl, PB_cl, DC_cl, SMC_cl, YRO_cl, 
                            SW_cl, YRS_cl)
all_river_cl$ID = factor(all_river_cl$ID,
                           levels = c("DC", "PB", "SMC", "YR-I", "YR-O", "SW", "YR-S"))


library(colorblindr)
library(scales)
library(patchwork)

p1 = ggplot(all_river_cond) +
  geom_line(mapping = aes(dateTime, MovingAverage_SpCond_uScm, 
                                          group = ID, color = ID)) +
  scale_color_OkabeIto(order = c(1:2,4:8)) +
  theme_minimal(base_size = 8) + theme(legend.title = element_blank()) +
  scale_x_datetime(date_breaks = 'month', labels = date_format("%b")) +
  labs(y = "SpC"~(µS~cm^-1), x = "") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        legend.position = 'none')

# Same plot zoomed in on y-axis without SW and PB
p2 = ggplot(all_river_cond |> filter(!ID %in% c('SW', 'PB'))) +
  geom_line(mapping = aes(dateTime, MovingAverage_SpCond_uScm, 
                                          group = ID, color = ID)) +
  scale_color_OkabeIto(order = c(1,4,5,6,8)) +
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
  scale_color_OkabeIto(order = c(1:2,4:8)) +
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
ggsave('Figures/Supplemental/FigureS1_timeseries.png', width = 6.5, height = 6, units = 'in', dpi = 500)

  
#######################same as above figures for SH#############################
SH_cl_estimated <- SH_ts_mass %>%
  rename(chloride_mgL = chloride_estimated_mgL) %>%
  filter(dateTime <= max(DC_cl$dateTime)) %>%
  select(dateTime, chloride_mgL, ID) %>%
  group_by(as.Date(dateTime), ID) %>%
  mutate(chloride_mgL = mean(chloride_mgL)) %>%
  ungroup() %>%
  dplyr::select(-dateTime) %>%
  mutate(dateTime = as.POSIXct(`as.Date(dateTime)`)) %>%
  dplyr::select(-`as.Date(dateTime)`) %>%
  distinct()

# SpC
p1 = ggplot(SH_cond |> filter(dateTime >= as.Date("2019-12-19") &
                                dateTime < as.Date("2021-04-02"))) +
  geom_line(mapping = aes(dateTime, MovingAverage_SpCond_uScm, 
                          group = ID, color = ID)) +
  scale_color_OkabeIto(order = 3) +
  theme_minimal(base_size = 8) + theme(legend.title = element_blank()) +
  scale_x_datetime(date_breaks = 'month', labels = date_format("%b")) +
  labs(y = "SpC"~(µS~cm^-1), x = "") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        legend.position = 'none')

#chloride
p2 = ggplot(SH_cl_estimated) +
  geom_point(mapping = aes(dateTime, chloride_mgL, 
                           group = ID, color = ID)) +
  geom_path(mapping = aes(dateTime, chloride_mgL, 
                          group = ID, color = ID), size = 0.2, linetype = 2) +
  scale_color_OkabeIto(order = 3) +
  theme_minimal(base_size = 8) + theme(legend.title = element_blank()) +
  scale_x_datetime(date_breaks = 'month', labels = date_format("%b")) +
  labs(y = "Chloride"~(mg~L^-1), x = "") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1), 
        legend.position = 'bottom', 
        legend.title=element_blank(),
        legend.margin = margin(0, 0, 0, 0)) +
  guides(colour = guide_legend(nrow = 1))

p1 / p2  +
  plot_annotation(tag_levels = 'a', tag_suffix = ')') & 
  theme(plot.tag = element_text(size = 8))
# Save combo plot
ggsave('Figures/Supplemental/FigureS2_timeseries_SH.png', width = 6.5, height = 6, units = 'in', dpi = 500)














##################figures including SH###################################################

#bind river conductivity data together
all_river_cond <- bind_rows(YRI_cond, PB_cond, DC_cond, SMC_cond, YRO_cond, 
                            SW_cond, YRS_cond, SH_cond) %>%
  filter(dateTime >= as.Date("2019-12-19") &
           dateTime < as.Date("2021-04-02"))
all_river_cond$ID = factor(all_river_cond$ID,
                           levels = c("DC", "PB","SH", "SMC", "YR-I", "YR-O", "SW", "YR-S"))

#bind chloride data together
SH_cl_estimated <- SH_ts_mass %>%
  rename(chloride_mgL = chloride_estimated_mgL) %>%
  filter(dateTime <= max(DC_cl$dateTime)) %>%
  select(dateTime, chloride_mgL, ID) %>%
  group_by(as.Date(dateTime), ID) %>%
  mutate(chloride_mgL = mean(chloride_mgL)) %>%
  dplyr::select(-dateTime) %>%
  rename(dateTime = `as.Date(dateTime)`) %>%
  distinct()

all_river_cl <- bind_rows(YRI_cl, PB_cl, DC_cl, SMC_cl, YRO_cl, 
                          SW_cl, YRS_cl, SH_cl_estimated)
all_river_cl$ID = factor(all_river_cl$ID,
                         levels = c("DC", "PB", "SH", "SMC", "YR-I", "YR-O", "SW", "YR-S"))



library(colorblindr)
library(scales)
library(patchwok)

#SpC in urban tribs
p1 = ggplot(all_river_cond |> filter(ID %in% c('SW', 'PB', 'SH'))) +
  geom_line(mapping = aes(dateTime, MovingAverage_SpCond_uScm, 
                          group = ID, color = ID)) +
  scale_color_OkabeIto(order = c(2,3,7)) +
  theme_minimal(base_size = 8) + theme(legend.title = element_blank()) +
  scale_x_datetime(date_breaks = 'month', labels = date_format("%b")) +
  labs(y = "SpC"~(µS~cm^-1), x = "") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        legend.position = 'none')

# SpC in ag tribs
p2 = ggplot(all_river_cond |> filter(!ID %in% c('SW', 'PB', 'SH'))) +
  geom_line(mapping = aes(dateTime, MovingAverage_SpCond_uScm, 
                          group = ID, color = ID)) +
  scale_color_OkabeIto(order = c(1,4,5,6,8)) +
  theme_minimal(base_size = 8) + theme(legend.title = element_blank()) +
  scale_x_datetime(date_breaks = 'month', labels = date_format("%b")) +
  labs(y = "SpC"~(µS~cm^-1), x = "") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        legend.position = 'none')

#chloride plot urban tribs
p3 = ggplot(all_river_cl |> filter(ID %in% c('SW', 'PB', 'SH'))) +
  geom_point(mapping = aes(dateTime, chloride_mgL, 
                           group = ID, color = ID)) +
  geom_path(mapping = aes(dateTime, chloride_mgL, 
                          group = ID, color = ID), size = 0.2, linetype = 2) +
  scale_color_OkabeIto(order = c(2,3,7)) +
  theme_minimal(base_size = 8) + theme(legend.title = element_blank()) +
  scale_x_datetime(date_breaks = 'month', labels = date_format("%b")) +
  labs(y = "Chloride"~(mg~L^-1), x = "") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1), 
        legend.position = 'bottom', 
        legend.title=element_blank(),
        legend.margin = margin(0, 0, 0, 0)) +
  guides(colour = guide_legend(nrow = 1))

#chloride plot ag tribs
p4 = ggplot(all_river_cl |> filter(!ID %in% c('SW', 'PB', 'SH'))) +
   geom_point(mapping = aes(dateTime, chloride_mgL, 
                            group = ID, color = ID)) +
  geom_path(mapping = aes(dateTime, chloride_mgL, 
                          group = ID, color = ID), size = 0.2, linetype = 2) +
  scale_color_OkabeIto(order = c(1,4,5,6,8)) +
  theme_minimal(base_size = 8) + theme(legend.title = element_blank()) +
  scale_x_datetime(date_breaks = 'month', labels = date_format("%b")) +
  labs(y = "Chloride"~(mg~L^-1), x = "") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1), 
        legend.position = 'bottom', 
        legend.title=element_blank(),
        legend.margin = margin(0, 0, 0, 0)) +
  guides(colour = guide_legend(nrow = 1))

p1 / p3 / p2 / p4 +
  plot_annotation(tag_levels = 'a', tag_suffix = ')') & 
  theme(plot.tag = element_text(size = 8))
# Save combo plot
ggsave('Figures/FX_timeseries_with_SH.png', width = 6.5, height = 6, units = 'in', dpi = 500)
