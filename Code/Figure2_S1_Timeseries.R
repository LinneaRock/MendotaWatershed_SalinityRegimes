
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

# Highest values
maxValues = all_river_cl |> group_by(ID) |> 
  mutate(cl.max = max(chloride_mgL, na.rm = T)) |> 
  filter(chloride_mgL == cl.max) |> 
  left_join(all_river_cond |> group_by(ID) |> 
  summarise(cond.max = max(MovingAverage_SpCond_uScm, na.rm = T)))

SH_cl |> #filter(dateTime > as.POSIXct('2019-12-01')) |> 
  mutate(cl.max = max(chloride_mgL, na.rm = T)) |> 
  filter(chloride_mgL == cl.max)

SH_cond |> #filter(dateTime > as.POSIXct('2019-12-01')) |> 
  mutate(cond.max = max(MovingAverage_SpCond_uScm, na.rm = T)) |> 
  filter(MovingAverage_SpCond_uScm == cond.max)

# Mean concentrations at each site
all_river_cond |> rbind(SH_cond) |> group_by(ID) |> 
  summarise(meanSpC = mean(SpCond_uScm, na.rm = T))


# p0 = ggplot(all_river_cond |> rbind(SH_cond)) +
#   geom_boxplot(aes(x = ID, y = SpCond_uScm, fill = ID), 
#                size = 0.1, outlier.size = 0.1) +
#   scale_fill_OkabeIto(order = c(1:2,4:8,3)) +
#   theme_bw(base_size = 8) +
#   labs(y = "SpC") +
#   scale_y_log10(limits = c(100,10000), expand = c(0,0.02)) +
#   theme(axis.title.x = element_blank(), 
#         legend.position = 'none', 
#         plot.margin = unit(c(0,0,0,0), "cm"))

# Compute the analysis of variance
res.aov <- aov(SpCond_uScm ~ ID, data = all_river_cond |> rbind(SH_cond))
# Summary of the analysis
summary(res.aov)
TukeyHSD(res.aov)


p1 = ggplot(all_river_cond) +
  geom_line(mapping = aes(dateTime, MovingAverage_SpCond_uScm, 
                                          group = ID, color = ID)) +
  scale_color_OkabeIto(order = c(1:2,4:8)) +
  theme_minimal(base_size = 8) + 
  scale_x_datetime(date_breaks = 'month', labels = date_format("%b")) +
  labs(y = "SpC"~(µS~cm^-1)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        axis.title.x = element_blank(),
        legend.title = element_blank(),
        legend.position = 'none')

# Same plot zoomed in on y-axis without SW and PB
p2 = ggplot(all_river_cond |> filter(!ID %in% c('SW', 'PB'))) +
  geom_line(mapping = aes(dateTime, MovingAverage_SpCond_uScm, 
                                          group = ID, color = ID)) +
  scale_color_OkabeIto(order = c(1,4,5,6,8)) +
  theme_minimal(base_size = 8) + 
  scale_x_datetime(date_breaks = 'month', labels = date_format("%b")) +
  labs(y = "SpC"~(µS~cm^-1)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        axis.title.x = element_blank(),
        legend.title = element_blank(),
        legend.position = 'none')

p3 = ggplot(all_river_cl) +
  geom_point(mapping = aes(dateTime, chloride_mgL, 
                          group = ID, color = ID)) +
  geom_path(mapping = aes(dateTime, chloride_mgL, 
                           group = ID, color = ID), size = 0.2, linetype = 2) +
  scale_color_OkabeIto(order = c(1:2,4:8)) +
  theme_minimal(base_size = 8) + 
  scale_x_datetime(date_breaks = 'month', labels = date_format("%b")) +
  labs(y = "Chloride"~(mg~L^-1)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1), 
        axis.title.x = element_blank(),
        legend.position = 'bottom', 
        legend.title=element_blank(),
        legend.margin = margin(0, 0, 0, 0)) +
  guides(colour = guide_legend(nrow = 1)); p3

# (p1 + inset_element(p0, left = 0.4, bottom = 0.55, right = 1, top = 1, align_to = 'full')) / 
#   p2 / p3 +
#   plot_layout(widths = c(2,1.5,1.5)) +
#   plot_annotation(tag_levels = 'a', tag_suffix = ')') & 
#   theme(plot.tag = element_text(size = 8))

 p1 / p2 / p3 +
plot_layout(widths = c(2,1.5,1.5)) +
plot_annotation(tag_levels = 'a', tag_suffix = ')') & 
theme(plot.tag = element_text(size = 8))
# Save combo plot
ggsave('Figures/F2_timeseries.png', width = 6.5, height = 6, units = 'in', dpi = 500)

  
####################### SPRING HARBOR #############################
# SpC
p1 = ggplot(SH_cond |> filter(dateTime >= as.Date("2014-12-19") &
                                dateTime < as.Date("2021-04-02"))) +
  geom_line(mapping = aes(dateTime, MovingAverage_SpCond_uScm, 
                          group = ID, color = ID)) +
  scale_color_OkabeIto(order = 3) +
  theme_minimal(base_size = 8) + theme(legend.title = element_blank()) +
  scale_x_datetime(date_breaks = 'year', labels = date_format("%Y")) +
  labs(y = "SpC"~(µS~cm^-1), x = "") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        legend.position = 'none')

#Chloride 
p2 = ggplot(SH_cl |> filter(dateTime >= as.Date("2014-12-19") &
                              dateTime < as.Date("2021-04-02"))) +
  geom_point(mapping = aes(dateTime, chloride_mgL, color = ID)) +
  geom_path(mapping = aes(dateTime, chloride_mgL, color = ID), size = 0.2, linetype = 2) +
  scale_color_OkabeIto(order = 3) +
  theme_minimal(base_size = 8) + 
  scale_x_datetime(date_breaks = 'year', labels = date_format("%Y")) +
  labs(y = "Chloride"~(mg~L^-1)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1), 
        axis.title.x = element_blank(),
        legend.position = 'none', 
        legend.title=element_blank(),
        legend.margin = margin(0, 0, 0, 0)) +
  guides(colour = guide_legend(nrow = 1)); p2

p1 / p2  +
  plot_annotation(tag_levels = 'a', tag_suffix = ')') & 
  theme(plot.tag = element_text(size = 8))

# Save combo plot
ggsave('Figures/Supplemental/FigureS1_timeseries_SH.png', width = 6.5, height = 6, units = 'in', dpi = 500)

