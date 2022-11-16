#script to create figures with relevant cQ slope information

#load in cQ slope dataframes
source("Data/cQ_slopes/call_cQslope_datasets.R")


################################################
######## ######## PLOT ######## ######## #####

all_individual_events$season = factor(all_individual_events$season,
                                      levels = c("Jan-Mar", "Apr-Jun", "Jul-Sep", "Oct-Dec"))
all_individual_events$trib = factor(all_individual_events$trib, levels = c('SH','PB','YR-I','SMC','DC', 'YR-O'))
all_individual_events$trib = factor(all_individual_events$trib, levels = c('SH','PB','YR-I','SMC','DC','YR-O'))


ggplot() +
  labs(x = "Stormflow cQ Slope", y = "") +
  annotate("rect", xmin = -0.05, xmax = 0.05, ymin = 0, ymax = Inf, alpha = 0.2, color = "grey") +
  annotate("text", label = 'chemostatic', x = 0.02, y = 2.2, size = 2.5, angle = 90,color = "grey50") +
  geom_jitter(all_individual_events, mapping = aes(slope_SpC, trib, fill = season), 
              width = 0, height = 0.2, size = 2.5, shape = 21, alpha = 0.8) +
  scale_fill_manual(labels = c("Jan-Mar", "Apr-Jun", "Jul-Sep","Oct-Dec"),
                    values = palette_OkabeIto[1:4]) +
  #values = c("#1DACE8", "#1C366B", "#F24D29", "#E5C4A1")) +
  scale_y_discrete(limits=rev) + # flip y axis order for continuity with other plots
  expand_limits(y = 7) +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  geom_point(all_full %>% filter(season == "Annual"), mapping = aes(slope_SpC, trib), shape = "|", size = 6) +
  geom_point(all_baseflow %>% filter(season == "Annual"), mapping = aes(slope_SpC, trib), shape = "|", size = 6, color = "#6394a6") +
  geom_point(all_bulkstorm %>% filter(season == "Annual"), mapping = aes(slope_SpC, trib), shape = "|", size = 6, color = "#801129") +
  geom_curve(aes(x = -0.14, y = 6.6, xend = -0.20, yend = 6.4), curvature = 0.5, arrow = arrow(length = unit(0.03, "npc")), col = "#801129") +
  geom_curve(aes(x = -0.29, y = 6.6, xend = -0.24, yend = 6.4), curvature = -0.5, arrow = arrow(length = unit(0.03, "npc"))) +
  geom_curve(aes(x = 0.2, y = 6.6, xend = 0.07, yend = 6.4), curvature = 0.5, arrow = arrow(length = unit(0.03, "npc")), color = "#6394a6") +
  annotate('text', label = 'stormflow', x = -0.13, y = 6.6, hjust = 0, size = 2.5, col = "#801129") +
  annotate('text', label = 'all', x = -0.33, y = 6.6, hjust = 0, size = 2.5) +
  annotate('text', label = 'baseflow', x = 0.21, y = 6.6, hjust = 0, size = 2.5, col = "#6394a6") +
  coord_equal(ratio = 1/10) 

ggsave("Figures/F5_individualSlopes.png", height = 3.25, width = 6.25, units = "in", dpi = 500)

