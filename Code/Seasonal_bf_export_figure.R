# This code makes two figures 
# 1) stormflow cQ slope by site and season
# 2) Total chloride load and percentage base vs. storm flow

#call in datasets of baseflow and event discharge and salt
source("Code/Baseflow_Events_Separation.R")
library(tidyverse)
library(scatterpie)
library(colorblindr)
library(patchwork)
#combine all data into single dataframe
all_rivers_events_bf <- bind_rows(YRI_events_bf %>% mutate(ID = "YR-I"), 
                                  SMC_events_bf %>% mutate(ID = "SMC"), 
                                  DC_events_bf %>% mutate(ID = "DC"), 
                                  PB_events_bf %>% mutate(ID = "PB"), 
                                  YRO_events_bf %>% mutate(ID = "YR-O"), 
                                  SH_events_bf %>% mutate(ID = "SH")) %>%
  mutate(mon = months.POSIXt(dateTime), yr = year(dateTime)) %>% #and add seasons to the dataframe
  mutate(season = case_when(mon %in% c('October','November','December') ~ "2020 Oct-Dec",
                            (mon %in% c('January','February','March') & yr == 2020) ~ "2020 Jan-Mar",
                            mon %in% c('April','May','June')  ~ "2020 Apr-Jun",
                            mon %in% c('July','August','September') ~ "2020 Jul-Sep",
                            (mon %in% c('January','February','March') & yr == 2021) ~ "2021 Jan-Mar"))

# Scatter plot of QC regressions for each site (slow plotting of 600k points!)
# b = all_rivers_events_bf |> select(dateTime, Spc = bf_SpC_uScm, Q = bf_cms, ID) |> mutate(group = 'base') |> 
#   bind_rows(all_rivers_events_bf |> select(dateTime, Spc = event_SpC_uScm, Q = event_flow_cms, ID) |> mutate(group = 'storm')) 
#   # bind_rows(all_rivers_events_bf |> select(dateTime, Spc = all_SpC_uScm, Q = all_dis_cms, ID) |> mutate(group = 'full'))
# 
# fb = ggplot(b) + 
#   geom_point(aes(x = Q, y = Spc, group = group, fill = group), alpha = 0.8, shape = 21) +
#   geom_smooth(aes(x = Q, y = Spc, color = group), method = 'lm') +
#   scale_fill_OkabeIto(name = 'Flow') +
#   scale_color_OkabeIto(name = 'Flow') +
#   scale_y_log10() + scale_x_log10() +
#   labs(y = "SpC"~(µS~cm^-1), x = "Discharge"~(m^3~s^-1)) +
#   facet_wrap(~ID, scales = 'free') +
#   theme_minimal()
# ggsave(plot = fb, filename = "Figures/FX_QCscatter.png",
#        height = 8, width = 8, units = "in", dpi = 500)

a = all_rivers_events_bf %>%
  filter(ID != 'YR-O', yr != 2019) |> 
  filter(dateTime < as.POSIXct("2021-04-01 00:00:00")) %>% #only calculate mass for seasons which we have no missing data, i.e., do not include the partial months Dec 2019 or Apr 2021
  group_by(ID, yr, season) %>%
  summarise(Base = sum(bf_cms, na.rm = TRUE), 
            Storm = sum(event_flow_cms, na.rm = T),
            Chloride = sum(all_chloride_Mg, na.rm = TRUE)) |> 
  ungroup()

a$ID = factor(a$ID, levels = c('SH','PB','YR-I','DC','SMC'))
a$Season = factor(a$season,
                   levels = c("2020 Jan-Mar", "2020 Apr-Jun", "2020 Jul-Sep", "2020 Oct-Dec",
                              "2021 Jan-Mar"))

# Arrange for plotting. Need axes of the same range so pie charts are circles. 
siteX = c(-1.5,-0.75,0,0.75,1.5)
df = a |> 
  arrange(ID, Season) |> 
  mutate(site = c(100*siteX,
                  100*(6+siteX),
                  100*(12+siteX),
                  100*(18+siteX),
                  100*(24+siteX)))

p1 = ggplot(df) +
  geom_path(aes(x = site, y = Chloride, 
                col = Season, group = Season),
            alpha = 0.5, size = 1) +
  geom_scatterpie(data = df, aes(x = site, y = Chloride, 
                                 group = Season), pie_scale = 1,
                  cols = c('Base','Storm'), lwd = 0.2,
                  legend_name = 'Flow') + 
  coord_equal() +
  scale_x_continuous(breaks = 100*c(0,6,12,18,24), 
                     labels = c('SH','PB','YR-I','DC','SMC')) +
  scale_fill_manual(values = c("#6394a6","#801129")) +
  scale_color_OkabeIto() +
  ylab('Chloride Load (Mg)') +
  xlab('High development → Low development') +
  theme_minimal() +
  theme(#legend.position = 'bottom', 
    axis.title.x = element_text(size = 7),
    legend.text = element_text(size = 7), 
    legend.title = element_text(size = 7))


# ggsave("Figures/FX_stormflowContribution.png",
#        height = 2.75, width = 6.25, units = "in", dpi = 500)
  
# Plot 2
# load in cQ slope dataframes
source("Data/cQ_slopes/call_cQslope_datasets.R")
# using all_individual events dataframe, plus 
all_individual_events$season = factor(all_individual_events$season,
                                      levels = c("Jan-Mar", "Apr-Jun", "Jul-Sep", "Oct-Dec"))
all_individual_events$trib = factor(all_individual_events$trib, levels = c('SH','PB','YR-I','DC','SMC', 'YR-O'))

p2 = ggplot() +
  labs(x = "Stormflow cQ Slope", y = "") +
  geom_jitter(all_individual_events, mapping = aes(slope_SpC, trib, fill = season), 
              width = 0, height = 0.2, size = 2.5, shape = 21, alpha = 0.8) +
  scale_fill_manual(labels = c("Jan-Mar", "Apr-Jun", "Jul-Sep","Oct-Dec"),
                    values = palette_OkabeIto[1:4]) +
  #values = c("#1DACE8", "#1C366B", "#F24D29", "#E5C4A1")) +
  scale_y_discrete(limits=rev) + # flip y axis order for continuity with other plots
  expand_limits(y = 7) +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  annotate("rect", xmin = -0.05, xmax = 0.05, ymin = 0, ymax = Inf, alpha = 0.2, color = "grey") +
  annotate("text", label = 'chemostatic', x = 0.02, y = 2.2, size = 2.5, angle = 90,color = "grey50") +
  geom_point(all_full %>% filter(season == "Annual"), mapping = aes(slope_SpC, trib), shape = "|", size = 6) +
  geom_point(all_baseflow %>% filter(season == "Annual"), mapping = aes(slope_SpC, trib), shape = "|", size = 6, color = "#6394a6") +
  geom_point(all_bulkstorm %>% filter(season == "Annual"), mapping = aes(slope_SpC, trib), shape = "|", size = 6, color = "#801129") +
  geom_curve(aes(x = -0.38, y = 6.6, xend = -0.33, yend = 6.4), curvature = -0.5, arrow = arrow(length = unit(0.03, "npc")), col = "#801129") +
  geom_curve(aes(x = -0.2, y = 6.6, xend = -0.19, yend = 6.4), curvature = -0.1, arrow = arrow(length = unit(0.03, "npc"))) +
  geom_curve(aes(x = 0.1, y = 6.6, xend = -0.07, yend = 6.4), curvature = 0.5, arrow = arrow(length = unit(0.03, "npc")), color = "#6394a6") +
  annotate('text', label = 'stormflow', x = -0.51, y = 6.6, hjust = 0, size = 2.5, col = "#801129") +
  annotate('text', label = 'all', x = -0.24, y = 6.6, hjust = 0, size = 2.5) +
  annotate('text', label = 'baseflow', x = 0.11, y = 6.6, hjust = 0, size = 2.5, col = "#6394a6") +
  coord_equal(ratio = 1/10) 

# ggsave("Figures/F7_individualSlopes.png", height = 3.25, width = 6.25, units = "in", dpi = 500)
                    

p1 / p2 +
  plot_annotation(tag_levels = 'a', tag_suffix = ')') & 
  theme(plot.tag = element_text(size = 8))
# Save combo plot
ggsave('Figures/F7_Flow_CQ.png', width = 6.5, units = 'in', dpi = 500)

