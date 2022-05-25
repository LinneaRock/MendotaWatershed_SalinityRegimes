#script to create figures with relevant cQ slope information

#load in cQ slope dataframes
source("Data/cQ_slopes/call_cQslope_datasets.R")

library(colorblindr)
library(egg)

################################################
######## ######## CQ Slopes ######## ######## #####
##plot showing seasonal and annual cQ slopes for full record, baseflow, and bulk stormflow####
cQ_slopes_all <- all_full %>% 
  mutate(flow = "Full Record") %>%
  rbind(all_baseflow %>%
          mutate(flow = "Baseflow")) %>%
  rbind(all_bulkstorm %>%
          mutate(flow = "Stormflow - bulk averaged")) 

cQ_slopes_all$flow = factor(cQ_slopes_all$flow, levels = c("Full Record", "Baseflow", "Stormflow - bulk averaged"))

cQ_slopes_all$season = factor(cQ_slopes_all$season,
                              levels = c("Oct-Dec", "Jan-Mar", "Apr-Jun", "Jul-Sep", "Annual"))

cQ_plot <- ggplot(cQ_slopes_all, aes(
  slope_SpC,
  reorder(season, desc(season)),
  color = trib,
  fill = trib)) +
  geom_jitter(aes(shape = flow), size = 2, width = 0, height = 0.07) +
  facet_wrap(~ flow) +
  annotate(
    "rect",
    xmin = -0.05,
    xmax = 0.05,
    ymin = 0,
    ymax = Inf,
    alpha = 0.2,
    color = "grey"
  ) +
  annotate(
    "text",
    label = 'chemostatic',
    x = 0.02,
    y = 1,
    size = 2.5,
    angle = 90,
    color = "grey50",
  ) +
  scale_color_OkabeIto() +
  scale_fill_OkabeIto() +
  theme_minimal() +
  labs(x = "cQ Slope",
       y = "")  +
  guides(shape = FALSE, colour = guide_legend(nrow = 1)) +
  theme(legend.title = element_blank(),
        legend.position = "bottom", 
        legend.margin=margin(t = 0, unit='cm')); cQ_plot

my_tags <- c("a) Full Record", "b) Baseflow", "c) Stormflow")

tag_facet(cQ_plot, x = -Inf, y = Inf, 
          vjust = 1, hjust = 0.005,
          open = "", close = "",
          fontface = 4,
          size = 3.5,
          #family = "serif",
          tag_pool = my_tags)

ggsave(
  "Figures/F6_fR_bf_qf_slopes.png",
  height = 2.75,
  width = 6.25,
  units = "in",
  dpi = 500
)

################################################
######## ######## PLOT ######## ######## #####
# load in cQ slope dataframes
source("Data/cQ_slopes/call_cQslope_datasets.R")
# using all_individual events dataframe, plus 
all_individual_events$season = factor(all_individual_events$season,
                                      levels = c("Jan-Mar", "Apr-Jun", "Jul-Sep", "Oct-Dec"))
all_individual_events$trib = factor(all_individual_events$trib, levels = c('SH','PB','YR-I','SMC','DC', 'YR-O'))

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

ggsave("Figures/F7_individualSlopes.png", height = 3.25, width = 6.25, units = "in", dpi = 500)

################################################
######## ######## CQ Table ######## ######## #####
source("Code/Functions/count_stormflow_events_functions.R")

BFI_tbl <- data.frame(
  Trib = c(
    "YR-I",
    "SMC",
    "DC",
    "PB",
    "YR-O",
    "SH"
  ),
  #BFI found in cQ_Slope_Calculations.R
  BFI = c(42,
          82,
          77,
          49,
          76,
          20
  ),
  stormflow_pos = c(count_mobilization_events(all_individual_events, "YR-I"),
                    count_mobilization_events(all_individual_events, "SMC"),
                    count_mobilization_events(all_individual_events, "DC"),
                    count_mobilization_events(all_individual_events, "PB"),
                    count_mobilization_events(all_individual_events, "YR-O"),
                    count_mobilization_events(all_individual_events, "SH")
  ),
  stormflow_stat = c(count_chemostatic_events(all_individual_events, "YR-I"),
                     count_chemostatic_events(all_individual_events, "SMC"),
                     count_chemostatic_events(all_individual_events, "DC"),
                     count_chemostatic_events(all_individual_events, "PB"),
                     count_chemostatic_events(all_individual_events, "YR-O"),
                     count_chemostatic_events(all_individual_events, "SH")
                     
  ),
  stormflow_neg = c(count_dilution_events(all_individual_events, "YR-I"),
                    count_dilution_events(all_individual_events, "SMC"),
                    count_dilution_events(all_individual_events, "DC"),
                    count_dilution_events(all_individual_events, "PB"),
                    count_dilution_events(all_individual_events, "YR-O"),
                    count_dilution_events(all_individual_events, "SH")
                    
  )
)

library(gt)

gt_tbl <- gt(BFI_tbl)
simpleregtable <- gt_tbl %>%
  cols_label(
    Trib = "Site",
    BFI = "Eckhardt BFI (%)",
    stormflow_neg = "No. Dilution Events",
    stormflow_stat = "No. Chemostatic Events",
    stormflow_pos = "No. Mobilization Events"
  ) %>%
  tab_header(
    title = "Specific Conductivity - Discharge Relationships",
    subtitle = "Log(SpC) - Log(Q) for all cQ data"
); simpleregtable

as_latex(simpleregtable)
