#script to create figures with relevant cQ slope information

#load in cQ slope dataframes
source("Data/cQ_slopes/call_cQslope_datasets.R")

library(colorblindr)
library(egg)

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




 ##plot showing individual stormflow events####
#using all_individual events dataframe, plus 
all_individual_events$season = factor(all_individual_events$season,
                                      levels = c("Oct-Dec", "Jan-Mar", "Apr-Jun", "Jul-Sep"))

ggplot() +
  labs(x = "Stormflow cQ Slope", y = "") +
  geom_jitter(
    all_individual_events,
    mapping = aes(slope_SpC, trib, fill = season),
    width = 0, height = 0.2,
    size = 2.5,
    shape = 21, alpha = 0.8,
  ) +
  scale_fill_manual(
    labels = c("Oct-Dec", "Jan-Mar", "Apr-Jun", "Jul-Sep"),
    values = palette_OkabeIto[1:4]
    #values = c("#1DACE8", "#1C366B", "#F24D29", "#E5C4A1")
  ) +
  scale_y_discrete(limits=rev) + # flip y axis order for continutity with other plots
  theme_minimal() +
  theme(legend.title = element_blank()) +
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
    y = 5.9,
    size = 2.5,
    angle = 90,
    color = "grey50",
  ) +
  geom_point(
    all_full %>% filter(season == "Annual"),
    mapping = aes(slope_SpC, trib),
    shape = "|",
    size = 6
  ) +
  geom_point(
    all_baseflow %>% filter(season == "Annual"),
    mapping = aes(slope_SpC, trib),
    shape = "|",
    size = 6,
    color = "#CD5000"
  ) +
  geom_point(
    all_bulkstorm %>% filter(season == "Annual"),
    mapping = aes(slope_SpC, trib),
    shape = "|",
    size = 6,
    color = "#801129"
  ) +
  geom_curve(aes(x = -0.38, y = 0.2, xend = -0.33, yend = 0.6), arrow = arrow(length = unit(0.03, "npc")), col = "#801129") +
  geom_curve(aes(x = -0.2, y = 0.2, xend = -0.19, yend = 0.6), curvature = 0.1, arrow = arrow(length = unit(0.03, "npc"))) +
  geom_curve(aes(x = 0.1, y = 0.2, xend = -0.07, yend = 0.6), curvature = -0.5, arrow = arrow(length = unit(0.03, "npc")), color = "#CD5000") +
  annotate('text', label = 'stormflow', x = -0.51, y = 0.2, hjust = 0, size = 2.5, col = "#801129") +
  annotate('text', label = 'all', x = -0.24, y = 0.2, hjust = 0, size = 2.5) +
  annotate('text', label = 'baseflow', x = 0.11, y = 0.2, hjust = 0, size = 2.5, col = "#CD5000")

ggsave(
  "Figures/F7_individualSlopes.png",
  height = 3.25,
  width = 6.25,
  units = "in",
  dpi = 500
)

source("Code/Functions/count_stormflow_events_functions.R")

##cQ table####

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
