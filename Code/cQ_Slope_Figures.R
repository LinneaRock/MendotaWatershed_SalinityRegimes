#script to create figures with relevant cQ slope information

#load in cQ slope dataframes
source("Data/cQ_slopes/call_cQslope_datasets.R")


##plot showing seasonal and annual cQ slopes for full record, baseflow, and bulk stormflow####

#full record seasonal cQ using all_full dataframe
all_full$season = factor(all_full$season,
                         levels = c("Oct-Dec", "Jan-Mar", "Apr-Jun", "Jul-Sep", "Annual"))

a <- ggplot() +
  geom_point(
    all_full,
    mapping = aes(
      slope,
      reorder(season, desc(season)),
      color = trib,
      fill = trib
    ),
    size = 2,
    shape = 21
  ) +
  annotate(
    "rect",
    xmin = -0.05,
    xmax = 0.05,
    ymin = 0,
    ymax = Inf,
    alpha = 0.2,
    color = "grey"
  ) +
  theme_minimal() +
  scale_color_manual(values = wes_palette("Darjeeling1", n = 5, type = "discrete")) +
  scale_fill_manual(values = wes_palette("Darjeeling1", n = 5, type = "discrete")) +
  labs(x = "cQ Slope",
       y = "") +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  guides(color = guide_legend(nrow = 1)) +
  labs(title = "Full Record")



#baseflow seasonal cQ using all_baseflow dataframe
all_baseflow$season = factor(all_baseflow$season,
                             levels = c("Oct-Dec", "Jan-Mar", "Apr-Jun", "Jul-Sep", "Annual"))

b <- ggplot() +
  geom_point(
    all_baseflow,
    mapping = aes(
      slope,
      reorder(season, desc(season)),
      color = trib,
      fill = trib
    ),
    size = 2,
    shape = 24
  ) +
  annotate(
    "rect",
    xmin = -0.05,
    xmax = 0.05,
    ymin = 0,
    ymax = Inf,
    alpha = 0.2,
    color = "grey"
  ) +
  theme_minimal() +
  scale_color_manual(values = wes_palette("Darjeeling1", n = 5, type = "discrete")) +
  scale_fill_manual(values = wes_palette("Darjeeling1", n = 5, type = "discrete")) +
  labs(x = "cQ Slope",
       y = "") +
  theme(legend.position = "none") +
  labs(title = "Baseflow") +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )



#Bulk stormflow seasonal cQ using all_bulkstorm dataframe
all_bulkstorm$season = factor(all_bulkstorm$season,
                              levels = c("Oct-Dec", "Jan-Mar", "Apr-Jun", "Jul-Sep", "Annual"))

c <- ggplot() +
  geom_point(
    all_bulkstorm,
    mapping = aes(
      slope,
      reorder(season, desc(season)),
      color = trib,
      fill = trib
    ),
    size = 2,
    shape = 23
  ) +
  annotate(
    "rect",
    xmin = -0.05,
    xmax = 0.05,
    ymin = 0,
    ymax = Inf,
    alpha = 0.2,
    color = "grey"
  ) +
  scale_color_manual(values = wes_palette("Darjeeling1", n = 5, type = "discrete")) +
  scale_fill_manual(values = wes_palette("Darjeeling1", n = 5, type = "discrete")) +
  theme_minimal() +
  labs(x = "cQ Slope",
       y = "Season") +
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  labs(title = "Stormflow -
bulk averaged") 
  


#Plots a,b,c together 

library(patchwork)

(a | b | c) +
  plot_annotation(
    tag_levels = 'a',
    tag_suffix = ')',
    theme = theme(
      plot.tag = element_text(size = 10),
      plot.caption = element_text(size = 10, hjust = 0),
      legend.position = "bottom"
    )
  ) + plot_layout(guides = "collect")

ggsave(
  "Figures/F3_fR_bf_qf_slopes.png",
  height = 4.25,
  width = 6.25,
  units = "in"
)




##plot showing individual stormflow events####
#using all_individual events dataframe, plus 
all_individual_events$season = factor(all_individual_events$season,
                                      levels = c("Oct-Dec", "Jan-Mar", "Apr-Jun", "Jul-Sep"))

ggplot() +
  labs(x = "cQ Slope", y = "") +
  geom_point(
    all_individual_events,
    mapping = aes(slope, trib, color = season),
    size = 2.5,
    shape = 21
  ) +
  scale_color_manual(
    labels = c("Oct-Dec", "Jan-Mar", "Apr-Jun", "Jul-Sep"),
    values = c("#1DACE8", "#1C366B", "#F24D29", "#E5C4A1")
  ) +
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
  geom_point(
    all_full %>% filter(season == "Annual"),
    mapping = aes(slope, trib),
    shape = "|",
    size = 6
  ) +
  geom_point(
    all_bulkstorm %>% filter(season == "Annual"),
    mapping = aes(slope, trib),
    shape = "|",
    size = 6,
    color = "red"
  )

ggsave(
  "Figures/F4_individualSlopes.png",
  height = 4.25,
  width = 6.25,
  units = "in"
)

source("Code/Functions/count_stormflow_events_functions.R")

##cQ table####

BFI_tbl <- data.frame(
  Trib = c(
    "YR-I",
    "SMC",
    "DC",
    "PB",
    "YR-O"
  ),
  #BFI found in cQ_Slope_Calculations.R
  BFI = c(42,
          82,
          77,
          49,
          76
  ),
  stormflow_pos = c(count_mobilization_events(all_individual_events, "YR-I"),
                    count_mobilization_events(all_individual_events, "SMC"),
                    count_mobilization_events(all_individual_events, "DC"),
                    count_mobilization_events(all_individual_events, "PB"),
                    count_mobilization_events(all_individual_events, "YR-O")
  ),
  stormflow_stat = c(count_chemostatic_events(all_individual_events, "YR-I"),
                     count_chemostatic_events(all_individual_events, "SMC"),
                     count_chemostatic_events(all_individual_events, "DC"),
                     count_chemostatic_events(all_individual_events, "PB"),
                     count_chemostatic_events(all_individual_events, "YR-O")
                     
  ),
  stormflow_neg = c(count_dilution_events(all_individual_events, "YR-I"),
                    count_dilution_events(all_individual_events, "SMC"),
                    count_dilution_events(all_individual_events, "DC"),
                    count_dilution_events(all_individual_events, "PB"),
                    count_dilution_events(all_individual_events, "YR-O")
                    
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
    title = "Chloride Concentration - Discharge Relationships",
    subtitle = "Log(C) - Log(Q) for all C-Q data"
); simpleregtable

as_latex(simpleregtable)
