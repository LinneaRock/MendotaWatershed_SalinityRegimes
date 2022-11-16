#script to create figures with relevant cQ slope information

#load in cQ slope dataframes
source("Data/cQ_slopes/call_cQslope_datasets.R")

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
                              levels = c( "Jan-Mar", "Apr-Jun", "Jul-Sep", "Oct-Dec", "Annual"))

cQ_plot <- ggplot(cQ_slopes_all) +
  geom_jitter(aes(slope_SpC,reorder(season, desc(season)), shape = flow, color = trib, fill = trib), 
              size = 2, width = 0, height = 0.07, alpha = 0) + # need to have this before annotate
  annotate(geom = "rect", xmin = -0.05, xmax = 0.05, ymin = 0, ymax = Inf,
           alpha = 0.2, color = "grey", size = 0.2) +
  annotate(
    "text",
    label = 'chemostatic',
    x = 0.02,
    y = 1,
    size = 2.5,
    angle = 90,
    color = "grey50",
  ) +
  geom_jitter(aes(slope_SpC,reorder(season, desc(season)), shape = flow, color = trib, fill = trib), 
              size = 2, width = 0, height = 0.07) +
  facet_wrap(~flow) +
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
          size = 3,
          #family = "serif",
          tag_pool = my_tags)

ggsave("Figures/Supplemental/FigureS3_fR_bf_qf_slopes.png", height = 2.75, width = 6.25, 
       units = "in", dpi = 500, bg = 'white')
