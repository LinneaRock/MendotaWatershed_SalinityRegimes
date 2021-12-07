#script to calculate and plot z-score of flow-normalized chloride concentration

#load datasets with chloride timeseries
source("Code/estimated_chloride_conc_mass.R")
#load function to flow-normalize and calculate z-score
source("Code/Functions/flow_normalize_zscore_function.R")


#flow-normalized chloride in each river binded to a single dataframe
All_flow_norm <- flow_normalize(YRI_ts_mass, "YR-I") %>%
  bind_rows(flow_normalize(DC_ts_mass, "DC"),
            flow_normalize(SMC_ts_mass, "SMC"),
            flow_normalize(PB_ts_mass, "PB"),
            flow_normalize(YRO_ts_mass, "YR-O"))


#create the z-score plot
library(colorblindr)
library(scales)

ggplot() +
  geom_vline(aes(xintercept = as.Date('2020-03-22')), linetype = 2) +
  geom_vline(aes(xintercept = as.Date('2021-03-20')), linetype = 2) +
  geom_line(All_flow_norm %>% filter(ID != "YR-O"), mapping = aes(date, zscore_chloride, group = ID, color = ID)) +
  geom_line(All_flow_norm %>% filter(ID == "YR-O"), mapping = aes(date, zscore_chloride, group = ID, color = ID), size = 1.25) +
  scale_color_OkabeIto() +
  #scale_color_manual(values = wes_palette("Darjeeling1", n = 5, type = "discrete")) +
  theme_minimal() + theme(legend.title = element_blank()) +
  scale_x_date(date_breaks = 'month', labels = date_format("%b")) +
  labs(y = "Z-score of flow-normalized chloride", x = "") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

ggsave("Figures/F2_flow_norm_cl_zscore.png", width = 6.25, height = 4.25, units = "in", dpi = 500)
  

