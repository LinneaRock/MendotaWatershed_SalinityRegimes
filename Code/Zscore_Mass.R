#script to calculate and plot z-score of chloride mass tranpsort
library(scales)

#load datasets with chloride timeseries
source("Code/estimated_chloride_conc_mass.R")


mass_normalized <- function(ts_mass, id) {
  
  ts1 <- ts_mass %>%
    group_by(ID) %>%
    mutate(zscore_chloride_mass = (chloride_mass_Mg - mean(chloride_mass_Mg, na.rm = TRUE)) / sd(chloride_mass_Mg, na.rm = TRUE))
  
}

massNorm <- mass_normalized(all_ts_mass)


ggplot() +
  # geom_vline(aes(xintercept = as.Date('2020-03-22')), linetype = 2) +
  # geom_vline(aes(xintercept = as.Date('2021-03-20')), linetype = 2) +
  geom_line(massNorm %>% filter(ID == "SH"), mapping = aes(dateTime, zscore_chloride_mass, group = ID, color = ID), size = 0.25) +
  geom_line(massNorm %>% filter(ID != "YR-O"), mapping = aes(dateTime, zscore_chloride_mass, group = ID, color = ID)) +
  geom_line(massNorm %>% filter(ID == "YR-O"), mapping = aes(dateTime, zscore_chloride_mass, group = ID, color = ID), size = 1.25) +
  scale_color_OkabeIto() +
  theme_minimal() + theme(legend.title = element_blank()) +
  scale_x_datetime(date_breaks = 'month', labels = date_format("%b")) +
  labs(y = "Z-score of chloride mass", x = "") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))


ggsave("Figures/F5_cl_mass_zscore.png", width = 6.25, height = 4.25, units = "in", dpi = 500)


