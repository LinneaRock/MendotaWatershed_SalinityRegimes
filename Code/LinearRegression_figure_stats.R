#Script to create linear regressions from known chloride concentrations and 
#specific conductivity from grab sampling

#load conductivity logger data
source("Data/Conductivity/call_cond_datasets.R")
#load chloride/conductivity field data
source("Data/Chloride/call_Clfield_datasets.R")

#function to join field and logger data to fill in missing field specific conductivity values with logger values where possible
join_for_linreg <- function(field_data, logger_data) {
  data <- field_data %>%
    mutate(date = round_date(dateTime, unit = "30 minutes")) %>%
    left_join(logger_data %>% mutate(date = dateTime), by = c("date", "dateTime")) %>%
    mutate(SpCond_uScm.x = ifelse(is.na(SpCond_uScm.x), MovingAverage_SpCond_uScm, SpCond_uScm.x))
}

#join datasets for each river
a <- join_for_linreg(YRI_cl, YRI_cond)
b <- join_for_linreg(SMC_cl, SMC_cond)
c <- join_for_linreg(DC_cl, DC_cond)
d <- join_for_linreg(PB_cl, PB_cond)
e <- join_for_linreg(YRO_cl, YRO_cond) 

#bind together 
all_river_linreg <- bind_rows(a, b, c, d, e)

#plot regressions on one graph
library(wesanderson)

ggplot(all_river_linreg, aes(SpCond_uScm.x , chloride_mgL)) +
  geom_point(aes(color = ID.x), size = 0.75) +
  geom_smooth(method = "lm", se = FALSE, size = 0.5, aes(color = ID.x)) +
  scale_color_manual(values = wes_palette("Darjeeling1", n = 5, type = "discrete")) +
  scale_fill_manual(values = wes_palette("Darjeeling1", n = 5, type = "discrete")) +
  labs(x = "Specific Conductivity"~(mu~S~cm^-1)~"@ 25"*~degree*C~"\n", 
       y = "\nChloride Concentration"~(mg~L^-1)) +
  theme_minimal() + theme(legend.title = element_blank())

#save as a supplemental figure
ggsave("Figures/Supplemental/S1_linearRegresions.png", width = 6.25, height = 4.25, units = "in")
