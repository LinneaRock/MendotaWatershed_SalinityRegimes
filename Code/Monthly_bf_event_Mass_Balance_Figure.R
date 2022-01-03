#script to make plot of monthly chloride mass in each river and get distinctions between baseflow and stormflow contributions to chloride export



#call in datasets of baseflow and event discharge and chloride
source("Code/Baseflow_Events_Separation.R")


#combine all data into single dataframe
all_rivers_events_bf <- bind_rows(YRI_events_bf %>% mutate(ID = "YR-I"), SMC_events_bf %>% mutate(ID = "SMC"), DC_events_bf %>% mutate(ID = "DC"), PB_events_bf %>% mutate(ID = "PB"), YRO_events_bf %>% mutate(ID = "YR-O")) %>%
  mutate(mon = months.POSIXt(dateTime)) %>%
  mutate(yr = year(dateTime))

#only calculate mass for months which we have no missing data, i.e., do not include the partial months Dec 2019 or Apr 2021
monthly_mass_events_bf <- all_rivers_events_bf %>%
  filter(yr != 2019) %>%
  filter(dateTime < as.Date("2021-04-01")) %>%
  group_by(ID, yr, mon) %>%
  summarise(bfTOT_chloride_Mg = sum(bf_chloride_Mg, na.rm = TRUE),
            eventTOT_chloride_Mg = sum(event_chloride_Mg, na.rm = TRUE)) %>%
  #pivot longer for easier graphing
  pivot_longer(c(bfTOT_chloride_Mg, eventTOT_chloride_Mg), names_to = "flow_type", values_to = "total_chloride_mass_Mg") %>%
  mutate(total_chloride_mass_Mg = ifelse(ID == "YR-O", total_chloride_mass_Mg * -1, total_chloride_mass_Mg)) %>% #YR-O is negative -- it is the outflow of the lake
  group_by(ID, mon) %>%
  mutate(total_mass = sum(total_chloride_mass_Mg)) %>%
  ungroup() %>%
  mutate(percent = total_chloride_mass_Mg/total_mass)%>%
  ungroup() %>%
  mutate(date = paste(yr, mon, "01", sep = "-")) %>%
  mutate(date = as.Date(date, format = "%Y-%B-%d"))

#monthly_mass_events_bf$mon = factor(monthly_mass_events_bf$season, levels = c("2020 Jan-Mar", "2020 Apr-Jun", "2020 Jul-Sep", "2020 Oct-Dec", "2021 Jan-Mar"))

library(colorblindr)

ggplot() +
  geom_bar(monthly_mass_events_bf, mapping = aes(fill = ID, x = date, y = total_chloride_mass_Mg), position = "stack", stat = "identity") +
  scale_fill_OkabeIto() +
  #scale_fill_manual(values = wes_palette("Darjeeling1", n = 5, type = "discrete")) +
  scale_color_manual(values = c("black", NA), guide = FALSE) +
  theme_minimal() + theme(legend.title = element_blank()) +
  labs(y = "Mass of Chloride (Mg)", x = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b %Y") +
  scale_y_continuous(n.breaks = 10)

ggsave(
  "Figures/F5_massbalance.png",
  height = 4,
  width = 6.25,
  units = "in",
  dpi = 500
)

