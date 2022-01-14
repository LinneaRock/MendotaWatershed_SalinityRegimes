#script to make plot of monthly chloride mass in each river and get distinctions between
#baseflow and stormflow contributions to chloride export
library(colorblindr)

#call in chloride and discharge data:
source("Code/Estimated_Chloride_Conc_Mass.R") 
#combine all mass data into monthly totals 
month.mass = all_ts_mass |> mutate(month = month(dateTime), year = year(dateTime)) |>
  group_by(ID, month, year) |> 
  summarise(chloride_mass_Mg = sum(chloride_mass_Mg), chloride_mass_Mg_high = sum(chloride_mass_Mg_high), 
            chloride_mass_Mg_low = sum(chloride_mass_Mg_low)) |> 
  mutate(date = ymd(paste(year,month,'01'))) |> 
  mutate_at(vars(starts_with('chloride_mass')), funs(if_else(ID %in% c("YR-O"), . * -1, .))) |>  ## YR-O is negative -- it is the outflow of the lake
  ungroup() |> 
  filter(date >= as.Date('2020-01-01') & date <= as.Date('2021-03-01')) #do not include the partial months Dec 2019 or Apr 2021

month.mass.total = month.mass |> 
  group_by(month, year, date) |> 
  summarise(chloride_mass_Mg = sum(chloride_mass_Mg), chloride_mass_Mg_high = sum(chloride_mass_Mg_high), 
            chloride_mass_Mg_low = sum(chloride_mass_Mg_low))

month.mass.in = month.mass |> filter(ID != 'YR-O') |> 
  group_by(month, year, date) |> 
  summarise(chloride_mass_Mg = sum(chloride_mass_Mg), chloride_mass_Mg_high = sum(chloride_mass_Mg_high), 
            chloride_mass_Mg_low = sum(chloride_mass_Mg_low))

month.mass.out = month.mass |> filter(ID == 'YR-O') |> 
  group_by(month, year, date) |> 
  summarise(chloride_mass_Mg = sum(chloride_mass_Mg), chloride_mass_Mg_high = sum(chloride_mass_Mg_high), 
            chloride_mass_Mg_low = sum(chloride_mass_Mg_low))

# only plots mass for months which we have no missing data, i.e., do not include the partial months Dec 2019 or Apr 2021
ggplot(month.mass) +
  geom_ribbon(data = month.mass.out, aes(x = date, ymin = chloride_mass_Mg_low, ymax = chloride_mass_Mg_high), alpha = 0.3) +
  geom_ribbon(data = month.mass.in, aes(x = date, ymin = chloride_mass_Mg_low, ymax = chloride_mass_Mg_high), alpha = 0.3) +
  geom_bar(mapping = aes(fill = ID, x = date, y = chloride_mass_Mg), position = "stack", stat = "identity") +
  geom_point(data = month.mass.total, aes(x = date, y = chloride_mass_Mg), alpha = 0.7) +
  geom_line(data = month.mass.total, aes(x = date, y = chloride_mass_Mg), linetype = 2, alpha = 0.7) +
  scale_fill_OkabeIto() +
  scale_color_manual(values = c("black", NA), guide = "none") +
  theme_minimal() + theme(legend.title = element_blank()) +
  labs(y = "Mass of Chloride (Mg)", x = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b %Y") +
  scale_y_continuous(n.breaks = 10)

ggsave(
  "Figures/F8_massbalance.png",
  height = 4,
  width = 6.25,
  units = "in",
  dpi = 500
)

# #combine all data into single dataframe
# all_rivers_events_bf <- bind_rows(YRI_events_bf %>% mutate(ID = "YR-I"), SMC_events_bf %>% mutate(ID = "SMC"), 
#                                   DC_events_bf %>% mutate(ID = "DC"), PB_events_bf %>% mutate(ID = "PB"), 
#                                   YRO_events_bf %>% mutate(ID = "YR-O"), SH_events_bf %>% mutate(ID = "SH")) %>%
#   mutate(mon = months.POSIXt(dateTime)) %>%
#   mutate(yr = year(dateTime))

#call in datasets of baseflow and event discharge and chloride
# source("Code/Baseflow_Events_Separation.R")
# monthly_mass_events_bf <- all_rivers_events_bf %>%
#   filter(yr != 2019) %>%
#   filter(dateTime < as.Date("2021-04-01")) %>%
#   group_by(ID, yr, mon) %>%
#   summarise(bfTOT_chloride_Mg = sum(bf_chloride_Mg, na.rm = TRUE),
#             eventTOT_chloride_Mg = sum(event_chloride_Mg, na.rm = TRUE)) %>%
#   #pivot longer for easier graphing
#   pivot_longer(c(bfTOT_chloride_Mg, eventTOT_chloride_Mg), names_to = "flow_type", values_to = "total_chloride_mass_Mg") %>%
#   mutate(total_chloride_mass_Mg = ifelse(ID == "YR-O", total_chloride_mass_Mg * -1, total_chloride_mass_Mg)) %>% #YR-O is negative -- it is the outflow of the lake
#   group_by(ID, mon) %>%
#   mutate(total_mass = sum(total_chloride_mass_Mg)) %>%
#   ungroup() %>%
#   mutate(percent = total_chloride_mass_Mg/total_mass)%>%
#   ungroup() %>%
#   mutate(date = paste(yr, mon, "01", sep = "-")) %>%
#   mutate(date = as.Date(date, format = "%Y-%B-%d"))




