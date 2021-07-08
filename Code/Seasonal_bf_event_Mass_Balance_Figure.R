#script to make plot of seasonal chloride mass in each river, with distinctions between baseflow and stormflow
#portions of total mass


#call in datasets of baseflow and event discharge and chloride
source("Code/Baseflow_Events_Separation.R")


#combine all data into single dataframe
all_rivers_events_bf <- bind_rows(YRI_events_bf %>% mutate(ID = "YR-I"), SMC_events_bf %>% mutate(ID = "SMC"), DC_events_bf %>% mutate(ID = "DC"), PB_events_bf %>% mutate(ID = "PB"), YRO_events_bf %>% mutate(ID = "YR-O")) %>%
  #and add seasons to the dataframe
  mutate(mon = months.POSIXt(dateTime)) %>%
  mutate(yr = year(dateTime)) %>%
  mutate(season = NA) %>%
  mutate(season = ifelse(
    mon == "October" |
      mon == "November" |
      mon == "December" , "2020 Oct-Dec", season),
    season =  ifelse(
      mon == "January" & yr == 2020 |
        mon == "February" & yr == 2020 |
        mon == "March" &
        yr == 2020, "2020 Jan-Mar", season),
    season =  ifelse(
      mon == "January" & yr == 2021 |
        mon == "February" & yr == 2021 |
        mon == "March" &
        yr == 2021, "2021 Jan-Mar", season),
    season = ifelse(
      mon == "April" |
        mon == "May" |
        mon == "June", "2020 Apr-Jun", season),
    season = ifelse(
      mon == "July" |
        mon == "August" |
        mon == "September", "2020 Jul-Sep", season))


#only calculate mass for seasons which we have no missing data, i.e., do not include the partial months Dec 2019 or Apr 2021
seasonal_mass_events_bf <- all_rivers_events_bf %>%
  # mutate(bf_chloride_Mg = ifelse(ID == "YR-O", bf_chloride_Mg * -1, bf_chloride_Mg),
  #        event_chloride_Mg = ifelse(ID == "YR-O", event_chloride_Mg * -1, event_chloride_Mg)) %>%
  filter(yr != 2019) %>%
  filter(dateTime < "2021-04-01 00:00:00") %>%
  group_by(ID, yr, season) %>%
  summarise(bfTOT_chloride_Mg = sum(bf_chloride_Mg, na.rm = TRUE),
            eventTOT_chloride_Mg = sum(event_chloride_Mg, na.rm = TRUE)) %>%
  #pivot longer for easier graphing
  pivot_longer(c(bfTOT_chloride_Mg, eventTOT_chloride_Mg), names_to = "flow_type", values_to = "total_chloride_mass_Mg") %>%
  mutate(total_chloride_mass_Mg = ifelse(ID == "YR-O", total_chloride_mass_Mg * -1, total_chloride_mass_Mg))

seasonal_mass_events_bf$season = factor(seasonal_mass_events_bf$season, levels = c("2020 Jan-Mar", "2020 Apr-Jun", "2020 Jul-Sep", "2020 Oct-Dec", "2021 Jan-Mar"))


ggplot() +
  geom_bar(seasonal_mass_events_bf, mapping = aes(fill = ID, x = reorder(season, desc(season)), y = total_chloride_mass_Mg), stat = "identity") +
  geom_bar(seasonal_mass_events_bf %>% filter(flow_type == "bfTOT_chloride_Mg"), mapping = aes(x = reorder(season, desc(season)), y = total_chloride_mass_Mg, fill = NA), color = "black", stat = "identity") +
  scale_fill_manual(values = wes_palette("Darjeeling1", n = 5, type = "discrete")) +
  theme_minimal() + theme(legend.title = element_blank()) +
  labs(y = "Mass of Chloride (Mg)", x = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_flip()

