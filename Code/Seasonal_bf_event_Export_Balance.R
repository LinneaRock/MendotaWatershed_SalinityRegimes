#script to calculate of seasonal salt export in each river and get distinctions between baseflow and stormflow contributions 



#call in datasets of baseflow and event discharge and salt
source("Code/Baseflow_Events_Separation.R")


#combine all data into single dataframe
all_rivers_events_bf <- bind_rows(YRI_events_bf %>% mutate(ID = "YR-I"), SMC_events_bf %>% mutate(ID = "SMC"), DC_events_bf %>% mutate(ID = "DC"), PB_events_bf %>% mutate(ID = "PB"), YRO_events_bf %>% mutate(ID = "YR-O"), SH_events_bf %>% mutate(ID = "SH")) %>%
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

## SEASONAL SALT EXPORT CALCULATED AS A PERCENTAGE OF sPECIFIC CONDUCTIVITY EXPORTED ##
seasonal_mass_events_bf <- all_rivers_events_bf %>%
  filter(yr != 2019) %>%
  filter(as.character(dateTime) < "2021-04-01 00:00:00") %>% #only calculate mass for seasons which we have no missing data, i.e., do not include the partial months Dec 2019 or Apr 2021
  group_by(ID, yr, season) %>%
  summarise(bfTOT_SpC = sum(bf_SpC_uScm, na.rm = TRUE),
            eventTOT_SpC = sum(event_SpC_uScm, na.rm = TRUE)) %>%
  #pivot longer for easier graphing
  pivot_longer(c(bfTOT_SpC, eventTOT_SpC), names_to = "flow_type", values_to = "total_SpC") %>%
  mutate(total_SpC = ifelse(ID == "YR-O", total_SpC * -1, total_SpC)) %>%
  ungroup() %>%
  group_by(ID, season) %>%
  mutate(total_annual_SpC = sum(total_SpC)) %>%
  ungroup() %>%
  mutate(percent = total_SpC/total_annual_SpC)



## ANNUAL SALT EXPORT CALCULATED AS A PERCENTAGE OF sPECIFIC CONDUCTIVITY EXPORTED ##
annual_mass_events_bf <- all_rivers_events_bf %>%
  filter(yr != 2019) %>%
  filter(as.character(dateTime) < "2021-04-01 00:00:00") %>% #only calculate mass for seasons which we have no missing data, i.e., do not include the partial months Dec 2019 or Apr 2021
  group_by(ID) %>%
  summarise(bfTOT_SpC = sum(bf_SpC_uScm, na.rm = TRUE),
            eventTOT_SpC = sum(event_SpC_uScm, na.rm = TRUE)) %>%
  #pivot longer for easier graphing
  pivot_longer(c(bfTOT_SpC, eventTOT_SpC), names_to = "flow_type", values_to = "total_SpC") %>%
  mutate(total_SpC = ifelse(ID == "YR-O", total_SpC * -1, total_SpC)) %>%
  group_by(ID) %>%
  mutate(total_annual_SpC = sum(total_SpC)) %>%
  mutate(percent = total_SpC/total_annual_SpC)
