#script to calculate of seasonal salt export in each river and get distinctions between baseflow and stormflow contributions 


#combine all data into single dataframe
all_rivers_events_bf <- bind_rows(YRI_events_bf %>% mutate(ID = "YR-I"), SMC_events_bf %>% mutate(ID = "SMC"), DC_events_bf %>% mutate(ID = "DC"), PB_events_bf %>% mutate(ID = "PB"), YRO_events_bf %>% mutate(ID = "YR-O"), SH_events_bf %>% mutate(ID = "SH")) %>%
  #and add seasons to the dataframe
  mutate(mon = months.POSIXt(dateTime), yr = year(dateTime)) %>% #and add seasons to the dataframe
  mutate(season = case_when(mon %in% c('October','November','December') ~ "2020 Oct-Dec",
                            (mon %in% c('January','February','March') & yr == 2020) ~ "2020 Jan-Mar",
                            mon %in% c('April','May','June')  ~ "2020 Apr-Jun",
                            mon %in% c('July','August','September') ~ "2020 Jul-Sep",
                            (mon %in% c('January','February','March') & yr == 2021) ~ "2021 Jan-Mar")) 
all_rivers_events_bf$season = factor(all_rivers_events_bf$season,
                  levels = c("2020 Jan-Mar", "2020 Apr-Jun", "2020 Jul-Sep", "2020 Oct-Dec",
                             "2021 Jan-Mar"))
all_rivers_events_bf$ID = factor(all_rivers_events_bf$ID, 
                                 levels = c('SH','PB','YR-I','SMC','DC','YR-O'))

## SEASONAL SALT EXPORT CALCULATED AS A PERCENTAGE OF SPECIFIC CONDUCTIVITY EXPORTED ##
# seasonal_mass_events_bf <- all_rivers_events_bf %>%
#   filter(yr != 2019) %>%
#   filter(dateTime < as.POSIXct("2021-04-01 00:00:00")) %>% #only calculate mass for seasons which we have no missing data, i.e., do not include the partial months Dec 2019 or Apr 2021
#   group_by(ID, yr, season) %>%
#   summarise(bfTOT_SpC = sum(bf_SpC_uScm, na.rm = TRUE),
#             eventTOT_SpC = sum(event_SpC_uScm, na.rm = TRUE)) %>%
#   #pivot longer for easier graphing
#   pivot_longer(c(bfTOT_SpC, eventTOT_SpC), names_to = "flow_type", values_to = "total_SpC") %>%
#   mutate(total_SpC = ifelse(ID == "YR-O", total_SpC * -1, total_SpC)) %>%
#   ungroup() %>%
#   group_by(ID, season) %>%
#   mutate(total_annual_SpC = sum(total_SpC)) %>%
#   ungroup() %>%
#   mutate(percent = total_SpC/total_annual_SpC)

## SEASONAL SALT EXPORT CALCULATED AS A PERCENTAGE OF CHLORIDE MASS##
seasonal_mass_events_bf = all_rivers_events_bf %>%
  filter(yr != 2019, dateTime < as.POSIXct("2021-04-01 00:00:00")) %>%
  group_by(ID, yr, season) %>%
  summarise(Base = sum(bf_chloride_Mg, na.rm = TRUE),
            Storm = sum(event_chloride_Mg, na.rm = TRUE)) %>%
  #pivot longer for easier graphing
  pivot_longer(c(Base, Storm), names_to = "flow_type", values_to = "clmass") %>%
  group_by(ID, season) %>%
  mutate(total_clmass = sum(clmass)) %>%
  mutate(percent = clmass/total_clmass) |> 
  ungroup()

seasonal_mass_events_bf$flow_type = factor(seasonal_mass_events_bf$flow_type, 
                                           levels = c('Storm','Base'))

annual_mass_events_bf = all_rivers_events_bf %>%
  filter(yr != 2019, dateTime < as.POSIXct("2021-04-01 00:00:00")) %>%
  group_by(ID) %>%
  summarise(Base = sum(bf_chloride_Mg, na.rm = TRUE),
            Storm = sum(event_chloride_Mg, na.rm = TRUE)) %>%
  pivot_longer(c(Base, Storm), names_to = "flow_type", values_to = "clmass") %>%
  group_by(ID) |> 
  mutate(total_clmass = sum(clmass)) %>%
  mutate(percent = clmass/total_clmass) |> 
  ungroup()

p1 = ggplot(seasonal_mass_events_bf) +
  geom_col(aes(x = ID, y = percent, fill = flow_type)) +
  facet_wrap(~season, nrow = 1) +
  scale_fill_manual(values = c("#801129","#6394a6"), name = 'Flow') +
  ylab('% Contribution to Chloride Load') +
  theme_minimal(base_size = 9) + 
  theme(axis.title.x = element_blank())

p2 = ggplot(seasonal_mass_events_bf) +
  geom_col(aes(x = ID, y = clmass, fill = flow_type)) +
  facet_wrap(~season, nrow = 1) +
  scale_fill_manual(values = c("#801129","#6394a6"), name = 'Flow') +
  ylab('Chloride Load (Mg)') +
  theme_minimal(base_size = 9) + 
  theme(axis.title.x = element_blank())

######## ######## Combo Plot ######## ######## #####
p1 / p2 + 
  plot_layout(guides = 'collect') +
  plot_annotation(tag_levels = 'a', tag_suffix = ')') & 
  theme(plot.tag = element_text(size = 8), 
        legend.key.height = unit(0.5, "cm"),
        legend.position = 'bottom',
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

# Save combo plot
ggsave('Figures/Supplemental/FigureS34_Seasonal_bf_event_export.png', 
       height = 6, width = 6.5, units = 'in', dpi = 500)


## ANNUAL SALT EXPORT CALCULATED AS A PERCENTAGE OF SPECIFIC CONDUCTIVITY EXPORTED ##
# For Table S3
annual_mass_events_bf

annual_mass_events_bf |> select(ID, flow_type, percent) |> 
  pivot_wider(names_from = flow_type, values_from = percent) |> 
  arrange(as.character(ID)) |> 
  xtable()

