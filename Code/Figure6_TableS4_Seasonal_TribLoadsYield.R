# This code makes two figures and stats 
# 1) stormflow cQ slope by site and season
# OLS model for chloride load and yield
# 2) Total chloride load and percentage base vs. storm flow

#call in datasets of baseflow and event discharge and salt
source("Code/DataLoad/Baseflow_Events_Separation.R")
library(tidyverse)
library(scatterpie)
library(colorblindr)
library(patchwork)

#combine all data into single dataframe
all_rivers_events_bf <- bind_rows(YRI_events_bf %>% mutate(ID = "YR-I"), 
                                  SMC_events_bf %>% mutate(ID = "SMC"), 
                                  DC_events_bf %>% mutate(ID = "DC"), 
                                  PB_events_bf %>% mutate(ID = "PB"), 
                                  YRO_events_bf %>% mutate(ID = "YR-O"), 
                                  SH_events_bf %>% mutate(ID = "SH")) %>%
  mutate(mon = months.POSIXt(dateTime), yr = year(dateTime)) %>% #and add seasons to the dataframe
  mutate(season = case_when(mon %in% c('October','November','December') ~ "2020 Oct-Dec",
                            (mon %in% c('January','February','March') & yr == 2020) ~ "2020 Jan-Mar",
                            mon %in% c('April','May','June')  ~ "2020 Apr-Jun",
                            mon %in% c('July','August','September') ~ "2020 Jul-Sep",
                            (mon %in% c('January','February','March') & yr == 2021) ~ "2021 Jan-Mar")) |> 
  mutate(quarter = paste0('S',quarter(dateTime, with_year = FALSE, fiscal_start = 1)))
  
# Scatter plot of QC regressions for each site (slow plotting of 600k points!)
# b = all_rivers_events_bf |> select(dateTime, Spc = bf_SpC_uScm, Q = bf_cms, ID) |> mutate(group = 'base') |> 
#   bind_rows(all_rivers_events_bf |> select(dateTime, Spc = event_SpC_uScm, Q = event_flow_cms, ID) |> mutate(group = 'storm')) 
#   # bind_rows(all_rivers_events_bf |> select(dateTime, Spc = all_SpC_uScm, Q = all_dis_cms, ID) |> mutate(group = 'full'))
# 
# fb = ggplot(b) + 
#   geom_point(aes(x = Q, y = Spc, group = group, fill = group), alpha = 0.8, shape = 21) +
#   geom_smooth(aes(x = Q, y = Spc, color = group), method = 'lm') +
#   scale_fill_OkabeIto(name = 'Flow') +
#   scale_color_OkabeIto(name = 'Flow') +
#   scale_y_log10() + scale_x_log10() +
#   labs(y = "SpC"~(µS~cm^-1), x = "Discharge"~(m^3~s^-1)) +
#   facet_wrap(~ID, scales = 'free') +
#   theme_minimal()
# ggsave(plot = fb, filename = "Figures/FX_QCscatter.png",
#        height = 8, width = 8, units = "in", dpi = 500)

drainageArea = data.frame(ID = c('SH','PB','YR-I','SMC','DC'),
                          Area = c(912.15, 4750.24, 29405.99, 12530.32, 3269.92),
                          Developed = c(86.30, 41.03, 17.09, 15.91, 9.39))

a = all_rivers_events_bf %>%
  filter(ID != 'YR-O', yr != 2019) |> 
  filter(dateTime < as.POSIXct("2021-04-01 00:00:00")) %>% #only calculate mass for seasons which we have no missing data, i.e., do not include the partial months Dec 2019 or Apr 2021
  group_by(ID, yr, season, quarter) %>%
  summarise(Base = sum(bf_cms, na.rm = TRUE), 
            Storm = sum(event_flow_cms, na.rm = T),
            TotQ = sum(all_dis_cms, na.rm = T),
            BaseSalt = sum(bf_chloride_Mg, na.rm = TRUE), 
            StormSalt = sum(event_chloride_Mg, na.rm = T),
            Chloride = sum(all_chloride_Mg, na.rm = TRUE)) |> 
  ungroup() |> 
  left_join(drainageArea) |> 
  mutate(StormQper = Storm/TotQ) |> 
  mutate(BaseYield = BaseSalt/Area, StormYield = StormSalt/Area, ChlorideYield = Chloride/Area)

a$ID = factor(a$ID, levels = c('SH','PB','YR-I','SMC','DC'))
a$Season = factor(a$season,
                   levels = c("2020 Jan-Mar", "2020 Apr-Jun", "2020 Jul-Sep", "2020 Oct-Dec",
                              "2021 Jan-Mar"))

################################################
######## ######## Stats ######## ######## #####
library(jtools)
m1 <- lm(Chloride ~ TotQ + Developed:quarter + Developed:TotQ,
             data = a) #f2 = 0.9393
summ(m1)
xtable(m1)

m2 <- lm(ChlorideYield ~ Developed:quarter,
         data = a) #f2 = 0.9044
summ(m2)
xtable(m2)
# huxtable::print_latex(export_summs(m1))

################################################
######## ######## PLOT 1: Chloride Load ######## 
# Arrange for plotting. Need axes of the same range so pie charts are circles. 
siteX = c(-1.5,-0.75,0,0.75,1.5)
df = a |> 
  arrange(ID, Season) |> 
  mutate(site = c(100*siteX,
                  100*(6+siteX),
                  100*(12+siteX),
                  100*(18+siteX),
                  100*(24+siteX)))

multiplier = 100
p1 = ggplot(df) +
  annotate("rect", xmin = 0-(3*multiplier), xmax = 0+(3*multiplier),
           ymin = 0.01, ymax = 1500, fill = 'grey80', alpha = 0.2) +
  annotate("rect", xmin = 12*multiplier-(3*multiplier), xmax = 12*multiplier+(3*multiplier),
           ymin = 0.01, ymax = 1500, fill = 'grey80', alpha = 0.2) +
  annotate("rect", xmin = 24*multiplier-(3*multiplier), xmax = 24*multiplier+(3*multiplier),
           ymin = 0.01, ymax = 1500, fill = 'grey80', alpha = 0.2) +
  geom_path(aes(x = site, y = Chloride, 
                col = Season, group = Season),
            alpha = 0.5, size = 1) +
  geom_scatterpie(data = df, aes(x = site, y = Chloride, 
                                 group = Season), pie_scale = 1,
                  cols = c('Base','Storm'), lwd = 0.2,
                  legend_name = 'Flow') + 
  coord_equal() +
  scale_x_continuous(breaks = multiplier*c(0,6,12,18,24), 
                     labels = c('SH','PB','YR-I','SMC','DC')) +
  scale_fill_manual(values = c("#6394a6","#801129")) +
  scale_color_OkabeIto() +
  ylab('Chloride Load (Mg)') +
  xlab('High development → Low development') +
  theme_minimal() +
  theme(#legend.position = 'bottom', 
    axis.title.x = element_text(size = 7),
    legend.text = element_text(size = 7), 
    legend.title = element_text(size = 7)); p1

# ggsave("Figures/FX_stormflowContribution.png",
#        height = 2.75, width = 6.25, units = "in", dpi = 500)
  
################################################
######## ######## PLOT 2: Chloride Yield #######
# Chloride Yield instead of Load 
# Arrange for plotting. Need axes of the same range so pie charts are circles. 
siteX = c(-1.5,-0.75,0,0.75,1.5)
multiplier = log10(1.15)
df = a |> 
  arrange(ID, Season) |> 
  mutate(site = c(multiplier*siteX,
                  multiplier*(6+siteX),
                  multiplier*(12+siteX),
                  multiplier*(18+siteX),
                  multiplier*(24+siteX)))

p2 = ggplot(df) +
  annotate("rect", xmin = 0-(3*multiplier), xmax = 0+(3*multiplier),
            ymin = 0.01, ymax = 0.2, fill = 'grey80', alpha = 0.2) +
  annotate("rect", xmin = 12*multiplier-(3*multiplier), xmax = 12*multiplier+(3*multiplier),
           ymin = 0.01, ymax = 0.2, fill = 'grey80', alpha = 0.2) +
  annotate("rect", xmin = 24*multiplier-(3*multiplier), xmax = 24*multiplier+(3*multiplier),
           ymin = 0.01, ymax = 0.2, fill = 'grey80', alpha = 0.2) +
  geom_path(aes(x = site, y = ChlorideYield,
                col = Season, group = Season),
            alpha = 0.5, size = 1) +
  geom_scatterpie(data = df, aes(x = site, y = ChlorideYield, 
                                 group = Season), pie_scale = 1,
                  cols = c('Base','Storm'), lwd = 0.2,
                  legend_name = 'Flow') + 
  coord_equal() +
  scale_x_continuous(breaks = multiplier*c(0,6,12,18,24), 
                     labels = c('SH','PB','YR-I','DC','SMC')) +
  scale_fill_manual(values = c("#6394a6","#801129")) +
  scale_color_OkabeIto() +
  scale_y_log10() +
  ylab("Chloride Yield"~(Mg~Ha^-1)) +
  xlab('High development → Low development') +
  theme_minimal() +
  theme(#legend.position = 'bottom', 
    axis.title.x = element_text(size = 7),
    legend.text = element_text(size = 7), 
    legend.title = element_text(size = 7)); p2


################################################
######## ######## Combo Plot ######## ######## #####
p1 / p2 + plot_layout(guides = 'collect') +
  plot_annotation(tag_levels = 'a', tag_suffix = ')') & 
  theme(plot.tag = element_text(size = 8), 
        legend.key.height = unit(0.5, "cm"))
# Save combo plot
ggsave('Figures/F6_TribLoadsYields.png', width = 6.5, units = 'in', dpi = 500)

