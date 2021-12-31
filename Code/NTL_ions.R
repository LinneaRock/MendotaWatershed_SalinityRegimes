library(NTLlakeloads) #install_github('hdugan/NTLlakeloads)
library(tidyverse)
library(lubridate)
library(purrr)
library(MetBrewer) # devtools::install_github("BlakeRMills/MetBrewer") 

# load NTL-LTER core datasets
lterions = loadLTERions()
lternuts = loadLTERnutrients()
ltertemp = loadLTERtemp()
# source carbonate function
devtools::source_url("https://raw.githubusercontent.com/hdugan/lake_DIC/master/lake_DIC.R?raw=TRUE")

me.ions = lterions %>% filter(lakeid %in% c('ME','MO')) %>% 
  select(lakeid, sampledate, rep, depth, cl:k) %>% 
  group_by(lakeid, sampledate, depth) %>% 
  summarise_all(mean, na.rm = TRUE) %>% 
  select(-rep)
table(month(me.ions$sampledate))

me.dic = lternuts %>% filter(lakeid%in% c('ME','MO')) %>% 
  select(lakeid, sampledate, depth, rep, ph, alk, dic) %>% 
  filter(!is.na(dic) & dic >0) %>% 
  group_by(lakeid, sampledate, depth) %>% 
  summarise(ph = mean(ph, na.rm = T), alk = mean(alk, na.rm = T), dic = mean(dic, na.rm = T))

me.temp = ltertemp %>% filter(lakeid %in% c('ME','MO')) %>% 
  select(lakeid, sampledate, depth, wtemp) %>% 
  group_by(lakeid, sampledate, depth) %>% 
  summarise(wtemp = mean(wtemp, na.rm = T))

me.all = me.dic %>% left_join(me.temp, 
             by = c("lakeid" = "lakeid", "sampledate" = "sampledate", "depth" = "depth")) %>% 
  group_by(lakeid, sampledate,depth,wtemp,dic,ph,alk) %>%
  do(carbonate(.$wtemp,.$dic,.$ph)) %>% 
  left_join(me.ions, 
            by = c("lakeid" = "lakeid", "sampledate" = "sampledate", "depth" = "depth")) %>% 
  ungroup() |> 
  mutate(salinity = bicarbonate_mgkg + carbonate_mgkg + cl + so4 + ca + mg + na + k)

me.all.long = me.all %>% 
  dplyr::select(-alk, -wtemp) %>% 
  pivot_longer(cols = dic:salinity, names_to = 'var', values_to = 'conc') %>% 
  mutate(month = month(sampledate))

plot.details = list(
  geom_ribbon(aes(x = month, ymin = q.25, ymax = q.75, fill = var), alpha = 0.8, color = 'black',linetype = 2), 
  geom_point(aes(x = month, y = q.25, fill = var), shape = 21), 
  geom_point(aes(x = month, y = q.75, fill = var), shape = 21), 
  scale_x_continuous(breaks = 1:12, labels = month.abb), 
  labs(y = "Conc."~(mg~L^-1)),  
  theme_bw(base_size = 8), 
  theme(axis.title.x = element_blank(), legend.title = element_blank()))

me.quant = me.all.long %>% filter(lakeid == 'ME', depth == 0, !is.na(conc)) |> 
  group_by(var, month = month(sampledate)) |> 
  mutate(n = n()) |> 
  filter(n >= 5) |> 
  summarise(q.75 = quantile(conc, na.rm = T)[4], q.25 = quantile(conc, na.rm = T)[2]) |> 
  filter_all(all_vars(!is.infinite(.)))

me1 = ggplot(me.quant |>  filter(var %in% c('salinity','bicarbonate_mgkg')) |> filter(!is.na(q.75))) +
  scale_fill_manual(values = met.brewer("Egypt", 4)[1:2], labels = c('Bicarbonate','Salinity')) +
  ylim(150,450) + 
  plot.details

me2 = ggplot(me.quant |>  filter(var %in% c('ca','cl')) |> filter(!is.na(q.75))) +
  scale_fill_manual(values = met.brewer("Egypt", 4)[3:4], labels = c('Calcium','Chloride')) +
  ylim(0,75) +
  plot.details

me3 = ggplot(me.quant |>  filter(var %in% c('na','so4','mg','k')) |> filter(!is.na(q.75))) +
  scale_fill_manual(values = met.brewer("Egypt", 4), labels = c('Potassium','Magensium','Sodium','Sulfate')) +
  plot.details

me4 = ggplot(me.quant |>  filter(var %in% c('ph')) |> filter(!is.na(q.75))) +
  scale_fill_manual(values = met.brewer("Egypt", 4)[5], labels = 'ph') +
  ylim(7.5,9.5) +
  plot.details +
  ylab('ph')

# me1 / me2 / me3 / me4 & theme(legend.justification = "left") 

mo.quant = me.all.long %>% filter(lakeid == 'MO', depth == 0, !is.na(conc)) |> 
  group_by(var, month = month(sampledate)) |> 
  mutate(n = n()) |> 
  filter(n >= 5) |> 
  summarise(q.75 = quantile(conc, na.rm = T)[4], q.25 = quantile(conc, na.rm = T)[2]) |> 
  filter_all(all_vars(!is.infinite(.)))

mo1 = ggplot(mo.quant |>  filter(var %in% c('salinity','bicarbonate_mgkg')) |> filter(!is.na(q.75))) +
  scale_fill_manual(values = met.brewer("Egypt", 4)[1:2], labels = c('Bicarbonate','Salinity')) +
  ylim(150,450) +
  plot.details

mo2 = ggplot(mo.quant |>  filter(var %in% c('ca','cl')) |> filter(!is.na(q.75))) +
  scale_fill_manual(values = met.brewer("Egypt", 4)[3:4], labels = c('Calcium','Chloride')) +
  ylim(0,75) +
  plot.details

mo3 = ggplot(mo.quant |>  filter(var %in% c('na','so4','mg','k')) |> filter(!is.na(q.75))) +
  scale_fill_manual(values = met.brewer("Egypt", 4), labels = c('Potassium','Magensium','Sodium','Sulfate')) +
  plot.details

mo4 = ggplot(mo.quant |>  filter(var %in% c('ph')) |> filter(!is.na(q.75))) +
  scale_fill_manual(values = met.brewer("Egypt", 4)[5], labels = 'ph') +
  ylim(7.5,9.5) +
  plot.details +
  ylab('ph')

# Outflow conductivities
cond.YRO = read_csv('Data/Conductivity/YR-O_cond.csv') |> 
  group_by(month = month(dateTime)) |> 
  summarize(SpC = mean(SpCond_uScm, na.rm = T))

c1 = ggplot(cond.YRO) +
  geom_point(aes(x = month, y = SpC), shape = 21, fill = 'black') +
  geom_line(aes(x = month, y = SpC), linetype = 2) +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  labs(y = "SpC"~(µS~cm^-1), title = 'Lake Mendota') + 
  annotate(geom = 'text', x=Inf, y = Inf, label = "outflow", vjust=1.1, hjust=1.1, size = 3) +
  ylim(450,675) +
  theme_bw(base_size = 8) +
  theme(axis.title.x = element_blank(), legend.title = element_blank())

cond.YRS = read_csv('Data/Conductivity/YS_cond.csv') |> 
  group_by(month = month(dateTime)) |> 
  summarize(SpC = mean(SpCond_uScm, na.rm = T))

c2 = ggplot(cond.YRS) +
  geom_point(aes(x = month, y = SpC), shape = 21, fill = 'black') +
  geom_line(aes(x = month, y = SpC), linetype = 2) +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  labs(y = "SpC"~(µS~cm^-1), title = 'Lake Monona') + 
  annotate(geom = 'text', x=Inf, y = Inf, label = "outflow", vjust=1.1, hjust=1.1, size = 3) +
  ylim(450,675) +
  theme_bw(base_size = 8) +
  theme(axis.title.x = element_blank(), legend.title = element_blank())

# Combine all plots 
(c1 + c2 + plot_layout(nrow = 1, guides = 'collect')) /
(me1 + mo1 + plot_layout(nrow = 1, guides = 'collect')) /
  (me2 + mo2 + plot_layout(nrow = 1, guides = 'collect')) /
  (me3 + mo3 + plot_layout(nrow = 1, guides = 'collect')) /
  (me4 + mo4 + plot_layout(nrow = 1, guides = 'collect')) + 
  plot_annotation(tag_levels = 'a', tag_suffix = ')') & 
  theme(plot.tag = element_text(size = 8), legend.justification = "left")

# Save combo plot
ggsave('Figures/FX_lakeIons.png', width = 6.5, height = 6, units = 'in', dpi = 500)
