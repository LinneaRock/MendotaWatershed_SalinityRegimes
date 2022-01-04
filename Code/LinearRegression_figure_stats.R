#Script to create linear regressions from known chloride concentrations and 
#specific conductivity from grab sampling
library(tidyverse)
library(patchwork)
library(broom)
#load conductivity logger data
source("Data/Conductivity/call_cond_datasets.R")
#load chloride/conductivity field data
source("Data/Chloride/call_Clfield_datasets.R")

#function to join field and logger data to fill in missing field specific conductivity values with logger values where possible
source("Code/Functions/join_field_cond_function.R")

#join datasets for each river
a <- join_for_linreg(YRI_cl, YRI_cond)
b <- join_for_linreg(SMC_cl, SMC_cond)
c <- join_for_linreg(DC_cl, DC_cond)
d <- join_for_linreg(PB_cl, PB_cond)
e <- join_for_linreg(YRO_cl, YRO_cond) 
f <- join_for_linreg(SW_cl, SW_cond) 
g <- join_for_linreg(YRS_cl, YRS_cond) 
h <- join_for_linreg(SH_cl, SH_cond) 

#bind together 
all_river_linreg <- bind_rows(a, b, c, d, e, f, g, h)
all_river_linreg$ID.x = factor(all_river_linreg$ID.x,
                               levels = c("DC", "PB", "SH", "SMC", "YR-I", "YR-O", "SW", "YR-S"))

# Check EC similarities
ggplot(all_river_linreg |> filter(ID.x != 'SH')) +
  geom_point(aes(x = SpCond_uScm.x, y = MovingAverage_SpCond_uScm)) +
  geom_abline()

#plot regressions on one graph
library(colorblindr)

ggplot(all_river_linreg, aes(SpCond_uScm.x , chloride_mgL)) +
  geom_point(aes(color = ID.x), size = 0.75) +
  geom_smooth(method = "lm", se = FALSE, size = 0.5, aes(color = ID.x)) +
  scale_color_OkabeIto() +
  scale_fill_OkabeIto() +
  # scale_color_manual(values = wes_palette("Darjeeling1", n = 5, type = "discrete")) +
  # scale_fill_manual(values = wes_palette("Darjeeling1", n = 5, type = "discrete")) +
  labs(x = "Specific Conductivity"~(mu~S~cm^-1)~"@ 25"*~degree*C~"\n", 
       y = "\nChloride Concentration"~(mg~L^-1)) +
  theme_minimal() + theme(legend.title = element_blank())

 #save as a supplemental figure
ggsave("Figures/Supplemental/FigureS1_linearRegresions.png", width = 6.25, height = 4.25, units = "in", dpi = 500)

sites.df = all_river_linreg |> 
  dplyr::select(date, ID.x, chloride_mgL, SpCond_uScm.x, MovingAverage_SpCond_uScm) |> 
  filter(!is.na(SpCond_uScm.x) | !is.na(MovingAverage_SpCond_uScm)) |> 
  pivot_longer(cols = SpCond_uScm.x: MovingAverage_SpCond_uScm) |> 
  arrange(ID.x, chloride_mgL)

## Broom regressions ####
b.regs = sites.df %>% 
  nest(data = -ID.x) %>% 
  mutate(
    fit = map(data, ~ lm(chloride_mgL ~ value, data = .x)),
    tidied = map(fit, tidy),
    glanced = map(fit, glance)
  ) %>% 
  unnest(glanced) |> 
  select(River = ID.x, Adjusted_R2 = adj.r.squared, P_value = p.value) |> 
  mutate(P_value = round(P_value, 2), 
         Adjusted_R2 = sprintf('%.2f',round(Adjusted_R2, 2)))

# r2 equations for plots
r2text <- function(site, Rin){
  paste0('"',site,":",'"',"~italic(r)^2~",'"',"=",'"',"~",Rin)
}

#Spring Harbor inset
pr0 = ggplot(sites.df |> filter(ID.x %in% c('SH')), aes(value , chloride_mgL)) +
  geom_point(aes(color = ID.x, group = chloride_mgL), size = 0.75, shape = 21, fill = 'black') +
  geom_smooth(method = "lm", se = FALSE, size = 0.5, aes(color = ID.x)) +
  scale_x_continuous(breaks = c(0,10000,20000)) +
  scale_color_OkabeIto(order = 3) +
  scale_fill_OkabeIto() +
  labs(x = "SpC"~(µS~cm^-1), y = "Chloride"~(mg~L^-1)) +
  theme_bw(base_size = 7) + 
  theme(legend.position = "none",
        axis.title = element_blank())

pr1 = ggplot(sites.df |> filter(ID.x %in% c('SH','PB','SW')), aes(value , chloride_mgL)) +
  geom_point(aes(color = ID.x, group = chloride_mgL), size = 0.75, shape = 21, fill = 'black') +
  geom_path(aes(color = ID.x, group = chloride_mgL), size = 0.3) +
  geom_smooth(method = "lm", se = FALSE, size = 1, aes(color = ID.x)) +
  xlim(0,3000) + ylim(0,700) +
  annotate('text', x= 2000, y = 100, col = palette_OkabeIto[2], vjust = 0,
           label = r2text('PB',b.regs |> filter(River == 'PB') |> pull(Adjusted_R2)), parse=T, size = 2) +
  annotate('text', x= 500, y = 300, col = palette_OkabeIto[3], vjust = 0,
           label = r2text('SH',b.regs |> filter(River == 'SH') |> pull(Adjusted_R2)), parse=T, size = 2) +
  annotate('text', x= 2500, y = 300, col = palette_OkabeIto[7], vjust = 0,
           label = r2text('SW',b.regs |> filter(River == 'SW') |> pull(Adjusted_R2)), parse=T, size = 2) +
  scale_color_OkabeIto(order = c(2,3,7)) +
  scale_fill_OkabeIto() +
  labs(x = "SpC"~(µS~cm^-1), y = "Chloride"~(mg~L^-1)) +
  theme_minimal(base_size = 8) + theme(legend.title = element_blank())

pr2 = ggplot(sites.df |> filter(!ID.x %in% c('SH','PB','SW')), 
       aes(value , chloride_mgL)) +
  geom_point(aes(color = ID.x, group = chloride_mgL), size = 0.75, shape = 21, fill = 'black') +
  geom_path(aes(color = ID.x, group = chloride_mgL), size = 0.3) +
  geom_smooth(method = "lm", se = FALSE, size = 1, aes(color = ID.x)) +
  annotate('text', x= 1000, y = 30, col = palette_OkabeIto[1], hjust = 0, vjust = 0, 
           label = r2text('DC',b.regs |> filter(River == 'DC') |> pull(Adjusted_R2)), parse=T, size = 2) +
  annotate('text', x= 1000, y = 55, col = palette_OkabeIto[4], hjust = 0, vjust = 0, 
           label = r2text('SMC',b.regs |> filter(River == 'SMC') |> pull(Adjusted_R2)), parse=T, size = 2) +
  annotate('text', x= 1000, y = 60, col = palette_OkabeIto[5], hjust = 0, vjust = 0, 
           label = r2text('YR-I',b.regs |> filter(River == 'YR-I') |> pull(Adjusted_R2)), parse=T, size = 2) +
  annotate('text', x= 750, y = 60, col = palette_OkabeIto[6], hjust = 0, vjust = 0, 
           label = r2text('YR-O',b.regs |> filter(River == 'YR-O') |> pull(Adjusted_R2)), parse=T, size = 2) +
  annotate('text', x= 650, y = 75, col = palette_OkabeIto[8], hjust = 0, vjust = 0, 
           label = r2text('YR-S',b.regs |> filter(River == 'YR-S') |> pull(Adjusted_R2)), parse=T, size = 2) +
  scale_color_OkabeIto(order = c(1,4,5,6,8)) +
  scale_fill_OkabeIto(order = c(1,4,5,6,8)) +
  labs(x = "SpC"~(µS~cm^-1), y = "Chloride"~(mg~L^-1)) +
  # labs(x = "Specific Conductivity"~(mu~S~cm^-1)~"@ 25"*~degree*C~"\n", 
  #      y = "\nChloride Concentration"~(mg~L^-1)) +
  theme_minimal(base_size = 8) + theme(legend.title = element_blank())

# join figures
pr1 + inset_element(pr0,0,0.6,0.4,1)

(pr1 + inset_element(pr0,0,0.6,0.4,1)) + pr2 +
  plot_annotation(tag_levels = 'a', tag_suffix = ')') & 
  theme(plot.tag = element_text(size = 8), legend.position = "none")

# Save combo plot
ggsave('Figures/FX_regressions2.png', width = 6.5, height = 3, units = 'in', dpi = 500)



##########figure without SH########################################

pr1.5 = ggplot(sites.df |> filter(ID.x %in% c('PB','SW')), aes(value , chloride_mgL)) +
  geom_point(aes(color = ID.x, group = chloride_mgL), size = 0.75, shape = 21, fill = 'black') +
  geom_path(aes(color = ID.x, group = chloride_mgL), size = 0.3) +
  geom_smooth(method = "lm", se = FALSE, size = 1, aes(color = ID.x)) +
  xlim(0,3000) + ylim(0,700) +
  annotate('text', x= 2000, y = 100, col = palette_OkabeIto[2], vjust = 0,
           label = r2text('PB',b.regs |> filter(River == 'PB') |> pull(Adjusted_R2)), parse=T, size = 2) +
  annotate('text', x= 2500, y = 300, col = palette_OkabeIto[7], vjust = 0,
           label = r2text('SW',b.regs |> filter(River == 'SW') |> pull(Adjusted_R2)), parse=T, size = 2) +
  scale_color_OkabeIto(order = c(2,7)) +
  scale_fill_OkabeIto() +
  labs(x = "SpC"~(µS~cm^-1), y = "Chloride"~(mg~L^-1)) +
  theme_minimal(base_size = 8) + theme(legend.title = element_blank())


pr1.5 + pr2 +
  plot_annotation(tag_levels = 'a', tag_suffix = ')') & 
  theme(plot.tag = element_text(size = 8), legend.position = "none")

# Save combo plot
ggsave('Figures/FX_regressions.png', width = 6.5, height = 3, units = 'in', dpi = 500)

##Stats for regressions##
# source("Code/Functions/regression_stats_functions.R")
# #make a table of regression stats
# River_stats <- data.frame(
#   River = c(
#     "YR-I",
#     "SMC",
#     "DC",
#     "PB",
#     "YR-O"
#   ),
#   Slope = c(
#     slope(YRI_cl, YRI_cond),
#     slope(SMC_cl, SMC_cond),
#     slope(DC_cl, DC_cond),
#     slope(PB_cl, PB_cond),
#     slope(YRO_cl, YRO_cond)
#   ),
#   Intercept = c(
#     intercept(YRI_cl, YRI_cond),
#     intercept(SMC_cl, SMC_cond),
#     intercept(DC_cl, DC_cond),
#     intercept(PB_cl, PB_cond),
#     intercept(YRO_cl, YRO_cond)
#   ),
#   Adjusted_R2 = c(
#     r.sqr.lm(YRI_cl, YRI_cond),
#     r.sqr.lm(SMC_cl, SMC_cond),
#     r.sqr.lm(DC_cl, DC_cond),
#     r.sqr.lm(PB_cl, PB_cond),
#     r.sqr.lm(YRO_cl, YRO_cond)
#       ),
#   P_value = c("<0.001", "<0.001", "<0.001","<0.001","<0.001"
#                # pvalue(YRI_cl, YRI_cond),
#                # pvalue(SMC_cl, SMC_cond),
#                # pvalue(DC_cl, DC_cond),
#                # pvalue(PB_cl, PB_cond),
#                # pvalue(YRO_cl, YRO_cond)
#   )
# )


### output regression table #####
library(gt)
library(webshot)
## Broom regressions
b.slopes = sites.df %>% 
  nest(data = -ID.x) %>% 
  mutate(
    fit = map(data, ~ lm(chloride_mgL ~ value, data = .x)),
    tidied = map(fit, tidy),
    glanced = map(fit, glance)
  ) %>% 
  unnest(tidied) |> 
  select(River = ID.x, term, estimate) |> 
  pivot_wider(names_from = term, values_from = estimate) |> 
  rename(Intercept = `(Intercept)`, Slope = value) |> 
  mutate(Intercept = round(Intercept, 2), Slope = round(Slope, 2))

River_stats = 
  b.slopes |> left_join(b.regs) |> 
  mutate(P_value = if_else(P_value <= 0.001, "<0.001", as.character(P_value)))

gt_tbl <- gt(River_stats)
simpleregtable <- gt_tbl %>%
  cols_label(
    River = "River Name",
    Slope = "Slope",
    Intercept = "Intercept",
    Adjusted_R2 = html("R<sup>2<sup>"),
    P_value = "P-Value"
  ) %>%
  tab_header(
    title = "Chloride - Specific Conductivity Linear Regression Statistics"
    ); simpleregtable

#copy table as latex
as_latex(simpleregtable)


