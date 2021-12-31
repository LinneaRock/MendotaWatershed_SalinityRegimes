#Script to create linear regressions from known chloride concentrations and 
#specific conductivity from grab sampling

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

#bind together 
all_river_linreg <- bind_rows(a, b, c, d, e, f, g)

# Check EC similarities
ggplot(all_river_linreg) +
  geom_point(aes(x = SpCond_uScm.x, y = MovingAverage_SpCond_uScm)) +
  geom_abline()


#plot regressions on one graph
library(colorblindr)

ggplot(all_river_linreg, aes(SpCond_uScm.y , chloride_mgL)) +
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

lakes = all_river_linreg |> 
  # filter(ID.x %in% c('YR-O','YR-S')) |> 
  select(date, ID.x, chloride_mgL, SpCond_uScm.x, MovingAverage_SpCond_uScm) |> 
  filter(!is.na(SpCond_uScm.x) | !is.na(MovingAverage_SpCond_uScm)) |> 
  pivot_longer(cols = SpCond_uScm.x: MovingAverage_SpCond_uScm) |> 
  arrange(ID.x, chloride_mgL)

ggplot(lakes, aes(value , chloride_mgL)) +
  geom_point(aes(color = ID.x, group = chloride_mgL), size = 0.75, shape = 21, fill = 'black') +
  geom_path(aes(color = ID.x, group = chloride_mgL), size = 0.5) +
  geom_smooth(method = "lm", se = FALSE, size = 1, aes(color = ID.x)) +
  xlim(0,NA) + ylim(0,NA) +
  scale_color_OkabeIto() +
  scale_fill_OkabeIto() +
  labs(x = "Specific Conductivity"~(mu~S~cm^-1)~"@ 25"*~degree*C~"\n", 
       y = "\nChloride Concentration"~(mg~L^-1)) +
  theme_minimal() + theme(legend.title = element_blank())

ggplot(lakes |> filter(ID.x %in% c('YR-O','YR-S')), aes(value , chloride_mgL)) +
  geom_point(aes(color = ID.x, group = chloride_mgL), size = 0.75, shape = 21, fill = 'black') +
  geom_path(aes(color = ID.x, group = chloride_mgL), size = 0.5) +
  geom_smooth(method = "lm", se = FALSE, size = 1, aes(color = ID.x)) +
  xlim(0,NA) + ylim(0,NA) +
  scale_color_OkabeIto() +
  scale_fill_OkabeIto() +
  labs(x = "Specific Conductivity"~(mu~S~cm^-1)~"@ 25"*~degree*C~"\n", 
       y = "\nChloride Concentration"~(mg~L^-1)) +
  theme_minimal() + theme(legend.title = element_blank())

##Stats for regressions##
source("Code/Functions/regression_stats_functions.R")


#make a table of regression stats
River_stats <- data.frame(
  River = c(
    "YR-I",
    "SMC",
    "DC",
    "PB",
    "YR-O"
  ),
  Slope = c(
    slope(YRI_cl, YRI_cond),
    slope(SMC_cl, SMC_cond),
    slope(DC_cl, DC_cond),
    slope(PB_cl, PB_cond),
    slope(YRO_cl, YRO_cond)
  ),
  Intercept = c(
    intercept(YRI_cl, YRI_cond),
    intercept(SMC_cl, SMC_cond),
    intercept(DC_cl, DC_cond),
    intercept(PB_cl, PB_cond),
    intercept(YRO_cl, YRO_cond)
  ),
  Adjusted_R2 = c(
    r.sqr.lm(YRI_cl, YRI_cond),
    r.sqr.lm(SMC_cl, SMC_cond),
    r.sqr.lm(DC_cl, DC_cond),
    r.sqr.lm(PB_cl, PB_cond),
    r.sqr.lm(YRO_cl, YRO_cond)
      ),
  P_value = c("<0.001", "<0.001", "<0.001","<0.001","<0.001"
               # pvalue(YRI_cl, YRI_cond),
               # pvalue(SMC_cl, SMC_cond),
               # pvalue(DC_cl, DC_cond),
               # pvalue(PB_cl, PB_cond),
               # pvalue(YRO_cl, YRO_cond)
  )
)

library(gt)
library(webshot)

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


