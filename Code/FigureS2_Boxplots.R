## First run 00_DataLoad.R ##

#bind river conductivity data together
all_river_cond <- bind_rows(YRI_cond, PB_cond, DC_cond, SMC_cond, YRO_cond, 
                            SW_cond, YRS_cond) 
all_river_cond$ID = factor(all_river_cond$ID,
                           levels = c("DC", "PB", "SMC", "YR-I", "YR-O", "SW", "YR-S"))

#bind chloride data together
all_river_cl <- bind_rows(YRI_cl, PB_cl, DC_cl, SMC_cl, YRO_cl, 
                          SW_cl, YRS_cl)
all_river_cl$ID = factor(all_river_cl$ID,
                         levels = c("DC", "PB", "SMC", "YR-I", "YR-O", "SW", "YR-S"))

################### SpC boxplot #########################
df.cond = all_river_cond |> rbind(SH_cond) |> filter(!is.na(MovingAverage_SpCond_uScm))
# Tukey without SH # analysis of variance
df.cond.T = df.cond |> 
  mutate(ID = str_replace_all(ID, "[[:punct:]]", " "))

anova <- aov(MovingAverage_SpCond_uScm ~ ID, data = df.cond.T)
Tresult = TukeyHSD(anova, ordered = T)
# compact letter display
cld <- multcompLetters4(anova, Tresult)

# table with factors and 3rd quantile
dt <- group_by(df.cond.T, ID) %>%
  summarise(w=mean(MovingAverage_SpCond_uScm), sd = sd(MovingAverage_SpCond_uScm)) %>%
  arrange(desc(w))

# extracting the compact letter display and adding to the Tk table
cld <- as.data.frame.list(cld$ID) |> 
  rownames_to_column("ID")
dt = dt |>  mutate(cld = cld$Letters) |> 
  mutate(ID = str_replace(ID, " ", "-"))

p1 = ggplot(df.cond) +
  geom_boxplot(aes(y = MovingAverage_SpCond_uScm, x = ID, fill = ID), 
               size = 0.1, outlier.size = 0.1) +
  scale_fill_OkabeIto(order = c(1:2,4:8,3)) +
  geom_text(data = dt, aes(x = ID, label = cld, y = w + sd), vjust = -6, hjust = -0.5, size = 2) +
  theme_minimal(base_size = 8) +
  labs(y = "SpC"~(µS~cm^-1), x = "") +
  scale_y_log10(expand = c(0,0.02)) +
  theme(axis.title.x = element_blank(), 
        legend.position = 'none')

################### chloride boxplot #########################
df.cl = all_river_cl |> rbind(SH_cl) |> filter(!is.na(chloride_mgL))
# Tukey without SH # analysis of variance
df.cl.T = df.cl |> filter(ID != 'SH') |> 
  mutate(ID = str_replace_all(ID, "[[:punct:]]", " "))
         
anova <- aov(chloride_mgL ~ ID, data = df.cl.T)
Tresult = TukeyHSD(anova, ordered = T)
# compact letter display
cld <- multcompLetters4(anova, Tresult)

# table with factors and 3rd quantile
dt <- group_by(df.cl.T, ID) %>%
  summarise(w=mean(chloride_mgL), sd = sd(chloride_mgL)) %>%
  arrange(desc(w))

# extracting the compact letter display and adding to the Tk table
cld <- as.data.frame.list(cld$ID) |> 
  rownames_to_column("ID")
dt = dt |>  mutate(cld = cld$Letters) |> 
  mutate(ID = str_replace(ID, " ", "-"))


p2 = ggplot(df.cl) +
  geom_boxplot(aes(y = chloride_mgL, x = ID, fill = ID), 
               size = 0.1, outlier.size = 0.1) +
  scale_fill_OkabeIto(order = c(1:2,4:8,3)) +
  geom_text(data = dt, aes(x = ID, label = cld, y = w + sd), vjust = -6, hjust = -0.5, size = 2) +
  theme_minimal(base_size = 8) +
  labs(y = "Chloride"~(mg~L^-1), x = "") +
  scale_y_log10(expand = c(0,0.02)) +
  theme(axis.title.x = element_blank(), 
        legend.position = 'none')

############ Join plots ############
p1 / p2 +
  plot_annotation(tag_levels = 'a', tag_suffix = ')') & 
  theme(plot.tag = element_text(size = 8))
# Save combo plot
ggsave('Figures/Supplemental/FigureS2_boxplots.png', width = 6.5, height = 4, units = 'in', dpi = 500)


