#script to find max values

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

# Highest values
maxValues = all_river_cl |> group_by(ID) |> 
  mutate(cl.max = max(chloride_mgL, na.rm = T)) |> 
  filter(chloride_mgL == cl.max) |> 
  left_join(all_river_cond |> group_by(ID) |> 
              summarise(cond.max = max(MovingAverage_SpCond_uScm, na.rm = T))) |> 
  select(ID, dateTime, cl.max, SpCond_uScm, cond.max) |> 
  arrange(maxValues)

xtable(maxValues)

SH_cl |> #filter(dateTime > as.POSIXct('2019-12-01')) |> 
  mutate(cl.max = max(chloride_mgL, na.rm = T)) |> 
  filter(chloride_mgL == cl.max)

SH_cond |> #filter(dateTime > as.POSIXct('2019-12-01')) |> 
  mutate(cond.max = max(MovingAverage_SpCond_uScm, na.rm = T)) |> 
  filter(MovingAverage_SpCond_uScm == cond.max)


# Mean concentrations at each site
all_river_cond |> rbind(SH_cond) |> group_by(ID) |> 
  summarise(meanSpC = mean(SpCond_uScm, na.rm = T))
