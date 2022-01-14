#script to make plot of monthly chloride mass in each river 
library(colorblindr)

#call in chloride and discharge data:
source("Code/Estimated_Chloride_Conc_Mass.R") 

### Important: 
# The Spring Harbor storm sewer encompasses 29 of the 133 outfall basins within the Lake Mendota sewershed, i.e.
# sewers that directly contribute to the lake. Spring Harbor represents 11.8% of the total Lake
# Mendota sewershed area. Therefore, the SH data accounts for all stormsewers. 

#combine all mass data into monthly totals 
month.mass = all_ts_mass |> mutate(month = month(dateTime), year = year(dateTime)) |>
  group_by(ID, month, year) |> 
  summarise(chloride_mass_Mg = sum(chloride_mass_Mg), chloride_mass_Mg_high = sum(chloride_mass_Mg_high), 
            chloride_mass_Mg_low = sum(chloride_mass_Mg_low)) |> 
  mutate(date = ymd(paste(year,month,'01'))) |> 
  mutate_at(vars(starts_with('chloride_mass')), funs(if_else(ID %in% c("YR-O"), . * -1, .))) |>  ## YR-O is negative -- it is the outflow of the lake
  mutate_at(vars(starts_with('chloride_mass')), funs(if_else(ID %in% c("SH"), . / 0.118, .))) |>  ## YR-O is negative -- it is the outflow of the lake
  mutate(ID = if_else(ID %in% c("SH"), 's.s.', ID)) |> 
  ungroup() |> 
  filter(date >= as.Date('2020-01-01') & date <= as.Date('2021-03-01')) #do not include the partial months Dec 2019 or Apr 2021

month.mass.total = month.mass |> 
  group_by(month, year, date) |> 
  summarise(chloride_mass_Mg = sum(chloride_mass_Mg), chloride_mass_Mg_high = sum(chloride_mass_Mg_high), 
            chloride_mass_Mg_low = sum(chloride_mass_Mg_low)) |> 
  ungroup()

# mass balance in 2020?
month.mass.total |> filter(year == 2020) |> 
  summarise(chloride_mass_Mg = sum(chloride_mass_Mg))

month.mass.in = month.mass |> filter(ID != 'YR-O') |> 
  group_by(month, year, date) |> 
  summarise(chloride_mass_Mg = sum(chloride_mass_Mg), chloride_mass_Mg_high = sum(chloride_mass_Mg_high), 
            chloride_mass_Mg_low = sum(chloride_mass_Mg_low))

month.mass.out = month.mass |> filter(ID == 'YR-O') |> 
  group_by(month, year, date) |> 
  summarise(chloride_mass_Mg = sum(chloride_mass_Mg), chloride_mass_Mg_high = sum(chloride_mass_Mg_high), 
            chloride_mass_Mg_low = sum(chloride_mass_Mg_low))

# only plots mass for months which we have no missing data, i.e., do not include the partial months Dec 2019 or Apr 2021
p1 = ggplot(month.mass) +
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
  scale_y_continuous(n.breaks = 10); p1

ggsave(
  "Figures/F8_massbalance.png",
  height = 4,
  width = 6.25,
  units = "in",
  dpi = 500
)

