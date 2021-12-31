library(tidyverse)
library(lubridate)

# getOption('timeout')
# options(timeout=6000)
# download.file(url = 'https://nwis.waterdata.usgs.gov/wi/nwis/uv?cb_00010=on&cb_00045=on&cb_00060=on&cb_00065=on&cb_00095=on&format=rdb&site_no=05427965&period=&begin_date=2012-12-01&end_date=2021-12-08', 
#               destfile = 'SH_SPC_raw.txt')


sh_spc = read_tsv('Data/USGS_SH/SH_SPC.txt') %>% select(datetime, `158192_00095`) %>% 
  rename(spc = `158192_00095`) %>% 
  filter(!is.na(spc))

sh_chloride = read_tsv('Data/USGS_SH/SH_Cl.txt') %>% 
  select(sample_dt, sample_tm, p00940) %>% 
  rename(chloride = p00940) %>% 
  mutate(datetime = ymd_hms(paste(sample_dt,sample_tm))) %>% 
  mutate(datetime_15 = lubridate::round_date(datetime, "15 minutes")) %>% 
  filter(!is.na(chloride)) %>%
  left_join(sh_spc)


ggplot(sh_chloride) +
  geom_point(aes(x = spc, y = chloride)) +
  geom_smooth(aes(x = spc, y = chloride), method = 'lm')

summary(lm(chloride ~ spc, data = sh_chloride))
summary(lm(spc ~ chloride, data = sh_chloride))

Cl = spc*0.32 - 17

