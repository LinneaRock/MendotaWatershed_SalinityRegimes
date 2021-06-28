#script to create study area map and land use characteristics talbe

#read in watershed info
ws_all <- read_rds("Code/Map/watershed_info.rds") 


#Study Area Map####
library(tidyverse)
library(sf)
library(ggspatial)
library(ggrepel)

## Esri basemap URLs ####
esri_land <-  paste0('https://services.arcgisonline.com/arcgis/rest/services/NatGeo_World_Map/MapServer/tile/${z}/${y}/${x}.jpeg')
world_gray <- paste0('https://services.arcgisonline.com/arcgis/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/${z}/${y}/${x}.jpeg')

# Manually build dataframes with cooridnates of sampling locations
gage.bb = data.frame(site = c('YR-I', 'SMC', 'DC', 'PB', 'YR-O'),lat = c(43.15083333, 43.14683333, 43.14027778, 43.10333333, 43.08944444), lon = c(-89.40194444, -89.43694444, -89.44222222, -89.51166667, -89.36083333))
gage.bb.sf = st_as_sf(gage.bb, coords = c("lon", "lat"), 
                      crs = 4326)

#creating this for labels
label <- gage.bb.sf %>%
  mutate(lat = unlist(map(label$geometry, 1)),
         long = unlist(map(label$geometry, 2)))

ggplot(gage.bb.sf) +
  annotation_map_tile(type = world_gray, zoom = 12) + 
  geom_sf(gage.bb.sf, color = "black", size = 2, mapping = aes()) +
  geom_sf(ws_all, mapping = aes(), color = "black", fill = NA) +
  geom_label_repel(data = label, mapping = aes(lat, long, label = site), size = 2.25) +
  theme_minimal() +
  labs(x = '', y = '')

ggsave("Figures/F1_Map.png", width = 6.25, height = 4.25, units = "in")



#Subwatershed characteristics table####
ws_info_aggregated <- ws_all %>%
  mutate(undeveloped = FOREST + LC01HERB + WETLAND) %>% #combining wetlands, forest, and herbaceous land into 'undeveloped' category
  dplyr::select(river, ID, DRNAREA, LC01WATER, DEVNLCD01, LC01CRPHAY, undeveloped, road_density_mha) %>%
  arrange(ID) %>%
  mutate(geometry = NULL) %>%
  mutate_if(is.numeric, round, digits = 2)
 



library(webshot)
library(gt)

# Make the tables
gt_tbl <- gt(ws_info_aggregated)
ws_usgs <- gt_tbl %>%
  cols_label(
    river = "River Name",
    ID = "Site ID",
    DRNAREA = "Drainage Area ha",
    LC01WATER = "Open Water Area %",
    DEVNLCD01 = "Developed Area %",
    LC01CRPHAY = "Cropland Area %",
    undeveloped = "Undeveloped Area %",
    road_density_mha = html("Road Density m ha<sup>-1<sup>")
  ) %>%
  tab_header(
    title = "Subwatershed characteristics"
  ); ws_usgs

gtsave(data = ws_usgs, "Figures/T1_watershed_characteristics.png", expand = 10, zoom = 10)

