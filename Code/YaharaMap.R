library(sf)
library(tidyverse)
library(wesanderson)
library(ggspatial)
library(patchwork)
library(readxl)

# Background maps 
esri_land <- paste0('https://services.arcgisonline.com/arcgis/rest/services/NatGeo_World_Map/MapServer/tile/${z}/${y}/${x}.jpeg')
basemap <- paste0('https://tiles.wmflabs.org/osm-no-labels/${z}/${x}/${y}.png')

# Manually build dataframes with coordinates of sampling locations
gage.bb = data.frame(site = c('DC', 'PBMS', 'SMC','YR-I', 'YR-O'),
                     lat = c(43.14027778, 43.10333333, 43.14683333, 43.15083333, 43.08944444), 
                     lon = c(-89.44222222, -89.51166667, -89.43694444, -89.40194444,-89.36083333), 
                     fill = as.character(wes_palette("Darjeeling1", n = 5, type = "discrete")),
                     shape = c(22, 22, 22, 22, 22))
gage.bb.sf = st_as_sf(gage.bb, coords = c("lon", "lat"), 
                      crs = 4326)

lakebuoys <- data.frame(site = c('ME'), 
                        lat = c(43.097951), 
                        lon = c(-89.404744),
                        fill = c('gold'),
                        shape = c(21))
sites.sf <- st_as_sf(lakebuoys, coords = c("lon", "lat"),
                     crs = 4326)

# Watersheds 
ws.Yahara = st_read('GIS/Shapefiles_Watersheds/YN/ws_YN.shp')
ws.PB = st_read('GIS/Shapefiles_Watersheds/PBMS/ws_PBMS.shp')
ws.SM = st_read('GIS/Shapefiles_Watersheds/SMC/ws_SMC.shp')
ws.DC = st_read('GIS/Shapefiles_Watersheds/DC/ws_DC.shp')
ws.ME = st_read('GIS/Shapefiles_Watersheds/ME/ws_ME.shp')

# lakes
YaharaLakes = st_read('GIS/Shapefiles_Yahara/YaharaLakes_DaneCty.shp') %>% st_transform(4326)
NHD = st_read('GIS/Shapefiles_Yahara/Hydrolakes_DaneCountyArea.shp') %>% st_transform(4326)
YaharaWatershed = st_read('GIS/Shapefiles_Yahara/Yahara_Basin.shp') %>% st_transform(4326)
NHD_flowlines = st_read('GIS/Shapefiles_Yahara/NHDplus_Yahara.shp') %>% st_transform(4326)
YaharaFlowlines = st_read('GIS/Shapefiles_Yahara/Yahara_Rivers.shp')

##### Figure A #####
m1 = ggplot(YaharaWatershed) +
  annotation_map_tile(type = basemap, zoom = 12) +
  geom_sf(data = YaharaWatershed, alpha = 0.5, size = 0, fill = 'grey80') +
  geom_sf(data = ws.ME, alpha = 0.05, size = 0.3, fill = '#567DC8') +
  geom_sf(data = ws.Yahara, alpha = 0.1, size = 0.3, fill = '#581845') +
  geom_sf(data = ws.PB, alpha = 0.1, size = 0.3, fill = '#FF5733') +
  geom_sf(data = ws.DC, alpha = 0.1, size = 0.3, fill = '#900C3F') +
  geom_sf(data = ws.SM, alpha = 0.1, size = 0.3, fill = '#C70039') +
  geom_sf(data = YaharaFlowlines, color = 'lightsteelblue4', size = 0.2) +
  geom_sf(data = YaharaLakes,fill = alpha('lightsteelblue1',1), size = 0.2) +
  geom_sf(data = sites.sf, aes(fill = fill, shape = shape), size = 1, stroke = 0.2, #USGS gages
          show.legend = "point", inherit.aes = FALSE) +
  geom_sf(data = gage.bb.sf, aes(fill = fill, shape = shape), size = 2, stroke = 0.2, #USGS gages
          show.legend = "point", inherit.aes = FALSE) +
  scale_fill_identity() +
  scale_shape_identity() +
  annotation_scale(location = "bl", width_hint = 0.2, height = unit(0.05,'in'), text_cex = 0.6) + # Scale bar
  annotation_north_arrow(location = "tr", which_north = "true", 
                         pad_x = unit(-0.1, "in"),
                         # pad_y = unit(0.2, "in"),
                         height = unit(0.3,'in'), width = unit(0.5,'in'),
                         style = north_arrow_minimal) + # North Arrow
  theme_bw(base_size = 9) +
  theme(plot.background = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "transparent",colour = NA),
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        axis.title.x=element_blank(),axis.title.y=element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_sf(xlim = c(-89.6, -89.2), ylim = c(43, 43.37)) +
  NULL

##### Figure C #####
# Legend for map 
m2 = ggplot() +
  theme_void() +
  xlim(0,1) +
  ylim(0,1) +
  geom_point(aes(x = 0, y = seq(0.8,0.2,length.out = 6)), 
             size = 2, shape = c(rep(22,5),21), 
             fill = c(as.character(wes_palette("Darjeeling1", n = 5, type = "discrete")), 'gold')) +
  geom_text(aes(x=0.08,y=0.5,
                label='Dorn Creek (DC)\nSix Mile Creek (SMC)\nPheasant Branch (PB)\nYahara River (YR-I)\nYahara Outlet (YR-O)\nLake Mendota '),
            hjust = 0,
            size = 2.4) +
  xlab(NULL); m2

##### Figure B #####
## Inset Map of Wisconsin
states = st_read('GIS/Shapefiles_InsetMap/WI_borderstates.shp')
NHD.simple = st_read('GIS/Shapefiles_InsetMap/NHD_simple.shp')
wi.simple = st_read('GIS/Shapefiles_InsetMap/Wisconsin_State_Boundary_simple.shp')
greatLakes = st_read('GIS/Shapefiles_InsetMap/greatLakes.shp')

w1 = ggplot(wi.simple) +
  geom_sf(data = states, col = 'grey50', fill = 'grey90', alpha = 0.5, size = 0.2) +
  geom_sf(data = NHD.simple, col = NA, fill = 'lightsteelblue2') +
  geom_sf(data = greatLakes, fill = 'lightsteelblue2', col = 'lightsteelblue2') +
  geom_sf(data = st_as_sfc(st_bbox(YaharaWatershed)), color = '#FF0000', size = 0.3) + # Inset box
  # geom_sf(data = wi.simple) +
  coord_sf(ylim = c(42.3,47.5), xlim = c(-93, -86), expand = FALSE) +# limit axes
  theme_bw(base_size = 8) +
  theme(#plot.background = element_rect(fill = "transparent", colour = NA),
    panel.grid.minor = element_blank(), 
    panel.grid.major = element_blank(),
    axis.title.x=element_blank(),axis.title.y=element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    # plot.background = element_rect(fill = 'white', colour = 'black ', size = 0.3)
    )

# Map without lat/long
w2 = ggplot(wi.simple) +
  geom_sf(data = states, col = 'grey50', fill = 'grey90', alpha = 0.5, size = 0.2) +
  geom_sf(data = NHD.simple, col = NA, fill = 'lightsteelblue2') +
  geom_sf(data = greatLakes, fill = 'lightsteelblue2', col = 'lightsteelblue2') +
  geom_sf(data = st_as_sfc(st_bbox(YaharaWatershed)), color = '#FF0000', size = 0.3) + # Inset box
  # geom_sf(data = wi.simple) +
  coord_sf(ylim = c(42.3,47.5), xlim = c(-93, -86), expand = FALSE) +# limit axes
  theme_bw(base_size = 8) +
  theme(#plot.background = element_rect(fill = "transparent", colour = NA),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.minor = element_blank(), 
    panel.grid.major = element_blank(),
    axis.title = element_blank(),
    # axis.text.x = element_text(angle = 45, hjust = 1),
    # plot.background = element_rect(fill = 'white', colour = 'black ', size = 0.3)
  )

##### Figure D #####
# Chloride concentration in Lake Mendota ####
watershed <- read_xlsx("Data/Historical/YaharaHist.xlsx") %>%
  mutate(KA = as.numeric(KA))

m3 = ggplot(watershed) +
  geom_smooth(aes(Date, ME), color = "black", size = 0.3, se = FALSE) +
  geom_point(aes(Date, ME), fill = "gold", size = .7, shape = 21, stroke = 0.2) +
  labs(y = "Chloride Concentration"~(mg~L^-1)) + 
  theme_bw(base_size = 8) +
  theme(legend.title = element_blank(), 
        legend.text = element_text(size = 8),
        axis.title.x = element_blank(),
        legend.position = c(0.25,0.85),
        legend.key.height = unit(0.1,'cm'))


##### Combine all the plots into publishable figure #####
layout <- "
AABDD
AACDD
"

combo = m1 + w2 + m2 + m3 + 
  plot_layout(design = layout) + 
  plot_annotation(tag_levels = 'a', tag_suffix = ')') & 
  theme(plot.tag = element_text(size = 8)) & theme(plot.margin = margin(0,0,0,0, "cm"))
ggsave(plot = combo, filename = "Figures/F1_map.png", width = 6.5, height = 3, units = "in", dpi = 500)
 