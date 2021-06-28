#script to generate shapefiles for subwatersheds and obtain land use characteristics
library(sf)
library(streamstats)
library(tidyverse)


#get shapefiles and land use characteristics from stream stats

#Yahara River inflow#####
wsYRI <-
  delineateWatershed(
    xlocation = -89.40194444,
    ylocation = 43.15083333,
    crs = 4326,
    includeparameters = "true",
    includeflowtypes = "true"
  )
leafletWatershed(wsYRI)
charsYRI <-
  computeChars(workspaceID = wsYRI$workspaceID, rcode = "WI")
writeShapefile(
  watershed = wsYRI,
  layer = "ws_YRI",
  dir = "Code/Map/shapefiles/YRI",
  what = "boundary"
)



#Sixmile Creek####
wsSMC <-
  delineateWatershed(
    xlocation = -89.43694444,
    ylocation = 43.14683333,
    crs = 4326,
    includeparameters = "true",
    includeflowtypes = "true"
  )
leafletWatershed(wsSMC)
charsSMC <-
  computeChars(workspaceID = wsSMC$workspaceID, rcode = "WI")
writeShapefile(
  watershed = wsSMC,
  layer = "ws_SMC",
  dir = "Code/Map/shapefiles/SMC",
  what = "boundary"
)



#Dorn Creek####
wsDC <-
  delineateWatershed(
    xlocation = -89.44222222,
    ylocation = 43.14027778,
    crs = 4326,
    includeparameters = "true",
    includeflowtypes = "true"
  )
leafletWatershed(wsDC)
charsDC <-
  computeChars(workspaceID = wsDC$workspaceID, rcode = "WI")
writeShapefile(
  watershed = wsDC,
  layer = "ws_DC",
  dir = "Code/Map/shapefiles/DC",
  what = "boundary"
)



#Pheasant Branch Creek####
wsPB <-
  delineateWatershed(
    xlocation = -89.51166667,
    ylocation = 43.10333333,
    crs = 4326,
    includeparameters = "true",
    includeflowtypes = "true"
  )
leafletWatershed(wsPB)
charsPB <-
  computeChars(workspaceID = wsPB$workspaceID, rcode = "WI")
writeShapefile(
  watershed = wsPB,
  layer = "ws_PB",
  dir = "Code/Map/shapefiles/PB",
  what = "boundary"
)



#Yahara River outflow####
wsYRO <-
  delineateWatershed(
    xlocation = -89.36083333,
    ylocation = 43.08944444,
    crs = 4326,
    includeparameters = "true",
    includeflowtypes = "true"
  )
leafletWatershed(wsYRO)
charsYRO <-
  computeChars(workspaceID = wsYRO$workspaceID, rcode = "WI")
writeShapefile(
  watershed = wsYRO,
  layer = "ws_YRO",
  dir = "Code/Map/shapefiles/YRO",
  what = "boundary"
)


 
#get road densisty in each subwatershed####
#download Open Street Map data and read in
WI_roads <- st_read("C:/Users/linne/Downloads/WIroads/wisconsin-latest-free.shp/gis_osm_roads_free_1.shp")

#clip roads to each subwatershed
roads_in_wsYRI <- st_intersection(WI_roads, wsYRI)   
roads_in_wsSMC <- st_intersection(WI_roads, wsSMC)       
roads_in_wsDC <- st_intersection(WI_roads, wsDC)
roads_in_wsPB <- st_intersection(WI_roads, wsPB)   
roads_in_wsYRO <- st_intersection(WI_roads, wsYRO) 


#calculate road densities
areaYRI <- st_area(wsYRI) / 10000 #[m^2 to ha]
lengthroadsYRI <- sum(st_length(roads_in_wsYRI))
road_densityYRI <- lengthroadsYRI/areaYRI

areaSMC <- st_area(wsSMC) / 10000 #[m^2 to ha]
lengthroadsSMC <- sum(st_length(roads_in_wsSMC))
road_densitySMC <- lengthroadsSMC/areaSMC

areaDC <- st_area(wsDC) / 10000 #[m^2 to ha]
lengthroadsDC <- sum(st_length(roads_in_wsDC))
road_densityDC <- lengthroadsDC/areaDC

areaPB <- st_area(wsPB) / 10000 #[m^2 to ha]
lengthroadsPB <- sum(st_length(roads_in_wsPB))
road_densityPB <- lengthroadsPB/areaPB

areaYRO <- st_area(wsYRO) / 10000 #[m^2 to ha]
lengthroadsYRO <- sum(st_length(roads_in_wsYRO))
road_densityYRO <- lengthroadsYRO/areaYRO




#Now, combine land use characteristics with road density in a single dataset and save as RDS for easy retrieval later
build_dataset <- function(ws_, name, id, road_density_) {
   
   ws_ <- ws_ %>%
      dplyr::select(DRNAREA, DEVNLCD01, FOREST, LC01CRPHAY, LC01HERB, LC01WATER, WETLAND, PRECIP) %>%
      mutate(DRNAREA = DRNAREA * 258.999, #square miles to hectares
             PRECIP = PRECIP * 2.54, #inches to centimeters
             river = name,
             ID = id) %>%
      mutate(road_density_mha = as.numeric(road_density_))
   
}


ws_all <- build_dataset(wsYRI, "Yahara River inflow", "YR-I", road_densityYRI) %>%
   bind_rows(build_dataset(wsYRO, "Yahara River outflow", "YR-O", road_densityYRO)) %>%
   bind_rows(build_dataset(wsSMC, "Sixmile Creek", "SMC", road_densitySMC)) %>%
   bind_rows(build_dataset(wsDC, "Dorn Creek", "DC", road_densitySMC)) %>%
   bind_rows(build_dataset(wsPB, "Pheasant Branch Creek", "PB", road_densityPB)) 

write_rds(ws_all, "Code/Map/watershed_info.rds")
