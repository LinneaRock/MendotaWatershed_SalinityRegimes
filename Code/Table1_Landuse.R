
# Get road network
daneroads = roads(state = 'WI', county = 'DANE') |> st_transform(crs(shapefile.utm), year = 2021)
columbiaroads = roads(state = 'WI', county = 'COLUMBIA') |> st_transform(crs(shapefile.utm), year = 2021)
yahararoads = daneroads |> bind_rows(columbiaroads)

# Get land use function
getLandUse <- function(shapefile, name) {
  # Get 2019 NLCD
  a = get_nlcd(template = shapefile, label = name)
  shapefile.utm = st_transform(shapefile, crs = crs(a))
  ## crop and mask
  r2 <- crop(a, extent(shapefile.utm))
  r3 <- mask(r2, shapefile.utm)
  ## Check that it worked
  # plot(r3)
  
  r3.values = table(values(r3))
  # 81	Pasture/Hay-areas of grasses, legumes, or grass-legume mixtures planted for livestock grazing or the production of seed or hay crops, typically on a perennial cycle. Pasture/hay vegetation accounts for greater than 20% of total vegetation.
  # 82	Cultivated Crops -areas used for the production of annual crops, such as corn, soybeans, vegetables, tobacco, and cotton, and also perennial woody crops such as orchards and vineyards. Crop vegetation accounts for greater than 20% of total vegetation. This class also includes all land being actively tilled.
  # 21-24 Developed
  r3.values = as.data.frame(r3.values)
  developed = r3.values |> filter(Var1 %in% c(21:24)) |> summarise(Total = sum(Freq)) |> as.numeric()
  ag = r3.values |> filter(Var1 %in% c(81,82)) |> summarise(Total = sum(Freq)) |> as.numeric()
  total = r3.values |> summarise(Total = sum(Freq)) |> as.numeric()
  
  ## crop and mask
  r2 <- st_intersection(yahararoads, shapefile.utm)
  roadlengths = st_length(r2)
  totalroads = sum(roadlengths, na.rm = T)
  roaddensity = as.numeric(totalroads)/(as.numeric(st_area(shapefile.utm))*0.0001)
  
  output.df = data.frame(name = name, area.ha = round(as.numeric(st_area(shapefile.utm))*0.0001,2), 
                         ag = round(100*ag/total,2), developed = round(100*developed/total,2), 
                         roaddensity = round(roaddensity,2))
  return(output.df)
}


# Watersheds 
ws.YaharaI = st_read('GIS/Shapefiles_Watersheds/YN/ws_YN.shp') |> 
  getLandUse(name = 'YR-I')
ws.Yahara.O = st_read('GIS/Shapefiles_Watersheds/YI/ws_YI.shp') |> 
  getLandUse(name = 'YR-O')
ws.PB = st_read('GIS/Shapefiles_Watersheds/PBMS/ws_PBMS.shp') |> 
  getLandUse(name = 'PB')
ws.SM = st_read('GIS/Shapefiles_Watersheds/SMC/ws_SMC.shp') |> 
  getLandUse(name = 'SM')
ws.DC = st_read('GIS/Shapefiles_Watersheds/DC/ws_DC.shp') |> 
  getLandUse(name = 'DC')
ws.SW = st_read('GIS/Shapefiles_Watersheds/SW/ws_SW.shp') |> 
  getLandUse(name = 'SW')
ws.YRS = st_read('GIS/Shapefiles_Watersheds/YS/ws_YS.shp') |> 
  getLandUse(name = 'YR-S')
ws.SH = st_read('GIS/Shapefiles_Watersheds/SH/ws_SH.shp') |> 
  getLandUse(name = 'SH')

# Table output
a = ws.DC |> bind_rows(ws.PB) |> 
  bind_rows(ws.SH) |> 
  bind_rows(ws.SM) |> 
  bind_rows(ws.YaharaI) |> 
  bind_rows(ws.Yahara.O) |> 
  bind_rows(ws.SW) |> 
  bind_rows(ws.YRS) 

# total ag in Upper Yahara
a |> mutate(ag = area.ha*ag/100) |> 
  summarise(area = sum(area.ha), ag = sum(ag)) |> 
  mutate(ag/area)

# Latex code for easy copying to overleaf
xtable(a)

# Lake Mendota Outfalls
ME.watershed = st_read('GIS/Shapefiles_Yahara/Mendota_Basin.shp') 
SH.watershed = st_read('GIS/Shapefiles_Yahara/SpringHarborWateshed.shp') |> st_transform(crs(ME.watershed))
Mendota.outfalls = st_read('GIS/Shapefiles_Yahara/MendotaOutfalls.shp') |> st_transform(crs(ME.watershed))
upperYahara = st_read('GIS/Shapefiles_Yahara/UpperYahara.shp') |> st_transform(crs(ME.watershed))

sum(st_area(upperYahara)) * 0.0001 #hectares
st_area(SH.watershed)/sum(st_area(Mendota.outfalls))
sum(st_area(Mendota.outfalls))/st_area(ME.watershed)

#18.4%
