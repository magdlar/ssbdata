options(stringsAsFactors = FALSE)
library("maps") # I get a world map from this package
library("raster") # Use the intersect function
library("sp") # Required for rgdal?
library("maptools") # I turn the map from maps into a spatial polygon with map2SpatialPolygons
library("rgdal") # I use readOGR to read the county level map file
library("ggplot2") # Plotting the maps
library("broom") # Tidying the spatial polygons dataframe
library("dplyr") # Piping
library("mapproj") # Required by ggplot2
library("rgeos") # I use gSimplify to reduce the complexity of the county map

## read county map, convert coordinates, fix names
fylker <- readOGR("Basisdata_0000_Norge_25833_Fylker_GEOJSON.geojson", layer="administrative_enheter.fylke")
fylker <- spTransform(fylker,CRS("+init=epsg:4326"))
fylker@data$navn <- c("Rogaland","Finnmark","Troms","Møre og Romsdal","Hordaland","Telemark","Vestfold","Østfold","Buskerud","Oslo","Akershus","Oppland","Hedmark","Nordland","Aust-Agder","Vest-Agder","Trøndelag","Sogn og Fjordane")
fylker <- SpatialPolygonsDataFrame(gSimplify(fylker, tol=.03, topologyPreserve = FALSE), data=fylker@data) # Simplify the geometry to render the map faster

## get a world map, convert to shape
world_map <- map("world", fill=TRUE)
world_map <- map2SpatialPolygons(world_map, IDs=world_map$names)

## new map is the intersection of the two maps
fylker_map <- raster::intersect(fylker,world_map)

## create a dataframe from the shapefile
map_df <- tidy(fylker_map)
map_df$region <- sapply(map_df$id,FUN=function(x){fylker_map@data$navn[as.numeric(x)]}) %>% as.character() # add regions to the df so we have something to join on

## import data and merge it into the map dataframe
sykprosent <- readRDS(file="sykprosent.RDS")
arbledighet <- readRDS(file="arbledighet.RDS")

map_df <- right_join(map_df,arbledighet)
map_df <- left_join(map_df,sykprosent,by=c("region","år"))
map_df <- map_df %>%
  rename("Region"="region","Year"="år","Unemployment"="arbeidsledighet", "Absenteeism"="sykprosent") %>%
  dplyr::select(c(1:7),"Region","Year","Unemployment","Absenteeism")

## Save the dataframe to use in the shinyapp
saveRDS(map_df,file="mapdf.RDA")