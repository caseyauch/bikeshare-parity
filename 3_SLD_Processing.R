#----------------------SET UP
#load packages
library(terra)
library(sf)
library(tidyverse)
library(tmap)

#set working directory
setwd("C:/Users/casey/OneDrive - Tufts/Documents/2022 Thesis/Data/Data_Clean") 

#import data
stations <- read_sf('stations_sld.shp')  %>% subset(select=c(1))
plot(st_geometry(stations))
stations_crs <- st_crs(stations)# ma state plane

#import sld data and subset mass
sld <- st_read("../SmartLocationDatabase.gdb")
mass <- sld %>% filter(STATEFP == 25)
# st_write(mass, "mass_sld.shp", append=FALSE)

# mass <- read_sf('mass_sld.shp')
plot(st_geometry(mass))
mass_crs <- st_crs(mass)

#check projections
stations_crs == mass_crs #FALSE

#subset columns from sld
mass_subset <- mass[,c("D1A", "D1C", "D2B_E8MIXA", "D2A_EPHHM", "D3AAO", "D3AMM", "D3APO", "D3BAO", "D3BMM4", "D3BPO4", "D4A", "D4D", "D5AR", "D5BR")]
bos_cbsa <- mass %>% filter(CBSA==14460)

bos_cbsa <- mass %>% filter(COUNTYFP=='025')
plot(st_geometry(bos_cbsa))
plot(st_geometry(stations), add=T)

#fix projections
mass_proj <- st_transform(mass_subset, stations_crs)

tm_shape(bos_cbsa) +tm_fill(title="Test")+tm_borders()

#create buffer around stations for spatial join
buffer_500 <- st_buffer(stations, 500)
plot(st_geometry(buffer_500))
plot(st_geometry(stations), add=T)

ggplot() + geom_sf(data = stations) + geom_sf(data = buffer_500,color = 'red')

# Then run spatial join
studyarea_500 <- st_filter(mass, buffer_500, largest=T)
plot(st_geometry(studyarea_500))
plot(st_geometry(buffer_500), add=T)
plot(st_geometry(stations), add=T)

st_write(studyarea_500, "studyarea.shp", append=FALSE)

studyarea_reduced <- studyarea_500[,c("D1A", "D1C", "D2B_E8MIXA", "D2A_EPHHM", "D3AAO", "D3AMM", "D3APO", "D3BAO", "D3BMM4", "D3BPO4", "D4A", "D4D", "D5AR", "D5BR")]
st_write(studyarea_reduced, "studyarea_reduced.shp", append=FALSE)

stations_sld <- st_join(stations, mass_subset)
stations_276 <- stations_sld %>% drop_na()
stations_ids_276 <- stations[,1] %>% rename('station_id'=1)
st_write(stations_276, "stations_sld_276.shp", append=FALSE)
st_write(stations_ids_276, "stations_ids_276.shp", append=FALSE)
