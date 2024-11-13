# Spatial analysis in R
# Han Olff nov 2021

rm(list = ls())
# set the working directory where your GIS data are located
setwd("C:/Users/praam/Documents/Master/APCE/APCE2024GIS")

# restore the libraries of the project 
renv::restore()


# load the different libraries
library(terra)       # for working with raster data
library(tidyterra)   # for adding terra objects to ggplot
library(ggspatial)  # for scale bars
library(sf)          # for vector data objects
library(tidyverse)   # ggplot, dplyr etc
library(scales)      # for oob (out of bounds) scale
library(ggnewscale) # for using multiple color fill scales in ggplot
library(patchwork)  # for combining multiple ggplots in one panel plot

# explore color palettes
# also see https://www.datanovia.com/en/blog/top-r-color-palettes-to-know-for-great-data-visualization/
# Base R palettes
barplot(rep(1,10), col = grey.colors(10))
mycolors<-c("red", "white", "blue")
mycolors
barplot(rep(1,10), col = rev(topo.colors(10))) # rev turns the scale arround
barplot(rep(1,10), col = rev(terrain.colors(10)))
#rev means reverse the palette order
library(RColorBrewer) 
RColorBrewer::display.brewer.all()
barplot(rep(1,10), col = RColorBrewer::brewer.pal(10, "Spectral"))

barplot(rep(1,10), col = RColorBrewer::brewer.pal(10, "BrBG"))
library(viridis)
barplot(rep(1,10), col = viridis::viridis(10))
barplot(rep(1,10), col = viridis::plasma(10))
barplot(rep(1,10), col = viridis::heat(10))
viridis::plasma(10)
library(wesanderson)
barplot(rep(1,10), col = rev(wesanderson::wes_palette("Zissou1", 10, type = "continuous")))
pal_zissou1<-rev(wesanderson::wes_palette("Zissou1", 10, type = "continuous"))
pal_zissou2<-wesanderson::wes_palette("Zissou1", 10, type = "continuous")
pal_zissou1

# load the vector data (gpkg) for the whole ecosystem
sf::st_layers("./2022_protected_areas/protected_areas.gpkg")
protected_areas<-terra::vect("./2022_protected_areas/protected_areas.gpkg",
            layer="protected_areas_2022") # read protected area boundaries)
sf::st_layers("./2022_rivers/rivers_hydrosheds.gpkg")
rivers<-terra::vect("./2022_rivers/rivers_hydrosheds.gpkg",
                    layer="rivers_hydrosheds")
sf::st_layers("./lakes/lakes.gpkg")
lakes<-terra::vect("./lakes/lakes.gpkg",
                   layer="lakes")  
sf::st_layers("./studyarea/studyarea.gpkg")
studyarea<-terra::vect("./studyarea/studyarea.gpkg",
                              layer="my_study_area")


# load the raster (tif) data for the whole ecosystem
woodybiom<-terra::rast("./2016_WoodyVegetation/TBA_gam_utm36S.tif")
hillshade<-terra::rast("./2023_elevation/hillshade_z5.tif")
rainfall<-terra::rast("./rainfall/CHIRPS_MeanAnnualRainfall.tif")
elevation<-terra::rast("./2023_elevation/elevation_90m.tif")
disttoriver<-terra::rast("./2022_rivers/DistanceToRiver.tif")
lastyearburn<-terra::rast("./fires/YearLastBurned.tif")
burnfreq<-terra::rast("./fires/BurnFreq.tif")

# inspect the data 
class(protected_areas)
class(elevation)
plot(protected_areas)
plot(elevation)
plot(protected_areas, add=T)

# set the limits of the map to show (xmin, xmax, ymin, ymax in utm36 coordinates)
xlimits<-c(550000,900000)
ylimits<-c(9600000,9950000)

# plot the woody biomass map that you want to predict
woody_map<-ggplot()+
  tidyterra::geom_spatraster(data=woodybiom) +  #Add color scale
  scale_fill_gradientn(colours = rev(terrain.colors(6)),
                       limits=c(0.77,6.55),
                        oob=squish, #everything outside scale become either largest or smallest color
                       name="TBA/ha") +
  tidyterra::geom_spatvector(data=protected_areas, fill=NA, linewidth=0.5)+
  tidyterra::geom_spatvector(data=rivers, col="blue", linewidth=0.5)+
  tidyterra::geom_spatvector(data=lakes, fill="lightblue", linewidth=0.5)+
  tidyterra::geom_spatvector(data=studyarea, fill=NA, linewidth=0.5, col="red")+
  labs(title="Woody biomass") + 
  coord_sf(xlimits,ylimits,datum = sf::st_crs(32736))+
  theme(axis.text=element_blank(),
        axis.ticks=element_blank())+
  ggspatial::annotation_scale(location = "bl", width_hint = 0.2)


# plot the rainfall map

rainfall_map<-ggplot()+
  tidyterra::geom_spatraster(data=rainfall) +  #Add color scale
  scale_fill_gradientn(colours = pal_zissou1,
                       limits=c(364,2054),
                       oob=squish, #everything outside scale become either largest or smallest color
                       name="mm/year") +
  tidyterra::geom_spatvector(data=protected_areas, fill=NA, linewidth=0.5)+
  tidyterra::geom_spatvector(data=rivers, col="blue", linewidth=0.5)+
  tidyterra::geom_spatvector(data=lakes, fill="lightblue", linewidth=0.5)+
  tidyterra::geom_spatvector(data=studyarea, fill=NA, linewidth=1, col="red")+
  labs(title="Rainfall") + 
  coord_sf(xlimits,ylimits,datum = sf::st_crs(32736))+
  theme(axis.text=element_blank(),
        axis.ticks=element_blank())+
  ggspatial::annotation_scale(location = "bl", width_hint = 0.2)

# plot the elevation map

elevation_map<-ggplot()+
  tidyterra::geom_spatraster(data=elevation) +  #Add color scale
  scale_fill_gradientn(colours = terrain.colors(10),
                       limits=c(500,2100),
                       oob=squish, #everything outside scale become either largest or smallest color
                       name="meters") +
  tidyterra::geom_spatvector(data=protected_areas, fill=NA, linewidth=0.5)+
  tidyterra::geom_spatvector(data=rivers, col="blue", linewidth=0.5)+
  tidyterra::geom_spatvector(data=lakes, fill="lightblue", linewidth=0.5)+
  tidyterra::geom_spatvector(data=studyarea, fill=NA, linewidth=1, col="red")+
  labs(title="Elevation") + 
  coord_sf(xlimits,ylimits,datum = sf::st_crs(32736))+
  theme(axis.text=element_blank(),
        axis.ticks=element_blank())+
  ggspatial::annotation_scale(location = "bl", width_hint = 0.2)

# combine the different maps  into one composite map using the patchwork library
# and save it to a high resolution png

composite_map <- woody_map + rainfall_map + elevation_map+plot_layout(ncol=2) 
plot(composite_map)
ggsave("C:/Users/praam/Documents/github/APCE2024/spatial-r-Gijsbert-Pr/Figure/composite_map.png", composite_map, width=20, height=20, units="cm")

############################
### explore your study area
# set the limits of your study area
xlimits<-c(820000,870000)
ylimits<-c(9690000,9720000)
saExt<-terra::ext(studyarea)

# crop the woody biomass to the extent of the studyarea

woodybiom_sa<-terra::crop(woodybiom, saExt)
rainfall_sa<-terra::crop(rainfall, saExt)
elevation_sa<-terra::crop(elevation, saExt)
distancetoriver_sa<-terra::crop(disttoriver, saExt)
lastyearburn_sa<-terra::crop(lastyearburn, saExt)
burnfreq_sa<-terra::crop(burnfreq, saExt)

# plot the woody biomass
woody_map_sa<-ggplot()+
  tidyterra::geom_spatraster(data=woodybiom_sa) +  #Add color scale
  scale_fill_gradientn(colours = rev(terrain.colors(6)),
                       limits=c(0.77,6.55),
                       oob=squish, #everything outside scale become either largest or smallest color
                       name="TBA/ha") +
  tidyterra::geom_spatvector(data=protected_areas, fill=NA, linewidth=0.5)+
  tidyterra::geom_spatvector(data=rivers, col="blue", linewidth=0.5)+
  tidyterra::geom_spatvector(data=lakes, fill="lightblue", linewidth=0.5)+
  tidyterra::geom_spatvector(data=studyarea, fill=NA, linewidth=0.5, col="red")+
  labs(title="Woody biomass") + 
  coord_sf(xlimits,ylimits,expand=F,datum = sf::st_crs(32736))+
  theme(axis.text=element_blank(),
        axis.ticks=element_blank())+
  ggspatial::annotation_scale(location = "bl", width_hint = 0.2)

# make maps also for the other layers that you found

#rainfall
rainfall_map_sa<-ggplot()+
  tidyterra::geom_spatraster(data=rainfall_sa) +  #Add color scale
  scale_fill_gradientn(colours = pal_zissou1,
                       limits=c(364,2054),
                       oob=squish, #everything outside scale become either largest or smallest color
                       name="mm/year") +
  tidyterra::geom_spatvector(data=protected_areas, fill=NA, linewidth=0.5)+
  tidyterra::geom_spatvector(data=rivers, col="blue", linewidth=0.5)+
  tidyterra::geom_spatvector(data=lakes, fill="lightblue", linewidth=0.5)+
  tidyterra::geom_spatvector(data=studyarea, fill=NA, linewidth=1, col="red")+
  labs(title="Rainfall") + 
  coord_sf(xlimits,ylimits, expand=F, datum = sf::st_crs(32736))+
  theme(axis.text=element_blank(),
        axis.ticks=element_blank())+
  ggspatial::annotation_scale(location = "bl", width_hint = 0.2)

#elevation
elevation_map_sa<-ggplot()+
  tidyterra::geom_spatraster(data=elevation_sa) +  #Add color scale
  scale_fill_gradientn(colours = terrain.colors(10),
                       limits=c(500,2100),
                       oob=squish, #everything outside scale become either largest or smallest color
                       name="meters") +
  tidyterra::geom_spatvector(data=protected_areas, fill=NA, linewidth=0.5)+
  tidyterra::geom_spatvector(data=rivers, col="blue", linewidth=0.5)+
  tidyterra::geom_spatvector(data=lakes, fill="lightblue", linewidth=0.5)+
  tidyterra::geom_spatvector(data=studyarea, fill=NA, linewidth=1, col="red")+
  labs(title="Elevation") + 
  coord_sf(xlimits,ylimits, expand=F, datum = sf::st_crs(32736))+
  theme(axis.text=element_blank(),
        axis.ticks=element_blank())+
  ggspatial::annotation_scale(location = "bl", width_hint = 0.2)


# create 500 random points in our study area
set.seed(123)
npoints<-500

# and add them to the previous map

# make distance to river map
disttoriver_map_sa<-ggplot()+
  tidyterra::geom_spatraster(data=distancetoriver_sa) +  #Add color scale
  scale_fill_gradientn(colours = rev(pal_zissou2),
                       limits=c(0,14012),
                       oob=squish, #everything outside scale become either largest or smallest color
                       name="meters") +
  tidyterra::geom_spatvector(data=protected_areas, fill=NA, linewidth=0.5)+
  tidyterra::geom_spatvector(data=rivers, col="blue", linewidth=0.5)+
  tidyterra::geom_spatvector(data=lakes, fill="lightblue", linewidth=0.5)+
  tidyterra::geom_spatvector(data=studyarea, fill=NA, linewidth=1, col="red")+
  labs(title="Distance to river") + 
  coord_sf(xlimits,ylimits, expand=F, datum = sf::st_crs(32736))+
  theme(axis.text=element_blank(),
        axis.ticks=element_blank())+
  ggspatial::annotation_scale(location = "bl", width_hint = 0.2)

#make a map of the last year burned NOOOOOOG DOOOOEEEEN
lastyearburn_map_sa<-ggplot()+
  tidyterra::geom_spatraster(data=lastyearburn_sa) +  #Add color scale
  scale_fill_gradientn(colours = plasma ,
                       limits=c(0,2024),
                       oob=squish, #everything outside scale become either largest or smallest color
                       name="year") +
  tidyterra::geom_spatvector(data=protected_areas, fill=NA, linewidth=0.5)+
  tidyterra::geom_spatvector(data=rivers, col="blue", linewidth=0.5)+
  tidyterra::geom_spatvector(data=lakes, fill="lightblue", linewidth=0.5)+
  tidyterra::geom_spatvector(data=studyarea, fill=NA, linewidth=1, col="red")+
  labs(title="Last year burned") + 
  coord_sf(xlimits,ylimits, expand=F, datum = sf::st_crs(32736))+
  theme(axis.text=element_blank(),
        axis.ticks=element_blank())+
  ggspatial::annotation_scale(location = "bl", width_hint = 0.2)

#make a map of the burn frequency
burnfreq_map_sa<-ggplot()+
  tidyterra::geom_spatraster(data=burnfreq_sa) +  #Add color scale
  scale_fill_gradientn(colours = plasma ,
                       limits=c(0,5),
                       oob=squish, #everything outside scale become either largest or smallest color
                       name="year") +
  tidyterra::geom_spatvector(data=protected_areas, fill=NA, linewidth=0.5)+
  tidyterra::geom_spatvector(data=rivers, col="blue", linewidth=0.5)+
  tidyterra::geom_spatvector(data=lakes, fill="lightblue", linewidth=0.5)+
  tidyterra::geom_spatvector(data=studyarea, fill=NA, linewidth=1, col="red")+
  labs(title="Burn frequency") + 
  coord_sf(xlimits,ylimits, expand=F, datum = sf::st_crs(32736))+
  theme(axis.text=element_blank(),
        axis.ticks=element_blank())+
  ggspatial::annotation_scale(location = "bl", width_hint = 0.2)


### # put all maps together
# and save it to a high resolution png
composite_map_sa <- woody_map_sa + rainfall_map_sa + elevation_map_sa + disttoriver_map_sa +plot_layout(ncol=2) 
plot(composite_map_sa)
ggsave("C:/Users/praam/Documents/github/APCE2024/spatial-r-Gijsbert-Pr/Figure/composite_map_sa.png", composite_map_sa, width=20, height=20, units="cm")



# extract your the values of the different raster layers to the points


# make long format

# plot how woody cover is predicted by different variables


