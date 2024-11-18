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
pal_zissou1<-rev(wesanderson::wes_palette("Zissou1", 10, type = "continuous"))
pal_zissou2<-wesanderson::wes_palette("Zissou1", 10, type = "continuous")

# load the vector data for the whole ecosystem
sf::st_layers("./2022_protected_areas/protected_areas.gpkg")
protected_areas<-terra::vect("./2022_protected_areas/protected_areas.gpkg",
                             layer="protected_areas_2022") # read protected area boundaries)
sf::st_layers("./2022_rivers/rivers_hydrosheds.gpkg")
rivers<-terra::vect("./2022_rivers/rivers_hydrosheds.gpkg",
                    layer="rivers_hydrosheds")
sf::st_layers("./lakes/lakes.gpkg")
lakes<-terra::vect("./lakes/lakes.gpkg",
                   layer="lakes")  

# read your study area !! check if this matches indeed the name of your area
sf::st_layers("./studyarea/studyarea.gpkg")
studyarea<-terra::vect("./studyarea/studyarea.gpkg",
                       layer="my_study_area")


# load the raster data for the whole ecosystem
woodybiom<-terra::rast("./2016_WoodyVegetation/TBA_gam_utm36S.tif")
rainfall<-terra::rast("./rainfall/CHIRPS_MeanAnnualRainfall.tif")
elevation<-terra::rast("./2023_elevation/elevation_90m.tif")
dist2river<-terra::rast("./2022_rivers/DistanceToRiver.tif")
lastyearburn<-terra::rast("./_MyData/fires/YearLastBurned.tif")
burnfreq<-terra::rast("./_MyData/fires/BurnFreq.tif")
cec<-terra::rast("./_MyData/soil/CEC_5_15cm.tif")
hills<-terra::rast("./_MyData/landforms/hills.tif")
dist2pa<-terra::rast("./_MyData/Protected area/DistanceToPA.tif") 
dist2crop<-terra::rast("./_MyData/DistanceToCropland.tif") 
EVI<-terra::rast("./Vegetation/Mean_EVI_2020_2024.tif")
dist2build<-terra::rast("./buildings/DistanceToBuilding.tif")

# set the limits of the map to show (xmin, xmax, ymin, ymax in utm36 coordinates)
xlimits<-c(550000,900000)
ylimits<-c(9600000,9950000)

# plot the woody biomass map that you want to predict
woody_map<-ggplot() +
  tidyterra::geom_spatraster(data=woodybiom) +
  scale_fill_gradientn(colours=rev(terrain.colors(6)),
                       limits=c(0.77,6.55),
                       oob=squish,
                       name="TBA/ha") +
  tidyterra::geom_spatvector(data=protected_areas,
                             fill=NA,linewidth=0.5) +
  tidyterra::geom_spatvector(data=studyarea,
                             fill=NA,linewidth=0.5,col="red") +
  tidyterra::geom_spatvector(data=lakes,
                             fill="lightblue",linewidth=0.5) +
  tidyterra::geom_spatvector(data=rivers,
                             col="blue",linewidth=0.5) +
  labs(title="Woody biomass") +
  coord_sf(xlimits,ylimits,datum = sf::st_crs(32736)) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  ggspatial::annotation_scale(location="bl",width_hint=0.2)
woody_map  


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
rainfall_map

# plot the elevation map

elevation_map<-ggplot() +
  tidyterra::geom_spatraster(data=elevation) +
  scale_fill_gradientn(colours=terrain.colors(10),
                       limits=c(500,2100),
                       oob=squish,
                       name="meters") +
  tidyterra::geom_spatvector(data=protected_areas,
                             fill=NA,linewidth=0.5) +
  tidyterra::geom_spatvector(data=studyarea,
                             fill=NA,linewidth=0.5,col="red") +
  tidyterra::geom_spatvector(data=lakes,
                             fill="lightblue",linewidth=0.5) +
  tidyterra::geom_spatvector(data=rivers,
                             col="blue",linewidth=0.5) +
  labs(title="Elevation") +
  coord_sf(xlimits,ylimits,datum = sf::st_crs(32736)) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  ggspatial::annotation_scale(location="bl",width_hint=0.2)
elevation_map  

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
dist2river_sa<-terra::crop(dist2river, saExt)
lastyearburn_sa<-terra::crop(lastyearburn, saExt)
burnfreq_sa<-terra::crop(burnfreq, saExt)
cec_sa<-terra::crop(cec, saExt)
hills_sa<-terra::crop(hills, saExt)
dist2pa_sa<-terra::crop(dist2pa, saExt)
dist2crop_sa<-terra::crop(dist2crop, saExt)
EVI_sa<-terra::crop(EVI$EVI, saExt)
dist2build_sa<-terra::crop(dist2build, saExt)


#01 Plot the woody biomass
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
woody_map_sa

#02 Rainfall

rainfall_30m <- rast(terra::ext(rainfall), resolution = 30, crs = crs(rainfall))
# Resample the raster to 30m resolution
rainfall_30m <- terra::resample(rainfall, rainfall_30m, method = "bilinear")  
rainfall_sa<-terra::crop(rainfall_30m,saExt) # crop to study area
rainfall_map_sa<-ggplot() +
  tidyterra::geom_spatraster(data=rainfall_sa) +
  scale_fill_gradientn(colours=pal_zissou1,
                       limits=c(600,1000),
                       oob=squish,
                       name="mm/yr") +
  tidyterra::geom_spatvector(data=protected_areas,
                             fill=NA,linewidth=0.5) +
  tidyterra::geom_spatvector(data=studyarea,
                             fill=NA,linewidth=0.5,col="red") +
  tidyterra::geom_spatvector(data=lakes,
                             fill="lightblue",linewidth=0.5) +
  tidyterra::geom_spatvector(data=rivers,
                             col="blue",linewidth=0.5) +
  labs(title="Rainfall") +
  coord_sf(xlimits,ylimits,expand=F,
           datum = sf::st_crs(32736)) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  ggspatial::annotation_scale(location="bl",width_hint=0.2)
rainfall_map_sa  


#03 Elevation
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
elevation_map_sa


#04 Distance to river
dist2river_map_sa<-ggplot()+
  tidyterra::geom_spatraster(data=dist2river_sa) +  #Add color scale
  scale_fill_gradientn(colours = topo.colors(6),
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
dist2river_map_sa


#05 Last year burned
lastyearburn_map_sa <- ggplot()+
  tidyterra::geom_spatraster(data=lastyearburn_sa) +  #Add color scale
  scale_fill_gradientn(colours = rev(pal_zissou2),
                       limits=c(2001,2016),
                       oob=squish, #everything outside scale become either largest or smallest color
                       name="years") +
  tidyterra::geom_spatvector(data=protected_areas, fill=NA, linewidth=0.5)+
  tidyterra::geom_spatvector(data=rivers, col="blue", linewidth=0.5)+
  tidyterra::geom_spatvector(data=lakes, fill="lightblue", linewidth=0.5)+
  tidyterra::geom_spatvector(data=studyarea, fill=NA, linewidth=1, col="red")+
  labs(title="Last year burned") + 
  coord_sf(xlimits,ylimits, expand=F, datum = sf::st_crs(32736))+
  theme(axis.text=element_blank(),
        axis.ticks=element_blank())+
  ggspatial::annotation_scale(location = "bl", width_hint = 0.2)
lastyearburn_map_sa


#06 Burn frequency
hist(burnfreq_sa)
burnfreq_map_sa<-ggplot() +
  tidyterra::geom_spatraster(data=burnfreq_sa) +
  scale_fill_gradientn(colours=pal_zissou2,
                       limits=c(0,4),
                       oob=squish,
                       name="years\nburned") +
  tidyterra::geom_spatvector(data=protected_areas,
                             fill=NA,linewidth=0.5) +
  tidyterra::geom_spatvector(data=studyarea,
                             fill=NA,linewidth=0.5,col="red") +
  tidyterra::geom_spatvector(data=lakes,
                             fill="lightblue",linewidth=0.5) +
  tidyterra::geom_spatvector(data=rivers,
                             col="blue",linewidth=0.5) +
  labs(title="Years burned") +
  coord_sf(xlimits,ylimits,expand=F,
           datum = sf::st_crs(32736)) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  ggspatial::annotation_scale(location="bl",width_hint=0.2)
burnfreq_map_sa


#07 Soil CEC (Ask question)
hist(cec_sa)
cec_map_sa<-ggplot() +
  tidyterra::geom_spatraster(data=cec_sa) +
  scale_fill_gradientn(colours=pal_zissou1,
                       limits=c(120,350),
                       oob=squish,
                       name="Soil\nCEC\n5-15cm") +
  tidyterra::geom_spatvector(data=protected_areas,
                             fill=NA,linewidth=0.5) +
  tidyterra::geom_spatvector(data=studyarea,
                             fill=NA,linewidth=0.5,col="red") +
  tidyterra::geom_spatvector(data=lakes,
                             fill="lightblue",linewidth=0.5) +
  tidyterra::geom_spatvector(data=rivers,
                             col="blue",linewidth=0.5) +
  labs(title="Soil CEC") +
  coord_sf(xlimits,ylimits,expand=F,
           datum = sf::st_crs(32736)) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  ggspatial::annotation_scale(location="bl",width_hint=0.2)
cec_map_sa


#08 Hills
landform_map_sa<-ggplot() +
  tidyterra::geom_spatraster(data=as.factor(hills_sa)) +
  scale_fill_manual(values=c("black","orange"),
                    labels=c("valleys\nand\nplains","hills")) +
  tidyterra::geom_spatvector(data=protected_areas,
                             fill=NA,linewidth=0.7) +
  tidyterra::geom_spatvector(data=studyarea,
                             fill=NA,linewidth=0.5,col="green") +
  tidyterra::geom_spatvector(data=lakes,
                             fill="lightblue",linewidth=0.5) +
  tidyterra::geom_spatvector(data=rivers,
                             col="blue",linewidth=0.5) +
  labs(title="Landform") +
  coord_sf(xlimits,ylimits,expand=F,
           datum = sf::st_crs(32736)) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  ggspatial::annotation_scale(location="bl",width_hint=0.2)
landform_map_sa

#09 Core protected areas
r<-terra::rast("./2022_protected_areas/CoreProtectedAreas.tif") 
CoreProtectedAreas_sa <- r |> #  replace NA by 0
  is.na() |>
  terra::ifel(0,r) 

CoreProtectedAreas_map_sa<-ggplot() +
  tidyterra::geom_spatraster(data=as.factor(CoreProtectedAreas_sa)) +
  scale_fill_manual(values=c("grey","lightgreen"),
                    labels=c("no","yes")) +
  tidyterra::geom_spatvector(data=protected_areas,
                             fill=NA,linewidth=0.5) +
  tidyterra::geom_spatvector(data=studyarea,
                             fill=NA,linewidth=0.5,col="red") +
  tidyterra::geom_spatvector(data=lakes,
                             fill="lightblue",linewidth=0.5) +
  tidyterra::geom_spatvector(data=rivers,
                             col="blue",linewidth=0.5) +
  labs(title="Core protected areas") +
  coord_sf(xlimits,ylimits,expand=F,
           datum = sf::st_crs(32736)) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  ggspatial::annotation_scale(location="bl",width_hint=0.2)
CoreProtectedAreas_map_sa


#10 Distance to Protected Area
hist(dist2pa_sa)
dist2pa_map_sa<-ggplot() +
  tidyterra::geom_spatraster(data=dist2pa_sa) +
  scale_fill_gradientn(colours=rev(pal_zissou2),
                       limits=c(0,60000),
                       oob=squish,
                       name="meters") +
  tidyterra::geom_spatvector(data=protected_areas,
                             fill=NA,linewidth=0.5) +
  tidyterra::geom_spatvector(data=studyarea,
                             fill=NA,linewidth=0.5,col="red") +
  tidyterra::geom_spatvector(data=lakes,
                             fill="lightblue",linewidth=0.5) +
  tidyterra::geom_spatvector(data=rivers,
                             col="blue",linewidth=0.5) +
  labs(title="Distance from protected area") +
  coord_sf(xlimits,ylimits,expand=F,
           datum = sf::st_crs(32736)) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  ggspatial::annotation_scale(location="bl",width_hint=0.2)
dist2pa_map_sa


#11 Distance to cropland
hist(dist2crop_sa)
dist2crop_map_sa<-ggplot() +
  tidyterra::geom_spatraster(data=dist2crop_sa) +
  scale_fill_gradientn(colours=rev(pal_zissou2),
                       limits=c(0,15000),
                       oob=squish,
                       name="meters") +
  tidyterra::geom_spatvector(data=protected_areas,
                             fill=NA,linewidth=0.5) +
  tidyterra::geom_spatvector(data=studyarea,
                             fill=NA,linewidth=0.5,col="red") +
  tidyterra::geom_spatvector(data=lakes,
                             fill="lightblue",linewidth=0.5) +
  tidyterra::geom_spatvector(data=rivers,
                             col="blue",linewidth=0.5) +
  labs(title="Distance from Cropland") +
  coord_sf(xlimits,ylimits,expand=F,
           datum = sf::st_crs(32736)) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  ggspatial::annotation_scale(location="bl",width_hint=0.2)
dist2crop_map_sa


#12 EVI

#Devide EVI values by 1000
EVI_sa<-EVI_sa/10000
hist(EVI_sa)
EVI_map_sa<-ggplot() +
  tidyterra::geom_spatraster(data=EVI_sa) +
  scale_fill_gradientn(colours=rev(pal_zissou2),
                       limits=c(0,0.5),
                       oob=squish,
                       name="EVI") +
  tidyterra::geom_spatvector(data=protected_areas,
                             fill=NA,linewidth=0.5) +
  tidyterra::geom_spatvector(data=studyarea,
                             fill=NA,linewidth=0.5,col="red") +
  tidyterra::geom_spatvector(data=lakes,
                             fill="lightblue",linewidth=0.5) +
  tidyterra::geom_spatvector(data=rivers,
                             col="blue",linewidth=0.5) +
  labs(title="EVI") +
  coord_sf(xlimits,ylimits,expand=F,
           datum = sf::st_crs(32736)) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  ggspatial::annotation_scale(location="bl",width_hint=0.2)
EVI_map_sa

#13 Distance to building
hist(dist2build_sa)
dist2build_map_sa<-ggplot() +
  tidyterra::geom_spatraster(data=dist2build_sa) +
  scale_fill_gradientn(colours=rev(pal_zissou2),
                       limits=c(0,15000),
                       oob=squish,
                       name="meters") +
  tidyterra::geom_spatvector(data=protected_areas,
                             fill=NA,linewidth=0.5) +
  tidyterra::geom_spatvector(data=studyarea,
                             fill=NA,linewidth=0.5,col="red") +
  tidyterra::geom_spatvector(data=lakes,
                             fill="lightblue",linewidth=0.5) +
  tidyterra::geom_spatvector(data=rivers,
                             col="blue",linewidth=0.5) +
  labs(title="Distance from building") +
  coord_sf(xlimits,ylimits,expand=F,
           datum = sf::st_crs(32736)) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  ggspatial::annotation_scale(location="bl",width_hint=0.2)
dist2build_map_sa



###RANDOM POINTS
# create 500 random points in our study area
set.seed(123)
rpoints <- terra::spatSample(studyarea, size = 250, 
                             method = "random")

# and add them to the previous map

rpoints_map_sa<-ggplot() +
  tidyterra::geom_spatvector(data=rpoints, size=0.5) +
  tidyterra::geom_spatvector(data=protected_areas,
                             fill=NA,linewidth=0.5) +
  tidyterra::geom_spatvector(data=studyarea,
                             fill=NA,linewidth=0.5,col="red") +
  tidyterra::geom_spatvector(data=lakes,
                             fill="lightblue",linewidth=0.5) +
  tidyterra::geom_spatvector(data=rivers,
                             col="blue",linewidth=0.5) +
  labs(title="250 random points") +
  coord_sf(xlimits,ylimits,expand=F,
           datum = sf::st_crs(32736)) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  ggspatial::annotation_scale(location="bl",width_hint=0.2)
rpoints_map_sa

### # put all maps together
# and save it to a high resolution png
composite_map_sa <- woody_map_sa + rainfall_map_sa + elevation_map_sa + dist2river_map_sa + lastyearburn_map_sa + burnfreq_map_sa + cec_map_sa + landform_map_sa + CoreProtectedAreas_map_sa + dist2pa_map_sa + dist2crop_map_sa + dist2build_map_sa + EVI_map_sa + rpoints_map_sa + plot_layout(ncol=3) 
plot(composite_map_sa)
ggsave("C:/Users/praam/Documents/github/APCE2024/spatial-r-Gijsbert-Pr/Figure/composite_map_sa.png", composite_map_sa, width=20, height=20, units="cm")




#############DATA extraction
# extract your the values of the different raster layers to the points
woody_points <- terra::extract(woodybiom_sa, rpoints) |> 
  as_tibble() |>
  dplyr::rename(woody=TBA_gam_utm36s)
woody_points
dist2river_points <- terra::extract(dist2river_sa, rpoints) |> 
  as_tibble() |>
  dplyr::rename(distancetoriver=distance)
dist2river_points
elevation_points <- terra::extract(elevation, rpoints) |> 
  as_tibble() 
elevation_points
rainfall_points <- terra::extract(rainfall_sa, rpoints) |> 
  as_tibble() |> 
  dplyr::rename(rainfall=CHIRPS_MeanAnnualRainfall)
rainfall_points
cec_points <- terra::extract(cec_sa, rpoints) |> 
  as_tibble() |>
  dplyr::rename(cec='cec_5-15cm_mean')
cec_points
burnfreq_points <- terra::extract(burnfreq_sa, rpoints) |> 
  as_tibble() |>
  dplyr::rename(burnfreq=burned_sum)
burnfreq_points
landform_points <- terra::extract(hills_sa, rpoints) |> 
  as_tibble() |>
  dplyr::rename(hills=remapped)
landform_points
dist2crop_points <- terra::extract(dist2crop_sa, rpoints) |> 
  as_tibble() |>
  dplyr::rename(distancetocrops=distance)
dist2crop_points
dist2pa_points <- terra::extract(dist2pa_sa, rpoints) |> 
  as_tibble() |>
  dplyr::rename(distancetopa=distance)
dist2pa_points
dist2build_points<- terra::extract(dist2build_sa, rpoints) |> 
  as_tibble() |>
  dplyr::rename(distancetobuild=distance)
EVI_points <- terra::extract(EVI_sa, rpoints) |> 
  as_tibble()

# merge the different variable into a single table
# use woody biomass as the last variable
pointdata<-cbind(dist2river_points[,2],elevation_points[,2],
                 rainfall_points[,2], 
                 cec_points[,2],burnfreq_points[,2],
                 landform_points[,2], dist2crop_points[,2], 
                 dist2pa_points[,2], dist2build_points[,2], EVI_points[,2], woody_points[,2]) |>
  as_tibble()
pointdata
pointdata<-pointdata[complete.cases(pointdata),]

#made a csv file
getwd()
readr::write_csv(pointdata, "pointdata.csv")

########Data analysis
# plot how woody cover is predicted by different variables
# Create a correlation panel plot
library(psych)
psych::pairs.panels(
  pointdata ,
  method = "pearson",     # Correlation method (use "spearman" for rank correlation)
  hist.col = "lightblue",  # Color for histograms
  density = TRUE,          # Add density plots
  ellipses = F,         # Add correlation ellipses
  lm = TRUE,                # Add linear regression lines
  stars=T
) #Is there a correlation between the variables? Follow diagonal to find correlation
ggsave("C:/Users/praam/Documents/github/APCE2024/spatial-r-Gijsbert-Pr/Figure/correlation_plot.png", width=20, height=20, units="cm")

# Make it long format
names(pointdata)
pointdata_long<-pivot_longer(data=pointdata,
                             cols = distancetoriver:EVI, # all except woody
                             names_to ="pred_var",
                             values_to = "pred_val")
pointdata_long

# panel plot
ggplot(data=pointdata_long, mapping=aes(x=pred_val,y=woody,group=pred_var)) +
  geom_point() +
  geom_smooth() +
  ylim(0,40) +
  facet_wrap(~pred_var,scales="free") 
ggsave("C:/Users/praam/Documents/github/APCE2024/spatial-r-Gijsbert-Pr/Figure/panel_plot.png", width=20, height=20, units="cm")

# do a pca
# Load the vegan package
library(vegan)
# Perform PCA using the rda() function
pca_result <- vegan::rda(pointdata,
                         scale = TRUE)
# Display a summary of the PCA
summary(pca_result)

# Plot the PCA
plot(pca_result, scaling = 2, type="n", xlab="",ylab="")  # Use scaling = 1 for distance preservation, scaling = 2 for correlations
# Add points for samples
points(pca_result, display = "sites", col = pointdata$hills+1, bg = "blue", cex = 1)
# Add arrows for variables
arrows(0, 0, scores(pca_result, display = "species")[, 1], scores(pca_result, display = "species")[, 2], 
       length = 0.1, col = "red")
# Label the variables with arrows
text(scores(pca_result, display = "species")[, 1], scores(pca_result, display = "species")[, 2], 
     labels = colnames(pointdata), col = "red", cex = 0.8, pos = 4)
# Add axis labels and a title
title(main = "PCA Biplot")
xlabel <- paste("PC1 (", round(pca_result$CA$eig[1] / sum(pca_result$CA$eig) * 100, 1), "%)", sep = "")
ylabel <- paste("PC2 (", round(pca_result$CA$eig[2] / sum(pca_result$CA$eig) * 100, 1), "%)", sep = "")
title(xlab=xlabel)
title(ylab=ylabel)
# add contours for woody cover
plot_PCA<-vegan::ordisurf(pca_result, pointdata$woody, add = TRUE, col = "green4")
ggsave("C:/Users/praam/Documents/github/APCE2024/spatial-r-Gijsbert-Pr/Figure/plot_PCA.png", width=20, height=20, units="cm")
