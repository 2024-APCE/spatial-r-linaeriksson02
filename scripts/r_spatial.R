# Spatial analysis in R
# Han Olff nov 2021

rm(list = ls())
# set the working directory where your GIS data are located
setwd("C:/Users/Lina/Documents/master year 1/
      advanced population and community ecology/GIT")

# restore the libraries of the project 
renv::restore()
renv::snapshot() # updates the packages to renv restore


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
barplot(rep(1,10), col = rev(topo.colors(10))) # rev turns the scale arround
barplot(rep(1,10), col = rev(terrain.colors(10)))
library(RColorBrewer) 
RColorBrewer::display.brewer.all() # predefined colour patterns we can use
barplot(rep(1,10), col = RColorBrewer::brewer.pal(10, "Spectral")) # how you choose from the colour pallete
barplot(rep(1,10), col = RColorBrewer::brewer.pal(10, "BrBG"))

library(viridis) # also colour pallete
barplot(rep(1,10), col = rev(viridis::viridis(10)))
barplot(rep(1,10), col = viridis::plasma(10))
barplot(rep(1,10), col = viridis::heat(10))
viridis::plasma(10)
library(wesanderson)
barplot(rep(1,10), col = rev(wesanderson::wes_palette("Zissou1", 10, type = "continuous")))
pal_zissou1<-rev(wesanderson::wes_palette("Zissou1", 10, type = "continuous"))
pal_zissou2<-wesanderson::wes_palette("Zissou1", 10, type = "continuous")
pal_zissou1

# load the vector data for the whole ecosystem
sf::st_layers("./2022_protected_areas/protected_areas.gpkg") # . is from the already set working directory with setwd. st_layers gives an overview of all the layers.
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

# insepect vector data
plot(protected_areas) # to see the layer.

# load the raster data for the whole ecosystem
woodybiom<-terra::rast("./2016_WoodyVegetation/TBA_gam_utm36S.tif")
hillshade<-terra::rast("./2023_elevation/hillshade_z5.tif")
rainfall<-terra::rast("./rainfall/CHIRPS_MeanAnnualRainfall.tif")
elevation<-terra::rast("./2023_elevation/elevation_90m.tif")

# inspect the data 
class(protected_areas)
plot(elevation)
plot(protected_areas, add=T)

# set the limits of the map to show (xmin, xmax, ymin, ymax in utm36 coordinates)
xlimits<-c(550000,900000)
ylimits<-c(9600000,9950000)

# plot the woody biomass map that you want to predict
woody_map <- ggplot() +
  tidyterra::geom_spatraster(data=woodybiom) +
  scale_fill_gradientn(colours=rev(terrain.colors(6)), 
                      limits=c(0.77,6.55),
                      oob=squish, # everything outside is the largest or smallest
                      name="TBA/ha") +
  tidyterra::geom_spatvector(data=protected_areas,
                             fill=NA, linewidth=0.5) +
  tidyterra::geom_spatvector(data=rivers,
                             fill=NA, linewidth=0.5, colour="lightblue") +
  tidyterra::geom_spatvector(data=lakes, fill="blue") +
  tidyterra::geom_spatvector(data=studyarea, fill=NA, linewidth=1, colour="red") +
  labs(title="woody biomass") + 
  coord_sf(xlimits, ylimits, datum=sf::st_crs(32736)) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  ggspatial::annotation_scale(location="bl", width_hint=0.2)
woody_map

# plot the rainfall map
rain_map <- ggplot() +
  tidyterra::geom_spatraster(data=rainfall) +
  scale_fill_gradientn(colors=pal_zissou1, 
                       limits=c(364,2054),
                       oob=squish, # everything outside is the largest or smallest
                       name="mm/y") +
  tidyterra::geom_spatvector(data=protected_areas,
                             fill=NA, linewidth=0.5) +
  tidyterra::geom_spatvector(data=rivers,
                             fill=NA, linewidth=0.5, colour="lightblue") +
  tidyterra::geom_spatvector(data=lakes, fill="blue") +
  tidyterra::geom_spatvector(data=studyarea, fill=NA, linewidth=1, colour="red") +
  labs(title="rainfall") + coord_sf(xlimits, ylimits, datum=sf::st_crs(32736)) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  ggspatial::annotation_scale(location="bl", width_hint=0.2)
rain_map

# plot the elevation map
elevation_map <- ggplot() +
  tidyterra::geom_spatraster(data=elevation) +
  scale_fill_gradientn(colours=rev(terrain.colors(6)), 
                       limits=c(500,2100),
                       oob=squish, # everything outside is the largest or smallest
                       name="m") +
  tidyterra::geom_spatvector(data=protected_areas,
                             fill=NA, linewidth=0.5) +
  tidyterra::geom_spatvector(data=rivers,
                             fill=NA, linewidth=0.5, colour="lightblue") +
  tidyterra::geom_spatvector(data=lakes, fill="blue") +
  tidyterra::geom_spatvector(data=studyarea, fill=NA, linewidth=1, colour="red") +
  labs(title= "elevation") + coord_sf(xlimits, ylimits, datum=sf::st_crs(32736)) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  ggspatial::annotation_scale(location="bl", width_hint=0.2)
elevation_map

# combine the different maps  into one composite map using the patchwork library
# and save it to a high resolution png
woody_elevation_rain_map2 <- woody_map + elevation_map + rain_map + 
  patchwork::plot_layout(ncol=2)
woody_elevation_rain_map2
ggsave("C:/Users/Lina/Documents/github/APCE24/spatial-r-linaeriksson02/plots/woody_elevation_rain_map2.png", p, width=20, height=20, units="cm")

############################
### explore your study area
# set the limits of your study area
#xlimits<-c(740000,790000)
#ylimits<-c(9810000,9840000)
xlimits<-sf::st_bbox(studyarea)[c(1,3)] # this is automotically
ylimits<-sf::st_bbox(studyarea)[c(2,4)]
saExt<-terra::ext(studyarea)


# crop the woody biomass to the extent of the studyarea
woodybiom_sa<-terra::crop(woodybiom, saExt)
woodybiom_sa
rainfall_sa <- terra::crop(rainfall, saExt)
elevation_sa <- terra::crop(elevation, saExt)

# woody biomass
woody_map_sa <- ggplot() +
  tidyterra::geom_spatraster(data=woodybiom_sa) +
  scale_fill_gradientn(colours=rev(terrain.colors(6)), 
                       limits=c(0.77,6.55),
                       oob=squish, # everything outside is the largest or smallest
                       name="TBA/ha") +
  tidyterra::geom_spatvector(data=protected_areas,
                             fill=NA, linewidth=0.5) +
  tidyterra::geom_spatvector(data=rivers,
                             fill=NA, linewidth=0.5, colour="lightblue") +
  tidyterra::geom_spatvector(data=lakes, fill="blue") +
  tidyterra::geom_spatvector(data=studyarea, fill=NA, linewidth=1, colour="red") +
  labs(title="woody biomass") + 
  coord_sf(xlimits, ylimits, expand=F,
           datum=sf::st_crs(32736)) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  ggspatial::annotation_scale(location="bl", width_hint=0.2)
woody_map_sa


# rainfall
rain_map_sa <- ggplot() +
  tidyterra::geom_spatraster(data=rainfall_sa) +
  scale_fill_gradientn(colours=pal_zissou1, 
                       limits=c(364,2054),
                       oob=squish, # everything outside is the largest or smallest
                       name="mm/year") +
  tidyterra::geom_spatvector(data=protected_areas,
                             fill=NA, linewidth=0.5) +
  tidyterra::geom_spatvector(data=rivers,
                             fill=NA, linewidth=0.5, colour="lightblue") +
  tidyterra::geom_spatvector(data=lakes, fill="blue") +
  tidyterra::geom_spatvector(data=studyarea, fill=NA, linewidth=1, colour="red") +
  labs(title= "rainfall") + coord_sf(xlimits, ylimits,
                                      expand=F, datum=sf::st_crs(32736)) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  ggspatial::annotation_scale(location="bl", width_hint=0.2)
rain_map_sa

# elevation
elevation_map_sa <- ggplot() +
  tidyterra::geom_spatraster(data=elevation_sa) +
  scale_fill_gradientn(colours=terrain.colors(6), 
                       limits=c(500,2100),
                       oob=squish, # everything outside is the largest or smallest
                       name="m") +
  tidyterra::geom_spatvector(data=protected_areas,
                             fill=NA, linewidth=0.5) +
  tidyterra::geom_spatvector(data=rivers,
                             fill=NA, linewidth=0.5, colour="lightblue") +
  tidyterra::geom_spatvector(data=lakes, fill="blue") +
  tidyterra::geom_spatvector(data=studyarea, fill=NA, linewidth=1, colour="red") +
  labs(title= "elevation") + coord_sf(xlimits, ylimits, 
                                      expand=F, datum=sf::st_crs(32736)) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  ggspatial::annotation_scale(location="bl", width_hint=0.2)
elevation_map_sa

# combine the maps
combined_sa <- woody_map_sa + elevation_map_sa + rain_map_sa + 
  patchwork::plot_layout(ncol=2)
combined_sa
ggsave("C:/Users/Lina/Documents/github/APCE24/spatial-r-linaeriksson02/plots/combined_sa.png", p, width=20, height=20, units="cm")

# create 500 random points in our study area
set.seed(123)

# and add them to the previous map

# make distance to river map
dist2river_sa<-terra::rast("./DistanceToRiver (1).tif")
map_dist2river_sa<- ggplot() +
  tidyterra::geom_spatraster(data=dist2river_sa) +
  scale_fill_gradientn(colours = pal_zissou2,
                       limits=c(0,10),
                       oob=squish,
                       name="kilometers") +
  tidyterra::geom_spatvector(data = protected_areas,fill=NA, linewidth=0.7) +
  tidyterra::geom_spatvector(data=rivers,linewidth=0.3,col="blue") +
  labs(title = "Distance to rivers") +
  coord_sf(xlim=xlimits,ylim=ylimits, # set bounding box
           expand=F,
           datum=sf::st_crs(32736)) +   # keep in original projected coordinates
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +   # Remove axis coordinate labels
  ggspatial::annotation_scale(  # Add a scale bar
    location = "bl",             # Position: bottom left
    width_hint = 0.2)             # Adjust width of the scale bar 
map_dist2river_sa

# make distance to river map
burnfreq_sa<-terra::rast("./BurnFreq.aux.tif")
map_dist2river_sa<- ggplot() +
  tidyterra::geom_spatraster(data=dist2river_sa) +
  scale_fill_gradientn(colours = pal_zissou2,
                       limits=c(0,10),
                       oob=squish,
                       name="kilometers") +
  tidyterra::geom_spatvector(data = protected_areas,fill=NA, linewidth=0.7) +
  tidyterra::geom_spatvector(data=rivers,linewidth=0.3,col="blue") +
  labs(title = "Distance to rivers") +
  coord_sf(xlim=xlimits,ylim=ylimits, # set bounding box
           expand=F,
           datum=sf::st_crs(32736)) +   # keep in original projected coordinates
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +   # Remove axis coordinate labels
  ggspatial::annotation_scale(  # Add a scale bar
    location = "bl",             # Position: bottom left
    width_hint = 0.2)             # Adjust width of the scale bar 
map_dist2river_sa

### put all maps together

# extract your the values of the different raster layers to the points


# make long format

# plot how woody cover is predicted by different variables


