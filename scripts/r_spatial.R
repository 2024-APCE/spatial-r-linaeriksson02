# Spatial analysis in R
# Han Olff nov 2021

rm(list = ls())
# set the working directory where your GIS data are located
setwd("C:/Users/Lina/Documents/master year 1/advanced population and community ecology/GIT")



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
barplot(rep(1,10), col = RColorBrewer::brewer.pal(9, "YlOrRd")) # how you choose from the colour pallete
# YoR <- RColorBrewer::display.brewer.all("YlOrRd", 9, type = "Spectral")

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
  labs(title="Woody biomass") + 
  coord_sf(xlimits, ylimits, expand=F,
           datum=sf::st_crs(32736)) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  ggspatial::annotation_scale(location="bl", width_hint=0.2)
woody_map_sa


# rainfall
# to take away the empty sides, turn into 30 resultion
rainfall_30m <- rast(terra::ext(rainfall), resolution = 30, crs = crs(rainfall))
# Resample the raster to 30m resolution
rainfall_30m <- terra::resample(rainfall, rainfall_30m, method = "bilinear")  
rainfall_sa<-terra::crop(rainfall_30m,saExt) # crop to study area

rain_map_sa <- ggplot() +
  tidyterra::geom_spatraster(data=rainfall_sa) +
  scale_fill_gradientn(colours=pal_zissou1, 
                       limits=c(700,1000),
                       oob=squish, # everything outside is the largest or smallest
                       name="mm/year") +
  tidyterra::geom_spatvector(data=protected_areas,
                             fill=NA, linewidth=0.5) +
  tidyterra::geom_spatvector(data=rivers,
                             fill=NA, linewidth=0.5, colour="lightblue") +
  tidyterra::geom_spatvector(data=lakes, fill="blue") +
  tidyterra::geom_spatvector(data=studyarea, fill=NA, linewidth=1, colour="red") +
  labs(title= "Rainfall") + coord_sf(xlimits, ylimits,
                                      expand=F, datum=sf::st_crs(32736)) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  ggspatial::annotation_scale(location="bl", width_hint=0.2)
rain_map_sa

# elevation
elevation_map_sa <- ggplot() +
  tidyterra::geom_spatraster(data=elevation_sa) +
  scale_fill_gradientn(colours=terrain.colors(6), 
                       limits=c(1100,2500),
                       oob=squish, # everything outside is the largest or smallest
                       name="meters") +
  tidyterra::geom_spatvector(data=protected_areas,
                             fill=NA, linewidth=0.5) +
  tidyterra::geom_spatvector(data=rivers,
                             fill=NA, linewidth=0.5, colour="lightblue") +
  tidyterra::geom_spatvector(data=lakes, fill="blue") +
  tidyterra::geom_spatvector(data=studyarea, fill=NA, linewidth=1, colour="red") +
  labs(title= "Elevation") + coord_sf(xlimits, ylimits, 
                                      expand=F, datum=sf::st_crs(32736)) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  ggspatial::annotation_scale(location="bl", width_hint=0.2)
elevation_map_sa


# make distance to river map
dist2river_sa<-terra::rast("C:/Users/Lina/Documents/master year 1/advanced population and community ecology/GIT/DistanceToRiver.tif")

dist2river_map_sa<-ggplot() +
  tidyterra::geom_spatraster(data=dist2river_sa) +
  scale_fill_gradientn(colours=topo.colors(6),
                       limits=c(0,12000),
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
  labs(title="Distance to river") +
  coord_sf(xlimits,ylimits,expand=F,
           datum = sf::st_crs(32736)) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  ggspatial::annotation_scale(location="bl",width_hint=0.2)
dist2river_map_sa

# make distance to river map --> fix this

burnfreq_sa<-terra::rast("./BurnFreq.tif")
burnfreq_sa_map <- ggplot() +
  tidyterra::geom_spatraster(data=burnfreq_sa) +
  scale_fill_gradientn(colours = RColorBrewer::brewer.pal(9, "YlOrRd"),
                       limits=c(0,17),
                       oob=squish,
                       name="number of years burned") +
  tidyterra::geom_spatvector(data = protected_areas,fill=NA, linewidth=0.7) +
  tidyterra::geom_spatvector(data=rivers,linewidth=0.3,col="blue") +
  labs(title = "Burn frequency") +
  coord_sf(xlim=xlimits,ylim=ylimits, # set bounding box
           expand=F,
           datum=sf::st_crs(32736)) +   # keep in original projected coordinates
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +   # Remove axis coordinate labels
  ggspatial::annotation_scale(  # Add a scale bar
    location = "bl",             # Position: bottom left
    width_hint = 0.2)             # Adjust width of the scale bar 
burnfreq_sa_map

# year last burned - not relevant
lastburned_sa<-terra::rast("./YearLastBurned.tif")
lastburned_sa_map <- ggplot() +
  tidyterra::geom_spatraster(data=lastburned_sa) +
  scale_fill_gradientn(colours = RColorBrewer::brewer.pal(9, "YlOrRd"),
                       limits=c(2001,2024),
                       oob=squish,
                       name="Year last burned") +
  tidyterra::geom_spatvector(data = protected_areas,fill=NA, linewidth=0.7) +
  tidyterra::geom_spatvector(data=rivers,linewidth=0.3,col="blue") +
  labs(title = "Year last burned") +
  coord_sf(xlim=xlimits,ylim=ylimits, # set bounding box
           expand=F,
           datum=sf::st_crs(32736)) +   # keep in original projected coordinates
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +   # Remove axis coordinate labels
  ggspatial::annotation_scale(  # Add a scale bar
    location = "bl",             # Position: bottom left
    width_hint = 0.2)             # Adjust width of the scale bar 
lastburned_sa_map # how to change that it goes from 0 to 2001 and so on
# not really relevant if i have frequency last burned, also difficult with the year limits and 0


# soil CEC
cec_sa<-terra::rast("./CEC_5_15cm.tif")
hist(cec_sa)
cec_map_sa<-ggplot() +
  tidyterra::geom_spatraster(data=cec_sa) +
  scale_fill_gradientn(colours=pal_zissou1,
                       limits=c(100,250),
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

# core_protected_areas  map 
r<-terra::rast("./CoreProtectedAreas.tif") 
CoreProtectedAreas_sa <- r |> #  replace NA by 0
  is.na() |>
  terra::ifel(0,r) 

CoreProtectedAreas_map_sa<-ggplot() +
  tidyterra::geom_spatraster(data=as.factor(CoreProtectedAreas_sa)) +
  scale_fill_manual(values=c("grey","LightGreen"),
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

# landforms
# zita note: niet de goeie, negeer deze
landform_sa<-terra::rast("./landforms/landforms.tif")
landform_map_sa<-ggplot() +
  tidyterra::geom_spatraster(data=as.factor(landform_sa)) +
  scale_fill_manual(values=c("black","orange", "yellow", "green", 
                             "blue", "purple", "pink"),
                    labels=c("peak ridge (warm)","upper slope (warm)", "upper slope (flat)", "lower slope (warm)", "lower slope (flat)", "valley", "valley narrow")) +
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

# landform - hills
# zita note: je moet in earth engine even naar han zn script en dan de verschillende dingen dowloaden die nieuw zijn. dan pak je de hills in de landsforms en dan heb je de goeie.
hills_sa<-terra::rast("./landforms/hills.tif")
hills_map_sa<-ggplot() +
  tidyterra::geom_spatraster(data=as.factor(hills_sa)) +
  scale_fill_manual(values=c("black","pink"),
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
hills_map_sa


# create 500 random points in our study area
set.seed(123)
rpoints <- terra::spatSample(studyarea, size = 250, 
                             method = "random")
#mapit
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

### put all maps together
# combine the maps
combined_sa <- woody_map_sa + elevation_map_sa + rain_map_sa + 
  burnfreq_sa_map + dist2river_map_sa + cec_map_sa +
  CoreProtectedAreas_map_sa  + hills_map_sa +
  rpoints_map_sa +
  patchwork::plot_layout(ncol=3)
combined_sa
ggsave("C:/Users/Lina/Documents/github/APCE24/spatial-r-linaeriksson02/plots/combined_sa.png", combined_sa, width=20, height=20, units="cm")


# extract your the values of the different raster layers to the points
# Extract raster values at the points
woody_points <- terra::extract(woodybiom_sa, rpoints) |> 
  as_tibble() |>
  dplyr::rename(woody=TBA_gam_utm36s)
woody_points
dist2river_points <- terra::extract(dist2river_sa, rpoints) |> 
  as_tibble() |>
  dplyr::rename(dist2river=distance)
dist2river_points
elevation_points <- terra::extract(elevation, rpoints) |> 
  as_tibble() 
elevation_points
CorProtAr_points <- terra::extract(CoreProtectedAreas_sa, rpoints) |> 
  as_tibble() |>
  dplyr::rename(CorProtAr=CoreProtectedAreas)
CorProtAr_points
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


# merge dif variables into a single table
# use woody biomass as last variable
pointdata<-cbind(dist2river_points[,2],elevation_points[,2],
                 CorProtAr_points[,2],rainfall_points[,2], 
                 cec_points[,2],burnfreq_points[,2],
                 landform_points[,2],woody_points[,2]) |>
  as_tibble()
pointdata

# plot how woody cover is predicted by different variables
# Create a correlation panel plot
#install.packages(psych)
library(psych)
psych::pairs.panels(pointdata, methods='pearson', hist.col='lightblue', density=TRUE, ellipses=FALSE, lm=TRUE, stars=TRUE)

# make long format
# make long format
names(pointdata)
pointdata_long<-pivot_longer(data=pointdata,
                             cols = dist2river:hills, # all except woody
                             names_to ="pred_var",
                             values_to = "pred_val")
pointdata_long

# panel plot
ggplot(data=pointdata_long, mapping=aes(x=pred_val,y=woody,group=pred_var)) +
  geom_point() +
  geom_smooth() +
  ylim(0,40) +
  facet_wrap(~pred_var,scales="free") 
# first oridnation access: new ranking of sites, what order would maximize the variables in the data
# residuals unexplained variation

# do a pca
# Load the vegan package
install.packages("vegan")
library(vegan)
# Perform PCA using the rda() function
pca_result <- vegan::rda(pointdata,
                         scale = TRUE)
# Display a summary of the PCA
summary(pca_result)
# decent because bumbers are high

# Plot the PCA
plot(pca_result, scaling = 2, type="n", xlab="",ylab="")  # Use scaling = 1 for distance preservation, scaling = 2 for correlations
# Add points for samples
points(pca_result, display = "sites", pch=pointdata$CorProtAr+1, col = pointdata$hills+1, bg = "blue", cex = 1)
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
vegan::ordisurf(pca_result, pointdata$woody, add = TRUE, col = "green4")
ggsave("C:/Users/Lina/Documents/github/APCE24/spatial-r-linaeriksson02/plots/pca.png", width=20, height=20, units="cm")

# results: negatively related with elevation
# higher woody cover outside protected areas
# higher woody cover closer to river
#
