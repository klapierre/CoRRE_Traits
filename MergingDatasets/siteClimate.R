##############################
## Updating Site Lat-Long ###
############################

# remotes::install_github("mikejohnson51/AOI") # suggested!
# remotes::install_github("mikejohnson51/climateR")

setwd("C:\\Users\\lapie\\Dropbox (Smithsonian)\\working groups\\CoRRE\\CoRRE_database\\")

library(raster)
library(climateR)
# library(rgdal)
# library(data.table)
# library(sp)
# library(SPEI)
library(tidyverse)

olddat <- read.csv("Data/OriginalData/CoRRE_experiment_locations.csv")

nutnet.sites <- read.csv("Data/OriginalData/Sites/NutNet/nutnet_siteData_01272021.csv", row.names = 1)%>%
  select(site_name, site_code, continent, latitude, longitude)%>%
  unique()%>%
  filter(site_code %in% c('cbgb.us', 'shps.us', 'temple.us', 'sier.us', 'yarra.au', 'veluwe.nl'))%>%
  rename(Location=site_name, Latitude=latitude, Longitude=longitude, Continent=continent)%>%
  mutate(PubLat=NA, PubLong=NA, Offset=NA)

sites <- read.csv("Data/CompiledData/RawAbundance.csv")%>%
  select(site_code)%>%
  unique()%>%
  full_join(olddat)%>%
  select(-MAP, -MAT)%>%
  rename(Latitude=Lattitue, Longitude=Longitue)%>%
  filter(!(site_code %in% c('cbgb.us', 'shps.us', 'temple.us', 'sier.us', 'yarra.au', 'veluwe.nl')))%>%
  rbind(nutnet.sites)


#getting worldclim data (MAP, MAT, Tmin, Tmax)
r <- getData("worldclim", var="bio",res=2.5)
r <- r[[c(1,5,6,12)]] #bio1=MAT, bio5=Tmax, bio6=Tmin, and bio12=MAP
names(r) <- c("MAT2","Tmax2","Tmin2","MAP")

coords <- sites[,7:6]
points <- SpatialPoints(coords, proj4string=r@crs)
values <- raster::extract(r,points)

df <- cbind.data.frame(coordinates(points),values)%>%
  mutate(MAT=MAT2/10, Tmin=Tmin2/10, Tmax=Tmax2/10)%>%
  select(-MAT2, -Tmax2, -Tmin2)

#precipitation data
# download.file(url = 'http://thredds.northwestknowledge.net:8080/thredds/fileServer/TERRACLIMATE_ALL/data/TerraClimate_ppt_2019.nc', destfile = 'ppt.nc')
ppt21 <- raster::stack(x = 'terraclimate data\\TerraClimate_ppt_2021.nc')
# ppt20 <- raster::stack(x = 'terraclimate data\\TerraClimate_ppt_2020.nc')
# ppt19 <- raster::stack(x = 'terraclimate data\\TerraClimate_ppt_2019.nc')
# ppt18 <- raster::stack(x = 'terraclimate data\\TerraClimate_ppt_2018.nc')
# ppt17 <- raster::stack(x = 'terraclimate data\\TerraClimate_ppt_2017.nc')
# ppt16 <- raster::stack(x = 'terraclimate data\\TerraClimate_ppt_2016.nc')
# ppt15 <- raster::stack(x = 'terraclimate data\\TerraClimate_ppt_2015.nc')
# ppt14 <- raster::stack(x = 'terraclimate data\\TerraClimate_ppt_2014.nc')
# ppt13 <- raster::stack(x = 'terraclimate data\\TerraClimate_ppt_2013.nc')
# ppt12 <- raster::stack(x = 'terraclimate data\\TerraClimate_ppt_2012.nc')
# ppt11 <- raster::stack(x = 'terraclimate data\\TerraClimate_ppt_2011.nc')
# ppt10 <- raster::stack(x = 'terraclimate data\\TerraClimate_ppt_2010.nc')
# ppt09 <- raster::stack(x = 'terraclimate data\\TerraClimate_ppt_2009.nc')
# ppt08 <- raster::stack(x = 'terraclimate data\\TerraClimate_ppt_2008.nc')
# ppt07 <- raster::stack(x = 'terraclimate data\\TerraClimate_ppt_2007.nc')
# ppt06 <- raster::stack(x = 'terraclimate data\\TerraClimate_ppt_2006.nc')
# ppt05 <- raster::stack(x = 'terraclimate data\\TerraClimate_ppt_2005.nc')
# ppt04 <- raster::stack(x = 'terraclimate data\\TerraClimate_ppt_2004.nc')
# ppt03 <- raster::stack(x = 'terraclimate data\\TerraClimate_ppt_2003.nc')
# ppt02 <- raster::stack(x = 'terraclimate data\\TerraClimate_ppt_2002.nc')
# ppt01 <- raster::stack(x = 'terraclimate data\\TerraClimate_ppt_2001.nc')
# ppt00 <- raster::stack(x = 'terraclimate data\\TerraClimate_ppt_2000.nc')
# ppt <- raster::stack(ppt21,ppt20)

# ,ppt19,ppt18,ppt17,ppt16,ppt15,ppt14,ppt13,ppt12,ppt11,ppt10,ppt09,ppt08,ppt07,ppt06,ppt05,ppt04,ppt03,ppt02,ppt01,ppt00)

ppt_mean <- calc(ppt21, # RasterStack object
                 fun = mean, # Function to apply across the layers
                 na.rm = TRUE)


pet21 <- raster::stack(x = 'terraclimate data\\TerraClimate_pet_2021.nc')

pet_mean <- calc(pet21, # RasterStack object
                 fun = mean, # Function to apply across the layers
                 na.rm = TRUE)

# aridity index = ppt/pet
aridity_index <- overlay(x = ppt_mean, # Raster object 1
                         y = pet_mean, # Raster object 2
                         fun = function(x, y){return(x / y)}) # Function to apply

aridityValues <- raster::extract(aridity_index,points)


#bind together precip, temp, and aridity values (aridity based on just 2021 data)
siteClimate <- sites%>%
  left_join(df)%>%
  cbind(aridityValues)

# write.csv(siteClimate, "Data/CompiledData/siteLocationClimate.csv", row.names=F)


