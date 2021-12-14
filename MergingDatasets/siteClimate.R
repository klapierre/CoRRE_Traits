##############################
## Updating Site Lat-Long ###
############################

###to do: fix DL and DCIMC, both have double entries in final output


setwd("C:\\Users\\lapie\\Dropbox (Smithsonian)\\working groups\\CoRRE\\CoRRE_database\\")

library(raster)
library(rgdal)
library(data.table)
library(sp)

olddat <- read.csv("Data/OriginalData/CoRRE_experiment_locations.csv")
alldat <- read.csv("Data/CompiledData/RawAbundance.csv")
nutnet.sites <- read.csv("Data/OriginalData/Sites/NutNet/nutnet_siteData_01272021.csv", row.names = 1)

sites <- data.frame(site_code = unique(alldat$site_code))

tst <- merge(olddat, sites, all.y = TRUE)
tst<- tst[,-9:-10]
names(tst)[c(6,7)] <- c("Latitude", "Longitude")

nutnet.sites <- unique(nutnet.sites[,c(1,2,3,12,13,15,26)])
names(nutnet.sites) <- c("Location", "site_code", "Continent", "Latitude", "Longitude", "MAT", "MAP")
nutnet.sites <- nutnet.sites[-which(nutnet.sites$site_code == "cdcr.us"),]
nutnet.sites <- nutnet.sites[-which(nutnet.sites$site_code == "bayr.de"),]


tst$Latitude[is.na(tst$Latitude)] <- nutnet.sites$Latitude[match(tst$site_code,nutnet.sites$site_code)][which(is.na(tst$Latitude))]
tst$Longitude[is.na(tst$Longitude)] <- nutnet.sites$Longitude[match(tst$site_code,nutnet.sites$site_code)][which(is.na(tst$Longitude))]
tst$Location[is.na(tst$Location)] <- nutnet.sites$Location[match(tst$site_code,nutnet.sites$site_code)][which(is.na(tst$Location))]
tst$Continent[is.na(tst$Continent)] <- nutnet.sites$Continent[match(tst$site_code,nutnet.sites$site_code)][which(is.na(tst$Continent))]


#getting worldclim data for MAP and MAT
r <- getData("worldclim", var="bio",res=2.5)
r <- r[[c(1,12)]] #bio1 and bio12 are MAT and MAP
names(r) <- c("MAT","MAP")

coords <- tst[,7:6]
points <- SpatialPoints(coords, proj4string=r@crs)
values <- extract(r,points)




library(tidyverse)

df <- cbind.data.frame(coordinates(points),values)%>%
  mutate(MAT2=MAT/10)%>%
  select(-MAT)%>%
  rename(MAT=MAT2)

siteClimate <- tst%>%
  left_join(df)


# write.csv(siteClimate, "Data/CompiledData/siteLocationClimate.csv", row.names=F)


