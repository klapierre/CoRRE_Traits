##############################
## Updating Site Lat-Long ###
############################

setwd("/Users/kaitlinkimmel/Dropbox/CoRRE_database")

library(tidyr)

olddat <- read.csv("Data/OriginalData/CoRRE_experiment_locations.csv")
alldat <- read.csv("Data/CompiledData/RawAbundance.csv", row.names = 1)
nutnet.sites <- read.csv("Data/OriginalData/2020 update/Data/NutNet/nutnet_siteData_01272021.csv", row.names = 1)

sites <- data.frame(site_code = unique(alldat$site_code))

tst <- merge(olddat, sites, all.y = TRUE)
names(tst)[c(6,7)] <- c("Location", "Longitude")

nutnet.sites <- unique(nutnet.sites[,c(1,2,3,12,13,15,26)])
names(nutnet.sites) <- c("Location", "site_code", "Continent", "Location", "Longitude", "MAT", "MAP")
nutnet.sites <- nutnet.sites[-which(nutnet.sites$site_code == "cdcr.us"),]
nutnet.sites <- nutnet.sites[-which(nutnet.sites$site_code == "bayr.de"),]


tst$Latitude[is.na(tst$Latitude)] <- nutnet.sites$Latitude[match(tst$site_code,nutnet.sites$site_code)][which(is.na(tst$Latitude))]
tst$Longitude[is.na(tst$Longitude)] <- nutnet.sites$Longitude[match(tst$site_code,nutnet.sites$site_code)][which(is.na(tst$Longitude))]
tst$Location[is.na(tst$Location)] <- nutnet.sites$Location[match(tst$site_code,nutnet.sites$site_code)][which(is.na(tst$Location))]
tst$MAT[is.na(tst$MAT)] <- nutnet.sites$MAT[match(tst$site_code,nutnet.sites$site_code)][which(is.na(tst$MAT))]
tst$MAP[is.na(tst$MAP)] <- nutnet.sites$MAP[match(tst$site_code,nutnet.sites$site_code)][which(is.na(tst$MAP))]
tst$Continent[is.na(tst$Continent)] <- nutnet.sites$Continent[match(tst$site_code,nutnet.sites$site_code)][which(is.na(tst$Continent))]


write.csv(tst, "Data/CompiledData/site_location.csv")


