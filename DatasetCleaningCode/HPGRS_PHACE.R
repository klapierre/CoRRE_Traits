#######################
#### HPGSRS_PHACE #####
#####################

setwd("~/Dropbox/CoRRE_database")

# library
library(readxl)
library(tidyr)

# data
dat <- read_excel("Data/OriginalData/Sites/HPGRS_PHACE_data.xlsx")

dat$treatment_year <- dat$YEAR - 2005
# How to add in warming treatment year?
# color?
dat$site_code <- "HPGRS"
dat$project_name <- "PHACE"
dat$data_type <- "biomass"

dat <- dat[-c(3,4,6,7,8,9)] # get rid of unnecessary columns
names(dat)[c(1:5)] <- c("calendar_year", "plot_id", "block", "treatment", "anpp") #rename columms

dat$treatment[which(dat$treatment == "cT" & dat$calendar_year < 2007)] <- "ct"
dat$treatment[which(dat$treatment == "CT" & dat$calendar_year < 2007)] <- "Ct"

bio_dat <- dat[,-c(6:59)] #pull out composite anpp data

write.csv(bio_dat, "Data/CleanedData/Sites/ANPP csv/HPGRS_PHACE_anpp.csv", row.names = FALSE) #save anpp data

dat <- dat[,-5]
dat <- gather(dat, key = "genus_species", value = "abundance", 5:58) # wide to long format
dat <- dat[which(dat$abundance > 0),] # get rid of species with 0 biomass

write.csv(dat, "Data/CleanedData/Sites/Species csv/HPGRS_PHACE.csv", row.names = FALSE)


