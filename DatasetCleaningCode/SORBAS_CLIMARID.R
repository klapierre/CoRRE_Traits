#######################
## SORBAS_CLIMARID ###
######################
setwd("/Users/kaitlinkimmel/Dropbox/CoRRE_database")

## libraries ##
library(tidyr)

# read in data

dat <- read.csv("Data/OriginalData/2020 update/Data/SORBAS_CLIMARID_data.csv")
sp <- read.csv("Data/OriginalData/2020 update/Data/SORBAS_CLIMARID_sp.csv")


colnames(dat)[c(1:3)] <- c("treatment", "plot_id", "sp_code")

# get rid of first row
dat <- dat[-1,]

# change sp codes to names

dat <- merge(dat, sp, by = "sp_code", all.x = TRUE)

#for some reasom 4 sp1 aren't merging over - correcting that here

dat$genus_species[which(is.na(dat$genus_species))] <- "Helianthemum squamatum"

dat <- gather(dat, key = "measurement", value = "abundance", 4:10)

dat$month <- lapply(strsplit(dat$measurement, "[.]"), "[[", 1)
dat$calendar_year <- as.numeric(lapply(strsplit(dat$measurement, "[.]"), "[[", 2))
dat$site_code <- "SORBAS"
dat$project_name <- "CLIMARID"
dat$data_type <- "count"
dat$treatment_year <- dat$calendar_year - 2010


### Need to figure out growing season so can calculate one value per season / year

