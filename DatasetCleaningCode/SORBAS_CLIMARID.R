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
# replace NA with 0 
dat$abundance[which(is.na(dat$abundance))] <- 0
# From Ivan: Our study system is kind of bimodal regarding growth, the main growth and 
# activity period is Spring (between February and May) but there is a second growth period 
# in autumn (Sept-Nov), probably lower in activity and growth than spring but still significant
dat <- aggregate(dat$abundance, by = list(calendar_year = dat$calendar_year, treatment = dat$treatment, 
                                           plot_id = dat$plot_id, genus_species = dat$genus_species),
                  FUN = max)
# Get rid of 2016 because only data from December
dat <- dat[-which(dat$calendar_year == 2016),]
# Get rid of 0's
dat <- dat[which(dat$x > 0),]
dat$site_code <- "SORBAS"
dat$project_name <- "CLIMARID"
dat$data_type <- "count"
dat$treatment_year <- dat$calendar_year - 2010

names(dat)[5] <- "abundance"

write.csv(dat, "Data/CleanedData/Sites/Species csv/SORBAS_CLIMARID.csv", row.names = FALSE)

