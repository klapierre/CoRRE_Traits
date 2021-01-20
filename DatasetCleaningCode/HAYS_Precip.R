####################
## HAYS_PRECIP ####
###################

setwd("/Users/kaitlinkimmel/Dropbox/CoRRE_database")

# library
library(readxl)
library(tidyr)

# read in data

dat <- read_excel("Data/OriginalData/2020 update/Data/HAYS_Precip.xlsx")

# Long to wide format
dat <- gather(dat, key = "sp_code", value = "abundance", 6:length(dat))

# species composition should be sum of 10 replicated
dat <- aggregate(dat$abundance, by = list(calendar_year = dat$YEAR, treatment = dat$TREAT, 
                                          plot_id = dat$PLOT, sp_code = dat$sp_code, 
                                          block = dat$Block),FUN = sum)

names(dat)[6] <- "abundance"

#code in treatments

dat$treatment[dat$treatment ==1] <- "reduction"
dat$treatment[dat$treatment ==2] <- "control"
dat$treatment[dat$treatment ==3] <- "add"

# get rid of 0s
dat <- dat[which(dat$abundance > 0),]

# Need to figure out how to get USDA species codes

# Add in other columns
dat$site_code <- "HAYS"
dat$project_name <- "Precip"
dat$treatment_year <- dat$calendar_year - 2007
dat$data_type <- "cover"
