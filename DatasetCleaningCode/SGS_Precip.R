####################
#### SGS_PRECIP ####
###################

setwd("/Users/kaitlinkimmel/Dropbox/CoRRE_database")

# library
library(readxl)
library(tidyr)

# read in data
dat <- read_excel("Data/OriginalData/2020 update/Data/SGS_Precip.xlsx")

# Long to wide format
dat <- gather(dat, key = "sp_code", value = "abundance", 4:length(dat))

#Code in treatments
dat$TREAT[dat$TREAT == 1] <- "reduction"
dat$TREAT[dat$TREAT == 2] <- "control"
dat$TREAT[dat$TREAT == 3] <- "add"

dat$treatment_year <- dat$YEAR - 2007

# irrigation started in 2008
for (i in 1:nrow(dat)){
  if(dat$TREAT[i] == "add"){
    dat$treatment_year[i] <- dat$YEAR[i] - 2008
  }
}

# get rid of 0's
dat <- dat[which(dat$abundance > 0),]

dat$site_code <- "SGS"
dat$project_name <- "Precip"
dat$data_type <- "cover"

names(dat)[c(1:3)] <- c("calendar_year", "plot_id", "treatment")
