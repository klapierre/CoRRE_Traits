##################
## ANR FERT2 ####
#################

setwd("/Users/kaitlinkimmel/Dropbox/CoRRE_database")

# library
library(readxl)
library(tidyr)

# data
dat <- read_excel("Data/OriginalData/2020 update/Data/ANR_Fert2_data.xlsx")

# plot number imported as numeric, want to make it an integer so rounding to nearest whole number

dat$`Main plot no` <- round(dat$`Main plot no`, 0)

# removal treatments nested within nutrient treatments, so main plot number is really a block
names(dat)[2] <- "block"
# code treatments 
dat$`Main plot colour`[dat$`Main plot colour` == "Vit" | dat$`Main plot colour` == "vit"] <- "control"
dat$`Main plot colour`[dat$`Main plot colour` == "Röd" | dat$`Main plot colour` == "röd"] <- "full_nut"
dat$`Main plot colour`[dat$`Main plot colour` == "blå"] <- "NH4NO3"
dat$`Main plot colour`[dat$`Main plot colour` == "grön"] <- "NH4PO4"
dat$`Main plot colour`[dat$`Main plot colour` == "gul"] <- "KNO3"

dat$`Sub plot colour`[dat$`Sub plot colour` == "blå"] <- "no_removal"
dat$`Sub plot colour`[dat$`Sub plot colour` == "gul"] <- "full_removal"
dat$`Sub plot colour`[dat$`Sub plot colour` == "orange"] <- "Dflex_removal"
dat$`Sub plot colour`[dat$`Sub plot colour` == "vit"] <- "Eherm_removal"

# combine main plot and sub plot to get treatment column
dat$treatment <- paste(dat$`Main plot colour`, dat$`Sub plot colour`, sep= "_")

#make plot_id column
plot_ids <- unique(dat[,c(2,45)])
plot_ids$plot_id <- seq(1:nrow(plot_ids))
dat <- merge(dat, plot_ids)

# Code year to calendar year
dat$Year[dat$Year == 1] <- 1999
dat$Year[dat$Year == 2] <- 2002
dat$Year[dat$Year == 3] <- 2005
dat$Year[dat$Year == 4] <- 2010

dat$treatment_year <- dat$Year - 1998

# go from long to wide format
dat <- gather(dat, key = "genus_species", value = "abundance", 6:38)

# getting rid of 3 columns which summed cover by class
dat <- dat[-which(dat$genus_species == "TOTAL LICHENS" | dat$genus_species == "TOTAL BRYOPHYTES"| dat$genus_sp == "TOTAL VASCULAR"),]

# get rid of unnecessary columns
dat <- dat[,-c(4:12)]

# get rid of sp with 0 abundance
dat <- dat[which(dat$abundance > 0),] 

dat$site_code <- "ANR"
dat$project_name <- "Fert2"
dat$data_type <- "pin_hits"


# fix column names

names(dat)[3] <- "calendar_year"


#save
write.csv(dat, "Data/CleanedData/Sites/Species csv/ANR_Fert2.csv", row.names = FALSE)
