##################
## Glen_Fert ####
################

setwd("~/Dropbox/CoRRE_database")

# library
library(readxl)
library(tidyr)

dat <- read_excel("Data/OriginalData/Sites/Glen_Fert/Glen_Fert_data1.xlsx")
enviro <- read_excel("Data/OriginalData/Sites/Glen_Fert/Glen_Fert_data2.xlsx")

# remove first row because just species abbreviations

dat <- dat[-1,]

# wide to long 
dat <- gather(dat, key = "genus_species", value = "abundance", 2:75)
# get rid of 0's 
dat <- dat[dat$abundance >0, ]
# merge with enviro data
dat <- merge(dat, enviro)

plot_id <- data.frame("siteplot" = unique(dat$siteplot))
plot_id$plot_id <- seq(1:nrow(plot_id))
dat <- merge(dat,plot_id)
dat <- dat[,-c(1,2,6,9,10)]

#two valleys = different blocks
names(dat)[c(3,4)] <- c("calendar_year", "block")

dat$site_code <- "Glen"
dat$project_name <- "Fert"
dat$data_type <- "cover"
dat$treatment_year <- dat$calendar_year - 1992

write.csv(dat, "Data/CleanedData/Sites/Species csv/Glen_Fert.csv", row.names = FALSE)

