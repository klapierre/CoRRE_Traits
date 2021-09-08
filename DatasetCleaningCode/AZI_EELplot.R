#####################
### AZI_EELplot ####
####################

setwd("~/Dropbox/CoRRE_database")
setwd('C:\\Users\\komatsuk\\Dropbox (Smithsonian)\\working groups\\CoRRE\\CoRRE_database')

# library
library(readxl)
library(tidyr)

# data
dat <- read_excel("Data/OriginalData/Sites/AZI_EELplot_data.xls")
dat <- dat[,-6]
dat$treatment_year <- dat$Year - 2015
dat$site_code <- "AZI"
dat$project_name <- "EELplot"
names(dat)[c(1:7)] <- c("plot_id", "block", "treatment", "calendar_year", "genus_species", "density", "anpp")

bio_dat <- dat[,-6]

dat <- gather(dat, key = "data_type", value = "density", 6:7)
dat$data_type[dat$data_type == "density"] <- "density"
dat <- dat[which(dat$density> 0),]
dat2 <- dat%>%
  select(-data_type)%>%
  mutate(data_type='density')

write.csv(dat2, "Data/CleanedData/Sites/Species csv/AZI_EELplot.csv", row.names = FALSE)

bio_dat <- aggregate(bio_dat$anpp, by = list(calendar_year = bio_dat$calendar_year, treatment = bio_dat$treatment, 
                                          plot_id = bio_dat$plot_id, block = bio_dat$block, site_code = bio_dat$site_code, 
                                          project_name = bio_dat$project_name, treatment_year = bio_dat$treatment_year),
                         FUN = sum)
names(bio_dat)[8] <- "anpp"
bio_dat$data_type <- "anpp"

write.csv(bio_dat, "Data/CleanedData/Sites/ANPP csv/AZI_EELplot_anpp.csv", row.names = FALSE)
