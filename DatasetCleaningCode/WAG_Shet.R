###################
### WAG_Shet #####
#################

setwd("/Users/kaitlinkimmel/Dropbox/CoRRE_database")


# library
library(tidyr)

# data
dat <- read.csv("Data/OriginalData/2020 update/Data/WAG_Shet.csv")
sp <- read.csv("Data/OriginalData/2020 update/Data/WAG_Shet_sp.csv")

# need to sum over the 4 measurements per plot to get biomass / 40cm2

dat <- aggregate(dat[8:23], by = list(calendar_year = dat$Year, plot_id = dat$Plot.ID, block = dat$Block,
                                      treatment = dat$Treatment), FUN = sum)
dat <- gather(dat, key = "sp_code", value = "abundance", 5:20)

dat <- merge(dat, sp)

dat <- dat[-1]
dat$treatment_year <- dat$calendar_year - 2014
dat$site_code <- "WAG"
dat$project_name <- "SHet"
dat$data_type <- "biomass"

write.csv(dat, "Data/CleanedData/Sites/Species csv/WAG_SHet.csv", row.names = FALSE)

bio_dat <- aggregate(dat$abundance, by = list(calendar_year = dat$calendar_year, plot_id = dat$plot_id, 
                                              block = dat$block, treatment = dat$treatment, 
                                              site_code = dat$site_code, project_name = dat$project_name, 
                                              data_type = dat$data_type, treatment_year = dat$treatment_year), FUN = sum)
names(bio_dat)[9] <- "anpp"

# anpp is currently per 40cm2, need to make it in terms of m2

bio_dat$anpp <- bio_dat$anpp/0.004 #check with meghan to make sure this makes sense

write.csv(bio_dat, "Data/CleanedData/Sites/ANPP csv/WAG_Shet_anpp.csv", row.names = FALSE)



