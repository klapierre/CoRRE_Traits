###################
### Bt_NPKDNet ####
##################


setwd("/Users/kaitlinkimmel/Dropbox/CoRRE_database")

# library
library(readxl)
library(tidyr)

dat <- read_excel(path = "Data/OriginalData/2020 update/Data/Bt/Bt_NPKDNet_cover_biomass2018-2020.xlsx", sheet = "cover18-20")
# control plots are the same as those in DroughtNet data - importing them too
control <- read.csv("Data/OriginalData/2020 update/Data/Bt/clean_control_data.csv", row.names = 1)
control <- control[-which(control$calendar_year < 2018),]
control$treatment_year <- control$calendar_year -2017
control$project_name <- "NPKDNet"

plot_id <- data.frame(plot = unique(dat$plot))
plot_id$plot_id <- seq(11,20)

dat <- merge(dat, plot_id)
dat$treatment <-substr(dat$plot,1,2)
dat$treatment[dat$treatment == "CR"] <- "drought*fert"
dat$treatment[dat$treatment == "NR"] <- "fert"

dat$calendar_year <- as.numeric(format(dat$date, format = "%Y"))
dat$month <- as.numeric(format(dat$dat, format = "%m"))

dat <- aggregate(dat$cover, by = list(plot_id = dat$plot_id, genus_species = dat$taxa, treatment = dat$treatment, calendar_year = dat$calendar_year), FUN = max)
names(dat)[5] <- "abundance"
dat$data_type <- "cover"
dat$site_code <- "Bt"
dat$project_name <- "NPKDNet"
dat$treatment_year <- dat$calendar_year-2017
dat <- dat[,-c(10,11)]

test <- rbind(dat, control)

write.csv(test, "Data/CleanedData/Sites/Species csv/Bt_NPKDNet.csv", row.names = FALSE)
  
####### biomass


dat <- read_excel(path = "Data/OriginalData/2020 update/Data/Bt/Bt_NPKDNet_cover_biomass2018-2020.xlsx", sheet = "biomass18-19")
dat1 <- read_excel(path = "Data/OriginalData/2020 update/Data/Bt/Bt_NPKDNet_cover_biomass2018-2020.xlsx", sheet = "biomassFG20")
# control plots are the same as those in DroughtNet data - importing them too
control <- read.csv("Data/OriginalData/2020 update/Data/Bt/biomass_cntrol.csv")
control <- control[-which(control$calendar_year < 2018),]
control$treatment_year <- control$calendar_year - 2017
control$project_name <- "NPKDNet"

dat$calendar_year <- as.numeric(format(dat$date, format = "%Y"))
dat <- merge(dat, plot_id)
dat$treatment <-substr(dat$plot,1,2)
dat$treatment[dat$treatment == "CR"] <- "drought*fert"
dat$treatment[dat$treatment == "NR"] <- "fert"
dat <- aggregate(dat$mass, by = list(calendar_year = dat$calendar_year, treatment = dat$treatment, plot_id =dat$plot_id), FUN = sum)
names(dat)[4] <- 'anpp'
dat1$calendar_year <- as.numeric(format(dat1$date, format = "%Y"))
dat1 <- merge(dat1, plot_id)
dat1$treatment <-substr(dat1$plot,1,2)
dat1$treatment[dat1$treatment == "CR"] <- "drought*fert"
dat1$treatment[dat1$treatment == "NR"] <- "fert"
dat1 <- dat1[-which(is.na(dat1$biomass)),]
dat1 <- aggregate(dat1$biomass, by = list(calendar_year = dat1$calendar_year, treatment = dat1$treatment, plot_id =dat1$plot_id), FUN = sum)
names(dat1)[4] <- 'anpp'
dat <- rbind(dat,dat1)

dat$site_code <- "Bt"
dat$project_name <- "NPKDNet"
dat$treatment_year <- dat$calendar_year -2017

findat <- rbind(dat,control)

write.csv(findat, "Data/CleanedData/Sites/ANPP csv/Bt_NPKDNet.csv", row.names = FALSE)
