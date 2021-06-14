###################
### Bt_NPKDNet ####
##################


setwd("/Users/kaitlinkimmel/Dropbox/CoRRE_database")

# library
library(readxl)
library(tidyr)

dat <- read_excel(path = "Data/OriginalData/2020 update/Data/Bt/Bt_NPKDNet_cover_biomass2018-2020.xlsx", sheet = "cover18-20")
# control plots are the same as those in DroughtNet data - importing them too - 
plot_id <- data.frame(plot = unique(dat$plot))
plot_id$plot_id <- seq(1:nrow(plot_id))

dat <- merge(dat, plot_id)
dat$treatment <-substr(dat$plot,1,2)
dat$treatment[dat$treatment == "CR"] <- "drought*fert"
dat$treatment[dat$treatment == "NR"] <- "fert"

dat$calendar_year <- as.numeric(format(dat$date, format = "%Y"))
dat$month <- as.numeric(format(dat$dat, format = "%m"))

dat <- aggregate(dat$cover, by = list(plot_id = dat$plot_id, genus_species = dat$taxa, treatment = dat$treatment, calendar_year = dat$calendar_year), FUN = max)
names(dat)[5] <- "abundance"
dat$data_type <- "cover"
dat$site <- "Bt"
dat$project_name <- "NPKDNet"
dat$treatment_year <- dat$calendar_year-2017
write.csv(dat, "Data/CleanedData/Sites/Species csv/Bt_NPKDNet.csv", row.names = FALSE)
  
