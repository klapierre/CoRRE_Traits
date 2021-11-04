##################
## EGN Nmow ####
#################

setwd("~/Dropbox/CoRRE_database")
setwd('C:\\Users\\komatsuk\\Dropbox (Smithsonian)\\working groups\\CoRRE\\CoRRE_database') #kim's desktop
setwd('C:\\Users\\lapie\\Dropbox (Smithsonian)\\working groups\\CoRRE\\CoRRE_database') #kim's laptop

# library
library(readxl)
library(tidyr)

# data
dat <- read.csv("Data/OriginalData/Sites/EGN_Nmow/EGN_Nmow.csv")%>%
  select(-Replication)

dat$site_code <- "EGN"
dat$project_name <- "Nmow"
dat$treatment_year <- dat$calendar_year - min(dat$calendar_year) + 1
dat$data_type <- "cover"
dat$community_type <- 0

#save
write.csv(dat, "Data/CleanedData/Sites/Species csv/EGN_Nmow.csv", row.names = FALSE)

