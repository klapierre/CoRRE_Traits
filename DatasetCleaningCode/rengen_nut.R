###################
## Rengen_Nut ####
##################

setwd("/Users/kaitlinkimmel/Dropbox/CoRRE_database")


#libraries
library(tidyr)
library(readxl)

dat <- read_excel(path = "Data/OriginalData/2020 update/Data/Rengen_Nut_data.xlsx")
# one column with no species name - ask Meghan to email provider

dat <- gather(dat, key = genus_species, value = abundance, 4:length(dat)) #wide to long format
dat <- dat[which(dat$abundance >0),] #get rid of species with 0 abundance
plot_id <- unique(dat[,c(1,2)]) #get unique combos of treatment and rep
plot_id$plot_id <- seq(1:nrow(plot_id)) #create unique plot id for each treatment/rep combo
dat <- merge(dat, plot_id) #add in plot id
# Add in treatments
dat$Treatment[dat$Treatment == "A"] <- "Control"
dat$Treatment[dat$Treatment == "B"] <- "Ca"
dat$Treatment[dat$Treatment == "C"] <- "CaN"
dat$Treatment[dat$Treatment == "D"] <- "CaNP"
dat$Treatment[dat$Treatment == "E"] <- "CaNP-KCl"
dat$Treatment[dat$Treatment == "F"] <- "CaNP-K2SO4"

dat <- dat[,-2] # get rid of replicate column

names(dat)[c(1,2)] <- c("treatment", "calendar_year") #change names 
# add in other information
dat$data_type <- "cover"
dat$treatment_year <- dat$calendar_year - 1940
dat$site_code <- "Rengen"
dat$project_name <- "Nut"

write.csv(dat, "Data/CleanedData/Sites/Species csv/Rengen_Nut.csv", row.names = FALSE)
