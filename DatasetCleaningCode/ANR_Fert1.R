##################
## ANR FERT1 ####
#################

setwd("~/Dropbox/CoRRE_database")
setwd('C:\\Users\\komatsuk\\Dropbox (Smithsonian)\\working groups\\CoRRE\\CoRRE_database')

# library
library(readxl)
library(tidyr)

# data
dat <- read_excel("Data/OriginalData/Sites/ANR_Fert1/ANR_Fert1_data_formatted.xls")
sp <- read_excel("Data/OriginalData/Sites/ANR_Fert1/ANR_Fert1_species.xlsx")
names(sp) <- c("sp_code", "genus_species")


## NB. Hierac column repeated in the data - going to combine these two (currently named "Hierac...16" & "Hierac...28")

dat$Hierac...16 <- dat$Hierac...16 + dat$Hierac...28
dat$Hierac...28 <- NULL
names(dat)[16] <- "Hierac"

# go from long to wide format
dat <- gather(dat, key = "sp_code", value = "abundance", 4:length(dat))

# convert species codes to species names
dat <- merge(dat, sp, all.x = TRUE)
# getting rid of 3 columns which summed cover by class
dat <- dat[-which(dat$sp_code == "bryophyt" | dat$sp_code == "lichens" | dat$sp_code == "vascular"),]
#NB. "Cornicul" sp_code does not have an associated genus_species. Emailed PI to see what this is. 
# Coding as unknown

dat$genus_species[dat$sp_code == "Cornicul"] <- "Unknown"

dat$sp_code <- NULL
dat$site_code <- "ANR"
dat$project_name <- "Fert1"
dat$treatment_year <- dat$Year - 1988
# a subset of treatments started one year after nutrient addition treatments
for(i in 1:nrow(dat)){
  if(dat$Treatment[i] == "GLUCOS"| 
    dat$Treatment[i] == "CACO3" |
    dat$Treatment[i] == "ACTIVATED CARBON"|
    dat$Treatment[i] == "REDUCTION"|
    dat$Treatment[i] == "PROTEIN")
    dat$treatment_year[i] <- dat$Year[i]-1989
}

dat$data_type <- "cover"

# code treatments 
dat$Treatment[dat$Treatment == "VIT"] <- "control"
dat$Treatment[dat$Treatment == "RÖD"] <- "full_nut"
dat$Treatment[dat$Treatment == "BLÅ"] <- "NH4NO3"
dat$Treatment[dat$Treatment == "GRÖN"] <- "NH4PO4"
dat$Treatment[dat$Treatment == "GUL"] <- "KNO3"
dat$Treatment[dat$Treatment == "ORANGE"] <- "micronut"

# fix column names

names(dat)[c(1,2,3)] <- c("calendar_year", "treatment", "plot_id")

# get rid of sp with 0 abundance
dat <- dat[which(dat$abundance > 0),]

###split into fert1
fert1 <- dat%>%
  filter(treatment %in% c('control', 'full_nut', 'NH4NO3', 'NH4PO4', 'KNO3', 'micronut'))
  
#save
write.csv(fert1, "Data/CleanedData/Sites/Species csv/ANR_Fert1.csv", row.names = FALSE)


#split into fert2
fert3 <- dat%>%
  filter(!(treatment %in% c('full_nut', 'NH4NO3', 'NH4PO4', 'KNO3', 'micronut')))%>%
  mutate(treatment_year=ifelse(treatment=='control', treatment_year-1, treatment_year))%>%
  filter(calendar_year>1990)
  
#save
write.csv(fert3, "Data/CleanedData/Sites/Species csv/ANR_Fert3.csv", row.names = FALSE)
