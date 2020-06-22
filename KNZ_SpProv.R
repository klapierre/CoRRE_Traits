## Konza Species Providence

# set wd
setwd("/Users/kaitlinkimmel/Dropbox/CoRRE_database")

# Libraries
library(Hmisc)

## Read in data
# Dataset with native status
Konza_sp <- read.csv("~/Dropbox/CoRRE_database/Data/OriginalData/Species Providence/2020 sp list_Konza.csv")

#Konza experiment species
KNZ_BGP <- read.csv("~/Dropbox/CoRRE_database/Contacting Data Providers/Site_Sp_lists/KNZ_BGP.csv")
KNZ_IRG <- read.csv("~/Dropbox/CoRRE_database/Contacting Data Providers/Site_Sp_lists/KNZ_IRG.csv")
KNZ_pplots <- read.csv("~/Dropbox/CoRRE_database/Contacting Data Providers/Site_Sp_lists/KNZ_pplots.csv")
KNZ_RaMPs <- read.csv("~/Dropbox/CoRRE_database/Contacting Data Providers/Site_Sp_lists/KNZ_RaMPs.csv")
KNZ_RHPs <- read.csv("~/Dropbox/CoRRE_database/Contacting Data Providers/Site_Sp_lists/KNZ_RHPs.csv")
KNZ_GFP <- read.csv("~/Dropbox/CoRRE_database/Contacting Data Providers/Site_Sp_lists/KNZ_GFP.csv")


## Get data ready to combine
# Create column for species_provided in Konza_sp dataset
Konza_sp$Species_accepted <- capitalize(paste(Konza_sp$genus, Konza_sp$species, sep = " "))

Konza_sp <- Konza_sp[,c(9,11)]
for(i in 1:nrow(Konza_sp)){
  if(Konza_sp$origin[i] == "i"){
    Konza_sp$origin[i] <- "Non"
  } else{
    Konza_sp$origin[i] <- "Native"
  }
}
names(Konza_sp)[1] <- "Native_status"

KNZ_BGP$Site <- "KNZ"
KNZ_BGP$Experiment <- "BGP"
KNZ_IRG$Site <- "KNZ"
KNZ_IRG$Experiment <- "IRG"
KNZ_pplots$Site <- "KNZ"
KNZ_pplots$Experiment <- "pplots"
KNZ_RaMPs$Site <- "KNZ"
KNZ_RaMPs$Experiment <- "RaMPS"
KNZ_RHPs$Site <- "KNZ"
KNZ_RHPs$Experiment <- "RHPs"
KNZ_GFP$Site <- "KNZ"
KNZ_GFP$Experiment <- "GFP"


#Combine Data

KNZ_BGP <- merge(KNZ_BGP, Konza_sp, all.x = TRUE)
KNZ_BGP <- KNZ_BGP[,c(3,4,1,2,5)]
KNZ_IRG <- merge(KNZ_IRG, Konza_sp, all.x = TRUE)
KNZ_IRG <- KNZ_IRG[,c(3,4,1,2,5)]
KNZ_pplots <- merge(KNZ_pplots, Konza_sp, all.x = TRUE)
KNZ_pplots <- KNZ_pplots[,c(3,4,1,2,5)]
KNZ_RaMPs <- merge(KNZ_RaMPs, Konza_sp, all.x = TRUE)
KNZ_RaMPs <- KNZ_RaMPs[,c(3,4,1,2,5)]
KNZ_RHPs <- merge(KNZ_RHPs, Konza_sp, all.x = TRUE)
KNZ_RHPs <- KNZ_RHPs[,c(3,4,1,2,5)]
KNZ_GFP <- merge(KNZ_GFP, Konza_sp, all.x = TRUE)
KNZ_GFP <- KNZ_GFP[,c(3,4,1,2,5)]


# Some sp have NA values. Will hand check to see if these are in species_provided instead of species_accepted
# save into folder

write.csv(KNZ_BGP, "~/Dropbox/CoRRE_database/Data/OriginalData/Species Providence/KNZ_BGP_NativeStatus.csv")
write.csv(KNZ_IRG, "~/Dropbox/CoRRE_database/Data/OriginalData/Species Providence/KNZ_IRG_NativeStatus.csv")
write.csv(KNZ_pplots, "~/Dropbox/CoRRE_database/Data/OriginalData/Species Providence/KNZ_pplots_NativeStatus.csv")
write.csv(KNZ_RaMPs, "~/Dropbox/CoRRE_database/Data/OriginalData/Species Providence/KNZ_RaMPs_NativeStatus.csv")
write.csv(KNZ_RHPs, "~/Dropbox/CoRRE_database/Data/OriginalData/Species Providence/KNZ_RHPs_NativeStatus.csv")
write.csv(KNZ_GFP, "~/Dropbox/CoRRE_database/Data/OriginalData/Species Providence/KNZ_GFP_NativeStatus.csv")



