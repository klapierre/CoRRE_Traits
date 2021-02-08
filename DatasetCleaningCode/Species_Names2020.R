###########################
### Species cleaning ####
########################
setwd("/Users/kaitlinkimmel/Dropbox/CoRRE_database/")

# load libraries
library(Taxonstand)

# import data
full.list <- read.csv ("Data/CompiledData/SpeciesList.csv", row.names = 1)
oldsp <- read.csv("Data/CompiledData/CoRRE_TRY_species_list.csv", row.names = 1)

oldsp$oldsp <- 1 # create a column to indicate name was in old species list
oldsp1 <- oldsp[,c(1,4)] # get rid of extra columns

merged.list<- merge(full.list, oldsp1, all.x = TRUE) #merge new list with old list

# get species that are not in old list 
unidentified.sp <- data.frame(genus_species=merged.list[which(is.na(merged.list$oldsp)),1])

clean.list <- TPL(unidentified.sp$genus_species)

matched.sp <- clean.list[clean.list$Plant.Name.Index == TRUE | clean.list$Taxonomic.status == "Accepted",]
unmatched.sp <- clean.list[clean.list$Plant.Name.Index == FALSE & clean.list$Taxonomic.status != "Accepted",]


