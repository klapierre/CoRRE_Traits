####################################
## Species from each experiment ####
####################################

# This is run to pull species found in each experiment so that the lists can be emailed
# to the data providers to get species provenance data

#libraries
library(tidyverse)

# set working directory 
setwd("~/Dropbox/CoRRE_database/")

# read in merged file with of species abunances
corre <- read.csv("Data/CompiledData/RawAbundance.csv")
nutnetprov <- read.csv("Data/OriginalData/2020 update/Data/NutNet/nutnet_cover_01272021.csv",row.names = 1)

species_list<-corre%>%
  select(site_code, project_name, genus_species)%>%
  unique()

# select new sites
species_list_new <- species_list %>% filter(site_code %in% c("ANR", "AZI", "DCIMC", "Hayoka", "HAYS", "HPGRS", "KUFS", "Rengen", "SGS", "Sil", "SIU", "SORBAS"),
                                            !project_name %in% c("NitPhos", "EDGE"))

site_proj <- unique(species_list_new[,c(1,2)])
sp.list <- list()
for(i in 1:nrow(site_proj)){
  sp.list[[i]] <- species_list_new %>%filter(site_code == site_proj$site_code[i],
                                        project_name == site_proj$project_name[i])
  sp.list[[i]]$Native_status <- NA
} 

for(i in 1:length(sp.list)){
  write.csv(sp.list[[i]], paste("Contacting Data Providers/Site_Sp_lists/", site_proj$site_code[i], site_proj$project_name[i], ".csv", sep =""))
}

#### cleaning up nutnet
nutnetprov <- unique(nutnetprov[,c(2,3,10,12)])
nutnetprov <- nutnetprov %>% mutate(local_provenance = ifelse(local_provenance == "NAT", "native", 
                                           ifelse(local_provenance == "INT", "Exotic", 
                                                  ifelse(local_provenance == "UNK", "Unknown", NA))))
nutnetprov$project_name <- "NutNet"
nutnetprov <- nutnetprov[,-1]
names(nutnetprov)[c(2,3)] <- c("genus_species", "Native_status")       

write.csv(nutnetprov, "Data/OriginalData/Species Provenance/NutNet_NativeStatus.csv", row.names = FALSE)
                                    
                                    