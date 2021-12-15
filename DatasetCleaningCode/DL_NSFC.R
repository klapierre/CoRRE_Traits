#################
####DL_NSFC ####
################
library(tidyverse)

setwd("~/Dropbox/CoRRE_database")
setwd('C:\\Users\\lapie\\Dropbox (Smithsonian)\\working groups\\CoRRE\\CoRRE_database\\')

### biomass data from 2004-2012
nsfc_names<-read.delim("Data\\OriginalData\\Sites\\DL_NSFC\\DL_NSFC_specieslist.txt")%>%
  mutate(species_code=tolower(species_list))

df1 <- read.delim("Data\\OriginalData\\Sites\\DL_NSFC\\DL_NSFC.txt")%>%
  gather(species_code, abundance, sp1:sp53)%>%
  left_join(nsfc_names)%>%
  filter(abundance>0)%>%
  mutate(community_type=0)%>%
  select(site_code, project_name, community_type, plot_id, treatment, calendar_year, treatment_year, genus_species, abundance)

### biomass data from 2013-2016
df2 <- read.csv("Data\\OriginalData\\Sites\\DL_NSFC\\DL species biomass-3-16.csv")%>%
  rename(calendar_year=ï..calendar_year)%>%
  select(calendar_year:Cynanchum_thesioides)%>%
  filter(!(is.na(calendar_year)))%>%
  mutate(site_code='DL', project_name='NSFC', community_type=0, treatment_year=(calendar_year-2004))%>%
  rename(treatment2=treatment)%>%
  mutate(treatment=ifelse(treatment2=='CK', 'C', ifelse(treatment2=='N10', 'N', ifelse(treatment2=='WCK', 'W', 'WN'))))%>%
  gather(genus_species, abundance, Stipa_krylovii:Cynanchum_thesioides)%>%
  filter(abundance>0)%>%
  select(site_code, project_name, community_type, plot_id, treatment, calendar_year, treatment_year, genus_species, abundance)%>%
  separate(genus_species, into=c("genus", "species"), sep='_', remove=F)%>%
  select(-genus_species)%>%
  mutate(genus_species=paste(genus, species, sep=' '))%>%
  select(site_code, project_name, community_type, plot_id, treatment, calendar_year, treatment_year, genus_species, abundance)


### combine data
df <- rbind(df1, df2) #need to fix the plot id for df2 to be consecutive and not repeating

# write.csv(df, "Data\\CleanedData\\Sites\\Species csv\\DL_NSFC.csv", row.names=F)


# get total biomass production
anpp <- df%>%
  group_by(site_code, project_name, community_type, plot_id, treatment, calendar_year, treatment_year)%>%
  summarise(anpp=sum(abundance))%>%
  ungroup()

# write.csv(anpp, "Data\\CleanedData\\Sites\\ANPP csv\\DL_NSFC_anpp.csv", row.names = FALSE)

