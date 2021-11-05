###################
#### NWT_ATWE ####
#################
#setwd("~/Dropbox/CoRRE_database")
setwd('C:\\Users\\lapie\\Dropbox (Smithsonian)\\working groups\\CoRRE\\CoRRE_database\\Data\\OriginalData\\Sites\\NWT_ATWE') #kim's laptop

library(tidyverse)

df <- read.csv('Winkler_ATWE_mixed_model_data.csv')%>%
  group_by(year, plot, species.code, treatment)%>%
  summarise(abundance=mean(cover), anpp=mean(biomass))%>%
  ungroup()%>%
  rename(Veg.Code=species.code)%>%
  left_join(read.csv('Winkler_ATWE_species_list.csv'))%>%
  mutate(site_code='NWT', project_name='ATWE', community_type=0, data_type='cover', version=2.0)%>%
  rename(calendar_year=year, plot_id=plot)%>%
  mutate(genus_species=paste(Genus, Species, sep='_'))%>%
  mutate(treatment_year=calendar_year-2009)%>%
  select(site_code, project_name, community_type, calendar_year, treatment_year, treatment, plot_id, genus_species, abundance , anpp)

cover <- df%>%
  select(-anpp)

# write.csv(cover, "C:\\Users\\lapie\\Dropbox (Smithsonian)\\working groups\\CoRRE\\CoRRE_database\\Data\\CleanedData\\Sites\\Species csv\\NWT_ATWE.csv", row.names = FALSE)


anpp <- df%>%
  group_by(site_code, project_name, community_type, calendar_year, treatment_year, treatment, plot_id)%>%
  summarise(anpp=sum(anpp))%>%
  ungroup()


# write.csv(anpp, "C:\\Users\\lapie\\Dropbox (Smithsonian)\\working groups\\CoRRE\\CoRRE_database\\Data\\CleanedData\\Sites\\ANPP csv\\NWT_ATWE.csv", row.names = FALSE)