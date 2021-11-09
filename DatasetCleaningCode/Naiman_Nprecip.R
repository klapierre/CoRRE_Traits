######################
##### Naiman_Nprecip ####
#####################
#setwd("~/Dropbox/CoRRE_database")
setwd('C:\\Users\\lapie\\Dropbox (Smithsonian)\\working groups\\CoRRE\\CoRRE_database') #kim's laptop

library(tidyverse)

cover <- read.csv('Data\\OriginalData\\Sites\\Naiman_Nprecip\\Naiman County Nitrogen addition experiment.csv')%>%
  gather(key="genus_species", value="abundance", Artemisia.argyi:Salsola.collina)%>%
  filter(abundance!='NA')%>%
  mutate(site_code='Naiman', project_name='Nprecip', community_type=0, version=2.0, data_type='biomass')%>%
  mutate(treatment=paste(fertilization, water, sep='_'))%>%
  rename(calendar_year=year, plot_id=plot)%>%
  mutate(treatment_year=calendar_year-min(calendar_year)+1)

# write.csv(cover, "Data/CleanedData/Sites/Species csv/Naiman_Nprecip.csv", row.names = FALSE)



##### ANPP ####
anpp <- cover%>%
  group_by(site_code, project_name, community_type, treatment, calendar_year, treatment_year, version, plot_id)%>%
  summarise(anpp=sum(abundance))%>%
  ungroup()

# write.csv(anpp, "Data/CleanedData/Sites/ANPP csv/Naiman_Nprecip_anpp.csv", row.names=F)