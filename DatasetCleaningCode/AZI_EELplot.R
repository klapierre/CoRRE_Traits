#####################
### AZI_EELplot ####
####################

setwd("~/Dropbox/CoRRE_database")
setwd('C:\\Users\\komatsuk\\Dropbox (Smithsonian)\\working groups\\CoRRE\\CoRRE_database') #kim's desktop
setwd('C:\\Users\\lapie\\Dropbox (Smithsonian)\\working groups\\CoRRE\\CoRRE_database') #kim's laptop


# library
library(readxl)
library(tidyverse)

# data
dat <- read_excel("Data/OriginalData/Sites/AZI_EELplot_data.xls")%>%
  mutate(site_code='AZI', project_name='EELplot', community_type=0)%>%
  rename(plot_id=Plot, block=Block, treatment=Treatment, calendar_year=Year, genus_species=Species, abundance=Abundance)%>%
  filter(abundance>0)%>%
  mutate(treatment_year=calendar_year-min(calendar_year)+1)%>%
  select(site_code, project_name, community_type, block, plot_id, calendar_year, treatment_year, treatment, genus_species, abundance)

write.csv(dat, "Data/CleanedData/Sites/Species csv/AZI_EELplot.csv", row.names = FALSE)

bio_dat <- read_excel("Data/OriginalData/Sites/AZI_EELplot_data.xls")%>%
  mutate(site_code='AZI', project_name='EELplot', community_type=0)%>%
  rename(plot_id=Plot, block=Block, treatment=Treatment, calendar_year=Year, genus_species=Species)%>%
  mutate(treatment_year=calendar_year-min(calendar_year)+1)%>%
  group_by(site_code, project_name, community_type, block, plot_id, calendar_year, treatment_year, treatment)%>%
  summarise(anpp=sum(ANPP))%>%
  ungroup()

write.csv(bio_dat, "Data/CleanedData/Sites/ANPP csv/AZI_EELplot_anpp.csv", row.names = FALSE)
