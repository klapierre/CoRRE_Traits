###################
### Bt_NPKDNet ####
##################


setwd("~/Dropbox/CoRRE_database")
setwd('C:\\Users\\lapie\\Dropbox (Smithsonian)\\working groups\\CoRRE\\CoRRE_database') #kim's laptop

# library
library(readxl)
library(tidyverse)

# control plots are the same as those in DroughtNet data
control <- read.csv("Data/OriginalData/Sites/Bt/clean_control_data.csv", row.names = 1)%>%
  mutate(project_name='NPKDNet', treatment='control')%>%
  filter(calendar_year>2017)%>%
  mutate(treatment_year=ifelse(calendar_year<2018, 0, (calendar_year - 2018+1)))

#importing NPKDnet data
dat <- read_excel(path = "Data/OriginalData/Sites/Bt/Bt_NPKDNet_cover_biomass2018-2020.xlsx", sheet = "cover18-20")%>%
  rename(site_code=site, plot_id=plot, abundance=cover, genus_species=taxa)%>%
  mutate(treatment=ifelse(plot_id %in% c('NR-FA-1', 'NR-FA-2', 'NR-FA-3', 'NR-FA-4', 'NR-FA-5'), 'NPK', ifelse(plot_id %in% c('CR-FA-1', 'CR-FA-2', 'CR-FA-3', 'CR-FA-4', 'CR-FA-5'), 'NPKdrought', 'control')))%>%
  filter(abundance>0, abundance!='NA')%>%
  mutate(calendar_year=as.numeric(format(date, format = "%Y")))%>%
  mutate(treatment_year=ifelse(calendar_year<2018, 0, (calendar_year - 2018+1)))%>%
  mutate(site_code='Bt', project_name='NPKDNet', community_type=0, data_type='cover')%>%
  select(-date, -note_cover)%>%
  rbind(control)%>%
  group_by(site_code, project_name, community_type, calendar_year, treatment_year, treatment, plot_id, genus_species, data_type)%>%
  summarise(abundance=max(abundance))%>%
  ungroup()

write.csv(dat, "Data/CleanedData/Sites/Species csv/Bt_NPKDNet.csv", row.names = FALSE)
  

####### biomass
dat <- read_excel(path = "Data/OriginalData/Sites/Bt/Bt_NPKDNet_cover_biomass2018-2020.xlsx", sheet = "biomass18-19")%>%
  select(-subplot, -note_biomass)%>%
  rename(functional_group=taxa, biomass=mass)
dat1 <- read_excel(path = "Data/OriginalData/Sites/Bt/Bt_NPKDNet_cover_biomass2018-2020.xlsx", sheet = "biomassFG20")%>%
  filter(biomass!='NA')%>%
  select(-project)

# control plots are the same as those in DroughtNet data - importing them too
control <- read.csv("Data/OriginalData/Sites/Bt/biomass_cntrol.csv")%>%
  filter(calendar_year>2017)%>%
  mutate(project_name="NPKDNet")

dat2 <- dat1%>%
  rbind(dat)%>%
  mutate(calendar_year=as.numeric(format(date, format = "%Y")))%>%
  rename(site_code=site, plot_id=plot)%>%
  mutate(treatment=ifelse(plot_id %in% c('NR-FA-1', 'NR-FA-2', 'NR-FA-3', 'NR-FA-4', 'NR-FA-5'), 'NPK', ifelse(plot_id %in% c('CR-FA-1', 'CR-FA-2', 'CR-FA-3', 'CR-FA-4', 'CR-FA-5'), 'NPKdrought', 'control')))%>%
  mutate(calendar_year=as.numeric(format(date, format = "%Y")))%>%
  mutate(treatment_year=ifelse(calendar_year<2018, 0, (calendar_year - 2018+1)))%>%
  mutate(site_code='Bt', project_name='NPKDNet', community_type=0)%>%
  select(-date)%>%
  group_by(site_code, project_name, community_type, treatment_year, calendar_year, treatment, plot_id)%>%
  summarise(anpp=sum(biomass))%>%
  ungroup()%>%
  rbind(control)

write.csv(dat2, "Data/CleanedData/Sites/ANPP csv/Bt_NPKDNet_anpp.csv", row.names = FALSE)
