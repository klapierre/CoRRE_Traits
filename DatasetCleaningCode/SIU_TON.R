################
## SIU_TON ####
###############

setwd("~/Dropbox/CoRRE_database")
setwd('C:\\Users\\komatsuk\\Dropbox (Smithsonian)\\working groups\\CoRRE\\CoRRE_database') #kim's desktop
setwd('C:\\Users\\lapie\\Dropbox (Smithsonian)\\working groups\\CoRRE\\CoRRE_database') #kim's laptop

#### Cover ####
df <- read.csv("Data/OriginalData/Sites/SIU_TON/TON_Div.csv")%>%
  mutate(abundance=ifelse(cov.1==1, 0.5, ifelse(cov.1==2, 3.5, ifelse(cov.1==3, 15, ifelse(cov.1==4, 37.5, ifelse(cov.1==5, 62.5, ifelse(cov.1==6, 85, ifelse(cov.1==7, 97.5, 0))))))))%>% #converting cover classes
  mutate(treatment=paste(trt.fert, trt.mow, sep=''))%>%
  rename(block=plot)%>%
  mutate(plot_id=paste(block, treatment, sep='_'))%>%
  filter(SPP_Old!='litter', !(SPP_Standardized %in% c('Soil', 'Grass', 'Moss')))%>%
  mutate(site_code='SIU', project_name='TON', community_type=0, data_type='cover', version=2.0)%>%
  rename(genus_species=SPP_Standardized, calendar_year=Year)%>%
  mutate(treatment_year=calendar_year-min(calendar_year)+1)%>%
  filter(block!=1)%>% #get rid of block 1, which was destroyed in 2013
  filter(!treatment %in% c('1C', '1S', '1B'))%>% #remove some treatments
  group_by(site_code, project_name, community_type, calendar_year, treatment_year, treatment, block, plot_id, genus_species, data_type, version)%>%
  summarise(abundance=mean(abundance))%>% #remove dupliate rows
  ungroup()%>%
  filter(abundance>0)


write.csv(df, "Data/CleanedData/Sites/Species csv/SIU_TON.csv", row.names = FALSE)


#### Biomass ####

df1 <- read.csv("Data/OriginalData/Sites/SIU_TON/TON_Biomass.csv")%>%
  mutate(treatment=paste(trt.fert, trt.mow, sep=''))%>%
  rename(block=PL)%>%
  mutate(plot_id=paste(block, treatment, sep='_'))%>%
  mutate(site_code='SIU', project_name='TON', community_type=0)%>%
  rename(calendar_year=Year)%>%
  mutate(treatment_year=calendar_year-min(calendar_year)+1)%>%
  filter(block!=1)%>% #get rid of block 1, which was destroyed in 2013
  filter(!treatment %in% c('1C', '1S', '1B'))%>% #remove some treatments
  group_by(site_code, project_name, community_type, calendar_year, treatment_year, treatment, block, plot_id)%>%
  summarise(biomass=sum(total))%>%
  ungroup()%>%
  mutate(anpp=2.5*biomass)%>%
  select(-biomass)

write.csv(df1, "Data/CleanedData/Sites/ANPP csv/SIU_TON_anpp.csv", row.names = FALSE)
