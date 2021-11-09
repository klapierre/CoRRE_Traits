####################
## HAYS_PRECIP ####
###################

setwd("~/Dropbox/CoRRE_database")
setwd("C:\\Users\\lapie\\Dropbox (Smithsonian)\\working groups\\CoRRE\\CoRRE_database\\") #kim's laptop

# library
library(readxl)
library(tidyverse)

#read in data
dat <- read_excel("Data/OriginalData/Sites/HAYS/HAYS_Precip.xlsx")%>%
  select(-GRAM, -FORB, -SHRUB, -RUDERAL, -TOT, -PER, -ANN)%>%
  gather(key = "sp_code", value = "cover", 6:66)%>%
  group_by(YEAR, Block, PLOT, TREAT, sp_code)%>%
  summarise(abundance=sum(cover))%>% #summing across replicates within each plot
  ungroup()%>%
  filter(abundance>0)%>%
  mutate(plot_id=paste(Block, PLOT, sep='_'))%>%
  mutate(treatment=ifelse(TREAT==1, 'reduction', ifelse(TREAT==2, 'control', 'add')))%>%
  rename(calendar_year=YEAR, block=Block)%>%
  mutate(site_code='HAYS', project_name='Precip', community_type=0, data_type='cover', version=2.0)%>%
  mutate(treatment_year=calendar_year-min(calendar_year)+1)%>%
  left_join(read_excel("Data/OriginalData/Sites/HAYS/HAYS_splist.xlsx"))%>% #join on species names
  select(site_code, project_name, community_type, calendar_year, treatment_year, treatment, block, plot_id, genus_species, abundance, data_type, version)

write.csv(dat, "Data/CleanedData/Sites/Species csv/HAYS_Precip.csv", row.names = FALSE)
