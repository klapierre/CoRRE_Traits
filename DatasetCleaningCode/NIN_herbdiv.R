######################
#### NIN_herbdiv ####
####################
setwd("~/Dropbox/CoRRE_database")
setwd('C:/Users/lapie/Dropbox (Smithsonian)/working groups/CoRRE/CoRRE_database') #kim's laptop

library(tidyverse)

df <- read.csv("Data/OriginalData/Sites/NIN_HerbDiv/HerbDiv_2019_Cover_Tx_Trait_toKaitlin.csv")%>%
  filter(!(EXCL %in% c('Psuedocage', 'Trnch CTL')), Scientific.name!='na')%>%
  mutate(fertilization=ifelse(Fert=='NoFert', 'NF', 'F'))%>%
  mutate(treatment=paste(EXCL_Code, fertilization, sep=''))%>%
  mutate(site_code='NIN', project_name='HerbDiv', community_type=0, data_type='cover', version=2.0)%>%
  rename(calendar_year=Year, plot_id=Plot, genus_species=Scientific.name, block=Block, abundance=Cover)%>%
  mutate(treatment_year=calendar_year-min(calendar_year)+1)%>%
  select(site_code, project_name, community_type, calendar_year, treatment_year, treatment, block, plot_id, genus_species, abundance, data_type, version)%>%
  group_by(site_code, project_name, community_type, calendar_year, treatment_year, treatment, block, plot_id, genus_species, data_type, version)%>% 
  summarise(abundance=mean(abundance))%>% #a subset of the data is repeated, means to remove repeats
  ungroup()

write.csv(df, "Data/CleanedData/Sites/Species csv/NIN_herbdiv.csv", row.names = FALSE)


# setwd("~/Dropbox/converge_diverge/datasets/ORIGINAL_DATASETS/houseman and russell/herbdiv")
# 
# library(tidyr)
# library(dplyr)
# 
# treatinfo<-read.delim('NIN_herbdiv.txt')%>%
#   select(plot_id, treatment)%>%
#   unique()
# 
# species<-read.csv("HerbDiv Plant Comp2015 CovDivgWrkGrp.csv")
# 
# treatment_year<-species%>%
#   select(calendar_year)%>%
#   unique()%>%
#   mutate(treatment_year=seq(1,6,by=1))
# 
# species2<-merge(species, treatment_year, by="calendar_year")
# species3<-merge(species2, treatinfo, by="plot_id")%>%
#   mutate(site_code="NIN", project_name="herbdiv")
# spcode<-read.csv("sp_codes.csv")
# species4<-merge(species3,spcode, by="genus_species")%>%
#   mutate(genus_species=Scientific.name)%>%
#   select(-Scientific.name)
# 
# write.csv(species4, "NIN_herbdiv.csv")
# 
# ###can ignore everything below
# 
# specieslist<-species%>%
#   select(genus_species)%>%
#   unique()
# 
# write.csv(specieslist, "NIN_species.csv")
# 
# #plot_id probelm - use plots from ealier the other 16 are structural controls
# 
# plots1<-species%>%
#   select(plot_id)%>%
#   unique()
# 
# plots<-merge(treatinfo, plots1, by="plot_id", all=T)
# write.csv(plots, "NIN_plots.csv")
