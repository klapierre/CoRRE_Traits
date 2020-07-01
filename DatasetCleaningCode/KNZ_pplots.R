setwd("~/Dropbox/converge_diverge/datasets/ORIGINAL_DATASETS/KNZ_pplots_newdata")

library(tidyr)
library(dplyr)

#the species codes are all messed up I have to use this new data
species<-read.csv("PPLOTS_SpCom_Plot_SpAcross.csv")%>%
  select(-nitro, -phos, -N, -P)%>%
  gather(spnum2, abundance, spp2:spp194)%>%
  mutate(site_code="KNZ", project_name="pplots")

treatment_year<-species%>%
  select(calendar_year)%>%
  unique()%>%
  mutate(treatment_year=seq(0,12, by=1))

species2<-merge(species, treatment_year, by="calendar_year")

specieslist<-read.csv("konza_spplist.csv")%>%
  mutate(genus_species=paste(genus, spp.1, sep="_"))%>%
  select(spnum2, genus_species)

species3<-merge(species2, specieslist, by="spnum2")%>%
  select(-spnum2)

write.csv(species3, "KNZ_PPLOTS.csv")


#biomass
old<-read.delim("KNZ_PPLOTS_anpp.txt")%>%
  select(site_code, project_name, treatment, calendar_year, treatment_year, plot_id, anpp)
new<-read.csv("PPlots_Biomass_02-14.csv")
plotinfo<-read.csv("treatments.csv")

new2<-merge(plotinfo, new, by=c("row","plot"))%>%
  mutate(anpp=(grass+forb+woody)*10, site_code="KNZ",project_name="PPLOTS")%>%
  select(site_code, project_name, calendar_year, plot_id, anpp)%>%
  tbl_df()%>%
  group_by(site_code, project_name, calendar_year, plot_id)%>%
  summarize(anpp=mean(anpp))

treatinfo<-species%>%
  select(plot_id, treatment)%>%
  unique()

new3<-merge(treatinfo, new2, by="plot_id")
new4<-merge(treatment_year, new3, by="calendar_year")

anpp<-rbind(new4, old)

write.csv(anpp, "KNZ_PPLOTS_anpp.csv")
