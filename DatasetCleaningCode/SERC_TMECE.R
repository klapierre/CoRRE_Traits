setwd("~/Dropbox/converge_diverge/datasets/ORIGINAL_DATASETS/SERC_TMECE")

library(tidyr)
library(dplyr)

anpp<-read.csv("4-CO2xComm Total Shoot Biomass 1987-2013.csv")%>%
  select(calendar_year, community_type, treatment, plot_id, SCbiomass_m2,SPbiomass_m2,DIbiomass_m2,OTHERbiomass_m2)%>%
  filter(OTHERbiomass_m2!=-99, DIbiomass_m2!=-99)%>%
  mutate(anpp=SCbiomass_m2+SPbiomass_m2+DIbiomass_m2+OTHERbiomass_m2)

treatment_year<-anpp%>%
  select(calendar_year)%>%
  unique()%>%
  mutate(treatment_year=seq(1,27, by=1))

anpp2<-merge(anpp, treatment_year, by="calendar_year")%>%
  mutate(site_code="SERC",
         project_name="TMECE")%>%
  select(-SCbiomass_m2, -SPbiomass_m2, -DIbiomass_m2, -OTHERbiomass_m2)


write.csv(anpp2,"SERC_TMECE_anpp.csv")


count<-read.csv("CO2xComm Master C4 Harvest 1987-2013 (08-21-2015) (2).csv")%>%
  select(-SP_Green_Mass, -SP_Scenescent_Mass, -SP_Total_Mass, -DI_Green_Mass, -DI_Scenescent_Mass, -DI_Total_Mass, -Other_1_Mass, -Other_2_Mass, -C3_Dead_Mass, -C4_Dead_Mass, -Other_Dead_Species, -Other_Dead_Mass, -Total_Dead_Mass)%>%
  filter(calendar_year>1996, SP!=-99, DI!=-99)

othersp1<-count%>%
  mutate(species=Other_1_Species, count=Other_1_Count)%>%
  select(-SP, -DI, -Other_2_Species, -Other_2_Count, -Other_1_Species, -Other_1_Count)
othersp2<-count%>%
  mutate(species=Other_2_Species, count=Other_2_Count)%>%
  select(-SP, -DI, -Other_1_Species, -Other_1_Count, -Other_2_Species, -Other_2_Count)
othersp<-rbind(othersp1, othersp2)%>%
  filter(species!=0, count!=0)

spdi<-count%>%
  select(-Other_1_Species, -Other_1_Count, -Other_2_Species, -Other_2_Count)%>%
  gather(species, count, DI:SP)

c4species<-rbind(spdi, othersp)%>%
  tbl_df()%>%
  group_by(calendar_year, community_type, plot_id, treatment, species)%>%
  summarize(abundance=mean(count)/.0025)

c3count<-read.csv("1-CO2xComm Sedge Shoot Biomass 1987-2013.csv")%>%
  mutate(species="SC")%>%
  filter(calendar_year>1996, abundance!=-99)%>%
  select(-SCbiomass_m2)

species<-rbind(c3count, c4species)
species2<-merge(treatment_year, species, by="calendar_year")%>%
  mutate(site_code="SERC",
         project_name="TMECE")
genus_sp<-as.data.frame(species2[,7])%>%
  unique()

write.csv(genus_sp, "genus_species_list.csv")


specieslist<-read.csv("genus_species_list2.csv")
species3<-merge(species2, specieslist, by="species", all=T)%>%
  tbl_df()%>%
  group_by(site_code, project_name, community_type, calendar_year, treatment_year, treatment, plot_id, genus_species)%>%
  summarise(abundance=sum(abundance))%>%
  filter(treatment!="C")

write.csv(species3, "SERC_TMECE.csv")
