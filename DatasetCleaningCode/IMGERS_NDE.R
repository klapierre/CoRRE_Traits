setwd("~/Dropbox/CoRRE_database")

library(tidyr)
library(dplyr)

data<-read.csv("Data/OriginalData/Sites/IMGERS_NDE/metadata-2011-2014fromChina-Yunhai Zhang.csv")%>%
  gather(genus_species, abundance, Leymus.chinensis:Gentianopsis.barbata)%>%
  na.omit
treat<-read.csv("Data/OriginalData/Sites/IMGERS_NDE/treatments.csv")

treatment_year<-data%>%
  select(calendar_year)%>%
  unique()%>%
  mutate(treatment_year=seq(4,6, by=1))

anpp<-data%>%
  tbl_df()%>%
  group_by(plot_id, block, treatment_num, calendar_year)%>%
  summarize(anpp=sum(abundance))

anpp2<-merge(anpp, treatment_year, by="calendar_year")%>%
  mutate(site_code="IMGERS", project_name="NDE")

anpp3<-merge(treat, anpp2, by="treatment_num")%>%
  filter(treatment_num!=1&treatment_num!=20)%>%
  select(-treatment_num)

write.csv(anpp3, "Data/CleanedData/Sites/ANPP csv/IMGERS_NDE_anpp.csv")

species<-merge(data, treatment_year, by="calendar_year")
species2<-merge(species, treat, by="treatment_num")%>%
  filter(treatment_num!=1&treatment_num!=20)%>%
  select(-treatment_num)%>%
  mutate(site_code="IMGERS", project_name="NDE")

write.csv(species2, "Data/CleanedData/Sites/Species csv/IMGERS_NDE.csv")
