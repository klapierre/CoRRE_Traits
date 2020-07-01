setwd("~/Dropbox/converge_diverge/datasets/ORIGINAL_DATASETS/IMGERS_NDE")

library(tidyr)
library(dplyr)

data<-read.csv("metadata-2011-2014fromChina-Yunhai Zhang.csv")%>%
  gather(genus_species, abundance, Leymus.chinensis:Gentianopsis.barbata)%>%
  na.omit
treat<-read.csv("treatments.csv")

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

write.csv(anpp3, "IMGERS_NDE_anpp.csv")

species<-merge(data, treatment_year, by="calendar_year")
species2<-merge(species, treat, by="treatment_num")%>%
  filter(treatment_num!=1&treatment_num!=20)%>%
  select(-treatment_num)%>%
  mutate(site_code="IMGERS", project_name="NDE")

write.csv(species2, "IMGERS_NDE.csv")
