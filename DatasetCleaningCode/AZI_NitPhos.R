setwd("~/Dropbox/converge_diverge/datasets/ORIGINAL_DATASETS/AZI_NitPhos")

library(dplyr)
library(tidyr)

dat2014<-read.csv("data2014.csv")
dat2012<-read.csv("data2012.csv")
dat2013<-read.csv("data2013.csv")

data<-rbind(dat2014, dat2013, dat2012)

treatment_year<-data%>%
  select(calendar_year)%>%
  unique()%>%
  arrange(calendar_year)%>%
  mutate(treatment_year=seq(2,4, by=1))

data2<-merge(data, treatment_year, by="calendar_year")%>%
  mutate(site_code="AZI", project_name="NitPhos")

species<-data2%>%
  filter(data_type=="cover")%>%
  gather(genus_species, abundance, Scirpus.pumilus:Vicia.sepium.Linn)%>%
  na.omit

write.csv(species, "AZI_NitPhos.csv")

anpp<-data2%>%
  filter(data_type=="biomass")%>%
  gather(genus_species, biomass, Scirpus.pumilus:Vicia.sepium.Linn)%>%
  mutate(biomass2=as.numeric(biomass))%>%
  na.omit%>%
  tbl_df()%>%
  group_by(plot_id, data_type, treatment, treatment_year, calendar_year, site_code, project_name)%>%
  summarize(anpp=sum(biomass2)*4)

write.csv(anpp, "AZI_NitPhos_anpp.csv")
