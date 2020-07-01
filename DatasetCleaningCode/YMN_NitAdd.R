setwd("~/Dropbox/converge_diverge/datasets/ORIGINAL_DATASETS/YMN_NitAdd")

library(dplyr)
library(tidyr)

sp13<-read.csv("sp2013.csv")

sp14<-read.csv("sp2014.csv")

sp15<-read.csv("sp2015.csv")
all<-rbind(sp13, sp14, sp15)

sp<-all%>%
  gather(genus_species, abundance,Heteropappus.altaicus: Ixeridium.chinense)

treatment_year<-sp%>%
  select(calendar_year)%>%
  unique()%>%
  mutate(treatment_year=seq(1,3, by=1))

sp2<-merge(sp, treatment_year, by="calendar_year")%>%
  mutate(site_code="YMN",
         project_name="NitAdd")%>%
  select(-rep)

speciesdata<-sp2%>%
  select(-anpp)

write.csv(speciesdata, "YMN_NitAdd.csv")

anppdata<-sp2%>%
  select(site_code, project_name, treatment, calendar_year, treatment_year, plot_id, anpp)%>%
  unique()

write.csv(anppdata, "YMN_NitAdd_anpp.csv")


