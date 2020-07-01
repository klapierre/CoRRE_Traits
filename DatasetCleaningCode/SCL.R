setwd("~/Dropbox/converge_diverge/datasets/ORIGINAL_DATASETS/SCL_Pedro_Datasets")

library(dplyr)
library(tidyr)

ter<-read.csv("SCL_TER_raw.csv")%>%
  select(-Forb.seedling)%>%
  gather(genus_species, abundance, Achyrocline.satureioides:Viola.arvensis)

treatment_year<-ter%>%
  select(calendar_year)%>%
  unique()%>%
  mutate(treatment_year=seq(1,4, by=1))

ter2<-merge(treatment_year, ter, by="calendar_year")%>%
  mutate(site_code="SCL", project_name="TER")

write.csv(ter2, "SCL_TER.csv")

luc<-read.csv("SCL_Lucero_raw.csv")%>%
  select(-Unknown.seedling)%>%
  gather(genus_species, abundance, Achyrocline.satureioides:Viola.arvensis)

treatment_year<-luc%>%
  select(calendar_year)%>%
  unique()%>%
  mutate(treatment_year=seq(1,6, by=1))

luc2<-merge(treatment_year, luc, by="calendar_year")%>%
  mutate(site_code="SCL", project_name="Lucero")

write.csv(luc2, "SCL_Lucero.csv")
