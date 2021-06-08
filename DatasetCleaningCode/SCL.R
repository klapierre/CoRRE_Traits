setwd("/Users/kaitlinkimmel/Dropbox/CoRRE_database")

library(dplyr)
library(tidyr)

ter<-read.csv("Data/OriginalData/Sites/SCL_Pedro_datasets/SCL_TER_raw.csv")%>%
  select(-Forb.seedling)%>%
  gather(genus_species, abundance, Achyrocline.satureioides:Viola.arvensis)

ter$treatment_year <- ter$calendar_year - 2003

ter2<-ter %>% mutate(site_code="SCL", project_name="TER", data_type = "cover")

write.csv(ter2, "Data/CleanedData/Sites/Species csv/SCL_TER.csv", row.names = FALSE)

luc<-read.csv("Data/OriginalData/Sites/SCL_Pedro_datasets/SCL_Lucero_raw.csv")%>%
  select(-Unknown.seedling)%>%
  gather(genus_species, abundance, Achyrocline.satureioides:Viola.arvensis)

treatment_year<-luc%>%
  select(calendar_year)%>%
  unique()%>%
  mutate(treatment_year=seq(1,6, by=1))

luc2<-merge(treatment_year, luc, by="calendar_year")%>%
  mutate(site_code="SCL", project_name="Lucero", data_type = "cover")

write.csv(luc2, "Data/CleanedData/Sites/Species csv/SCL_Lucero.csv", row.names = FALSE)
