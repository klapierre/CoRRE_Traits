setwd("~/Dropbox/converge_diverge/datasets/ORIGINAL_DATASETS/CDR e001")

library(dplyr)
library(tidyr)

splist<-read.csv("e001mega13.csv")%>%
  mutate(treatment=NTrt,
         calendar_year=Year,
         plot_id=Plot,
         community_type=Field)%>%
  gather(genus_species, abundance, Acernegu:Ziziapte)

treatment_year<-splist%>%
  select(calendar_year)%>%
  arrange(calendar_year)%>%
  unique()%>%
  mutate(treatment_year=seq(1,31, by=1))

sp3<-merge(treatment_year, splist, by="calendar_year")%>%
  select(calendar_year, treatment_year, community_type, plot_id, treatment, genus_species, abundance)%>%
  mutate(site_code="CDR", project_name="e001")%>%
  na.omit%>%
  filter(abundance>0)

write.csv(sp3, "CDR_e001.csv")

anpp<-sp3%>%
  ungroup()%>%
  filter(genus_species!="Fungi"&genus_species!="Misclitt")%>%
  group_by(calendar_year, treatment_year, community_type, plot_id, treatment, site_code, project_name)%>%
  summarize(anpp=sum(abundance))

write.csv(anpp, "CDR_e001_anpp.csv")
