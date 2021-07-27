setwd("~/Dropbox/CoRRE_database")

library(dplyr)
library(tidyr)

dat<-read.csv("Data/OriginalData/Sites/TRA_Lovegrass/TRA_Lovegrass_raw.csv")

dat2<-dat%>%
  select(-baresoil, -branchg, -Dates, -grazing, -Grazing_1, -control, -nitrogen)%>%
  gather(genus_species, pinhits, Eragrostis_curvula:Lichen)%>%
  mutate(treatment=ifelse(Treatment=="nscc","nsc",as.character(Treatment)), 
         abundance=(pinhits/Total_Points)*100,
         plot_id=paste(block, plot_id1, sep="_"))%>%
  tbl_df()%>%
  group_by(block, plot_id, calendar_year, treatment, genus_species)%>%
  summarize(abundance=max(abundance))

treatment_year<-dat%>%
  select(calendar_year)%>%
  arrange(calendar_year)%>%
  unique()%>%
  mutate(treatment_year=seq(1,3, by=1))

dat3<-merge(dat2, treatment_year, by="calendar_year")%>%
  mutate(site_code="TRA", project_name="Lovegrass")%>%
  filter(abundance!=0)

write.csv(dat3, "Data/CleanedData/Sites/Species csv/TRA_Lovegrass.csv")

