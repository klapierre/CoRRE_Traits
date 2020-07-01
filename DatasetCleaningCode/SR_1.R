setwd("~/Dropbox/converge_diverge/datasets/ORIGINAL_DATASETS/seabloom/exp1")

library(dplyr)
library(tidyr)

dat<-read.csv("invade-exp-data-output-full-plant-biomass_CLEAN.csv")
#nitrogen experiment

nit<-dat%>%
  filter(exp=="NUTRIENT")%>% #have to drop this b/c not a resource manip
    mutate(treatment=paste(seed,trt, sep="_"),
         community_type=plant, 
         plot_id=plot,
         project_name="Nitrogen",
         calendar_year=year,
         site_code="SR",
         genus_species=taxa,
         abundance=mass)%>%
  select(site_code, project_name, block, plot_id, community_type, treatment, calendar_year, genus_species, abundance)

treatment_year<-nit%>%
  select(calendar_year)%>%
  arrange(calendar_year)%>%
  unique()%>%
  mutate(treatment_year=seq(1,5, by=1))

nit2<-merge(treatment_year, nit, by="calendar_year")%>%
  filter(genus_species!="LITTER")

write.csv(nit2, "SR_Nitrogen.csv")

nit_anpp<-nit2%>%
  tbl_df()%>%
  group_by(site_code, project_name, block, plot_id, community_type, treatment, calendar_year, treatment_year)%>%
  summarize(anpp=sum(abundance))
  
write.csv(nit_anpp,"SR_Nitrogen_anpp.csv")
###water experiment
wat<-dat%>%
  filter(exp=="WATER")%>% #have to drop this b/c not a resource manip
  mutate(treatment=paste(seed,trt,gopher, sep="_"),
         community_type=plant, 
         plot_id=plot,
         project_name="Water",
         calendar_year=year,
         site_code="SR",
         genus_species=taxa,
         abundance=mass)%>%
  select(site_code, project_name, block, plot_id, community_type, treatment, calendar_year, genus_species, abundance)

treatment_year<-wat%>%
  select(calendar_year)%>%
  arrange(calendar_year)%>%
  unique()%>%
  mutate(treatment_year=seq(1,5, by=1))

wat2<-merge(treatment_year, wat, by="calendar_year")%>%
  filter(genus_species!="LITTER")

write.csv(wat2, "SR_Water.csv")

wat_anpp<-wat2%>%
  tbl_df()%>%
  group_by(site_code, project_name, block, plot_id, community_type, treatment, calendar_year, treatment_year)%>%
  summarize(anpp=sum(abundance))

write.csv(wat_anpp,"SR_Water_anpp.csv")
