setwd("~/Dropbox/converge_diverge/datasets/ORIGINAL_DATASETS/TAS_FACE")

library(dplyr)
library(tidyr)

abund<-read.csv("Tasface_abund_trt.csv")%>%
  gather(genus_species, abundance, Acaena.echinata:Wurmbea.dioica)

treatment_year<-abund%>%
  select(calendar_year)%>%
  unique()%>%
  mutate(treatment_year=seq(2,8, by=1))

spdata<-merge(abund, treatment_year, by="calendar_year")%>%
  mutate(site_code="TAS",
         project_name="FACE")

write.csv(spdata, "TAS_FACE.csv")
