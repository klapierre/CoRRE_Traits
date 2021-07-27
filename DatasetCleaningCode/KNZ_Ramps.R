setwd("~/Dropbox/CoRRE_database")

library(dplyr)
library(tidyr)

splist<-read.csv("Data/OriginalData/Sites/KNZ_RAMPS/ramps_updated/konza_spplist.csv")%>%
  mutate(genus_species=paste(genus, spp.1, sep="_"))%>%
  select(spnum2, genus_species)


sp1<-read.csv("Data/OriginalData/Sites/KNZ_RAMPS/ramps_updated/RaMPs_03_13_SpComp_w0s_v3.csv")%>%
  gather(spnum2, abundance, spp2:spp410)%>%
  filter(precip!="control", type!=0)%>%#this drop half the plots to account for spatial array
  mutate(treatment=paste(precip, heat, sep="_"), 
         plot_id=paste(ramp, subplot, sep="_"))

sp<-sp1%>%
  tbl_df()%>%
  group_by(calendar_year, spnum2, treatment, plot_id)%>%
  summarize(abundance=mean(abundance))


sp2<-merge(splist, sp, by="spnum2")%>%
  select(-spnum2)

treatment_year<-sp2%>%
  select(calendar_year)%>%
  arrange(calendar_year)%>%
  unique()%>%
  mutate(treatment_year=seq(6,16, by=1))

sp3<-merge(treatment_year, sp2, by="calendar_year")%>%
  mutate(site_code="KNZ", project_name="RaMPs")

write.csv(sp3, "Data/CleanedData/Sites/Species csv/KNZ_RaMPs.csv")

anpp<-read.csv("Data/OriginalData/Sites/KNZ_RAMPS/ramps_updated/Ramps_ANPP_forRegs.csv")%>%
  mutate(treatment=paste(trt, subtrt, sep="_"))%>%
  select(calendar_year, block, ramp, treatment, total)

plots<-sp1%>%
  select(ramp, plot_id, treatment)%>%
  unique

anpp2<-merge(anpp, plots, by=c("ramp", "treatment"))
anpp3<-merge(anpp2, treatment_year, by="calendar_year")%>%
  select(-ramp)%>%
  mutate(site_code="KNZ", project_name="RaMPs")

write.csv(anpp3, "Data/CleanedData/Sites/ANPP csv/KNZ_RaMPs_anpp.csv")
