setwd("~/Dropbox/converge_diverge/datasets/ORIGINAL_DATASETS/SERC_CXN")

dat<-read.csv("4-CO2xN Total Shoot Biomass 2005-2014 (1).csv")

anpp<-dat%>%
  mutate(anpp=SCbiomass_m2+SPbiomass_m2+DIbiomass_m2)%>%
  select(calendar_year, plot_id, treatment, anpp)
names(anpp)[names(anpp)=="treatment"]<-"treat1"

anpp2<-anpp%>%
  mutate(treat="t", treatment=paste(treat, treat1, sep=""))%>%
  select(-treat, -treat1)%>%
  mutate(site_code="SERC", project_name="CXN")

species<-dat
names(species)[names(species)=="SCbiomass_m2"]<-"Scirpus.olneyi"
names(species)[names(species)=="SPbiomass_m2"]<-"Spartina.patens"
names(species)[names(species)=="DIbiomass_m2"]<-"Distichlis.spicata"
names(species)[names(species)=="treatment"]<-"treat1"

species2<-species%>%
  select(-CO2, -Nitrogen, -SCdensity_m2, -SPdensity_m2, -DIdensity_m2, -Litterbiomass_m2)%>%
  gather(genus_species, abundnace,Scirpus.olneyi:Distichlis.spicata)%>%
  mutate(treat="t", treatment=paste(treat, treat1, sep=""))%>%
  select(-treat, -treat1)%>%
  mutate(site_code="SERC", project_name="CXN")

treatment_year<-anpp%>%
  select(calendar_year)%>%
  unique()%>%
  mutate(treatment_year=seq(1,10, by=1))

anpp3<-merge(anpp2, treatment_year, by="calendar_year")
write.csv(anpp3, "SERC_CXN_anpp.csv")


species3<-merge(species2, treatment_year, by="calendar_year")
write.csv(species3, "SERC_CXN.csv")
