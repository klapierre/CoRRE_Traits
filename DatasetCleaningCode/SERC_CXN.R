setwd("~/Dropbox/CoRRE_database")
setwd("C:\\Users\\mavolio2\\Dropbox\\CoRRE_database")

dat<-read.csv("Data/OriginalData/Sites/SERC_CXN/SERC CO2xN 2005-2019 zeroes removed.csv")

anpp<-dat%>%
  rename(treatment=Treatment, 
         community_type=Community) %>% 
  group_by(site_code, project_name, community_type, block, plot_id, calendar_year, treatment_year, treatment) %>% 
  summarize(anpp=sum(abundance))
  
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
write.csv(anpp, "Data/CleanedData/Sites/ANPP csv/SERC_CXN_anpp.csv")


species3<-merge(species2, treatment_year, by="calendar_year")
write.csv(species3, "Data/CleanedData/Sites/Species csv/SERC_CXN.csv")
