setwd("~/Dropbox/CoRRE_database")

data<-read.csv("Data/OriginalData/Sites/LEFT_PME/Lefthand_precip_experiment_data2.csv")

anpp<-data%>%
  mutate(anpp=aboveground_biomass, site_code="LEFT", project_name="PME")%>%
  select(calendar_year, month, plot_id, block, treatment, anpp, site_code, project_name)%>%
  na.omit

treatment_year<-anpp%>%
  select(calendar_year)%>%
  unique()%>%
  mutate(treatment_year=seq(2,4, by=1))

anpp2<-merge(anpp, treatment_year, by="calendar_year")

write.csv(anpp2, "Data/CleanedData/Sites/ANPP csv/LEFT_PME_anpp.csv")


species<-data%>%
  select(-aboveground_biomass)%>%
  gather(genus_species, abundance, Alyssum_parviflorum:Tragopogon_dubius)%>%
  tbl_df()%>%
  group_by(calendar_year, plot_id, block, treatment, genus_species)%>%
  summarize(abundance=max(abundance))%>%
  mutate(site_code="LEFT", project_name="PME")

species2<-merge(species, treatment_year, by="calendar_year")

write.csv(species2, "Data/CleanedData/Sites/Species csv/LEFT_NME.csv")
  