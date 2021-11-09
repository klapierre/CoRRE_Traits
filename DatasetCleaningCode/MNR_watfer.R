setwd("~/Dropbox/CoRRE_database")
setwd("C:\\Users\\lapie\\Dropbox (Smithsonian)\\working groups\\CoRRE\\CoRRE_database\\")


library(tidyr)
library(dplyr)

species<-read.csv("Data/OriginalData/Sites/MNR_watfer/species.csv")
###Ask what species clagara is in 2014 and filcal

sp11<-read.csv("Data/OriginalData/Sites/MNR_watfer/sp2011.csv")%>%
  select(-ID, -Line, -Nutrients, -Light, -Carbon, -Water, -Other_Manipulation, -Treatment_Year, -Block, -Num_Manipulations, -Data_Type)%>%
  mutate(Treatment_Year=Experiment_Year, plot_id=Plot_ID, community_type=0, block=0)%>%
  gather(species, abundance, Achmol:Vulmyu)%>%
  select(-Plot_ID, -Experiment_Year)


sp12<-read.csv("Data/OriginalData/Sites/MNR_watfer/sp2012.csv")%>%
  select(-ID, -Line, -Nutrients, -Light, -Carbon, -Water, -Other_Manipulation, -Treatment_Year, -Block, -Num_Manipulations, -Data_Type)%>%
  mutate(Treatment_Year=Experiment_Year, plot_id=Plot_ID, community_type=0, block=0)%>%
  gather(species, abundance, Amsmen:Vulmyu)%>%
  select(-Plot_ID, -Experiment_Year)


sp14<-read.csv("Data/OriginalData/Sites/MNR_watfer/sp2014.csv")%>%
  select(-ID, -Line, -Nutrients, -Light, -Carbon, -Water, -Other_Manipulation, -Treatment_Year, -Block, -Num_Manipulations, -Data_Type)%>%
  mutate(Treatment_Year=Experiment_Year, plot_id=Plot_ID, community_type=0, block=0)%>%
  gather(species, abundance, Achmol:Vulmyu)%>%
  select(-Plot_ID, -Experiment_Year)

sp<-rbind(sp11, sp12, sp14)
sp2<-merge(sp, species, by="species", all=T)%>%
  rename(site_code=Site_Code, project_name=Project_Name, treatment=Treatment, calendar_year=Calendar_Year, treatment_year=Treatment_Year, genus_species=species_name)%>%
  select(-species)

write.csv(sp2, "Data/CleanedData/Sites/Species csv/MNR_watfer.csv", row.names=F)

anpp11<-read.csv("Data/OriginalData/Sites/MNR_watfer/anpp2011.csv")%>%
  select(-ID, -Line, -Nutrients, -Light, -Carbon, -Water, -Other_Manipulation, -Treatment_Year, -Block, -Num_Manipulations)%>%
  mutate(Treatment_Year=Experiment_Year,
         Nutrients=1,
         Light=0,
         Carbon=0,
         Water=1,
         Other_Manipulations=0,
         Num_Manipulations=2)

anpp12<-read.csv("Data/OriginalData/Sites/MNR_watfer/anpp2012.csv")%>%
  select(-ID, -Line, -Nutrients, -Light, -Carbon, -Water, -Other_Manipulation, -Treatment_Year, -Block, -Num_Manipulations)%>%
  mutate(Treatment_Year=Experiment_Year,
         Nutrients=1,
         Light=0,
         Carbon=0,
         Water=1,
         Other_Manipulations=0,
         Num_Manipulations=2)

anpp14<-read.csv("Data/OriginalData/Sites/MNR_watfer/anpp2014.csv")%>%
  select(-ID, -Line, -Nutrients, -Light, -Carbon, -Water, -Other_Manipulation, -Treatment_Year, -Block, -Num_Manipulations)%>%
  mutate(Treatment_Year=Experiment_Year,
         Nutrients=1,
         Light=0,
         Carbon=0,
         Water=1,
         Other_Manipulations=0,
         Num_Manipulations=2)

anpp<-rbind(anpp11, anpp12, anpp14)%>%
  rename(site_code=Site_Code, project_name=Project_Name, treatment=Treatment, calendar_year=Calendar_Year, treatment_year=Treatment_Year, plot_id=Plot_ID, anpp=Biomass)%>%
  mutate(community_type=0)%>%
  select(site_code, project_name, community_type, plot_id, treatment, calendar_year, treatment_year, anpp)

write.csv(anpp,"Data/CleanedData/Sites/ANPP csv/MNR_watfer.csv", row.names=F)


# treat_info<-sp%>%
#   select(-abundance, -species, -Plot_ID, -Calendar_Year, -Treatment_Year, -Experiment_Year)%>%
#   unique()
# 
# write.csv(treat_info, "Data/OriginalData/Sites/MNR_watfer/Exp_Info.csv")
# 
# ##with exp info
# ###Ask what species clagara is in 2014.
# 
# sp11<-read.csv("sp2011.csv")%>%
#   select(-ID, -Line)%>%
#   mutate(Treatment_Year=Experiment_Year, plot_id=Plot_ID, community_type=0, block=0)%>%
#   gather(species, abundance, Achmol:Vulmyu)%>%
#   select(-Plot_ID, -Experiment_Year)
# 
# 
# sp12<-read.csv("sp2012.csv")%>%
#   select(-ID, -Line)%>%
#   mutate(Treatment_Year=Experiment_Year, plot_id=Plot_ID, community_type=0, block=0)%>%
#   gather(species, abundance, Amsmen:Vulmyu)%>%
#   select(-Plot_ID, -Experiment_Year)
# 
# 
# sp14<-read.csv("sp2014.csv")%>%
#   select(-ID, -Line)%>%
#   mutate(Treatment_Year=Experiment_Year, plot_id=Plot_ID, community_type=0, block=0)%>%
#   gather(species, abundance, Achmol:Vulmyu)%>%
#   select(-Plot_ID, -Experiment_Year)
# 
# sp<-rbind(sp11, sp12, sp14)
# sp2<-merge(sp, species, by="species", all=T)
# 
