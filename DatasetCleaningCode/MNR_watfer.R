setwd("~/Dropbox/converge_diverge/datasets/ORIGINAL_DATASETS/MNR_watfer")


library(tidyr)
library(dplyr)

species<-read.csv("species.csv")
###Ask what species clagara is in 2014 and filcal

sp11<-read.csv("sp2011.csv")%>%
  select(-ID, -Line, -Nutrients, -Light, -Carbon, -Water, -Other_Manipulation, -Treatment_Year, -Block, -Num_Manipulations, -Data_Type)%>%
  mutate(Treatment_Year=Experiment_Year, plot_id=Plot_ID, community_type=0, block=0)%>%
  gather(species, abundance, Achmol:Vulmyu)%>%
  select(-Plot_ID, -Experiment_Year)


sp12<-read.csv("sp2012.csv")%>%
  select(-ID, -Line, -Nutrients, -Light, -Carbon, -Water, -Other_Manipulation, -Treatment_Year, -Block, -Num_Manipulations, -Data_Type)%>%
  mutate(Treatment_Year=Experiment_Year, plot_id=Plot_ID, community_type=0, block=0)%>%
  gather(species, abundance, Amsmen:Vulmyu)%>%
  select(-Plot_ID, -Experiment_Year)


sp14<-read.csv("sp2014.csv")%>%
  select(-ID, -Line, -Nutrients, -Light, -Carbon, -Water, -Other_Manipulation, -Treatment_Year, -Block, -Num_Manipulations, -Data_Type)%>%
  mutate(Treatment_Year=Experiment_Year, plot_id=Plot_ID, community_type=0, block=0)%>%
  gather(species, abundance, Achmol:Vulmyu)%>%
  select(-Plot_ID, -Experiment_Year)

sp<-rbind(sp11, sp12, sp14)
sp2<-merge(sp, species, by="species", all=T)

write.csv(sp2, "MNR_watfer.csv")

anpp11<-read.csv("anpp2011.csv")%>%
  select(-ID, -Line, -Nutrients, -Light, -Carbon, -Water, -Other_Manipulation, -Treatment_Year, -Block, -Num_Manipulations)%>%
  mutate(Treatment_Year=Experiment_Year,
         Nutrients=1,
         Light=0,
         Carbon=0,
         Water=1,
         Other_Manipulations=0,
         Num_Manipulations=2)

anpp12<-read.csv("anpp2012.csv")%>%
  select(-ID, -Line, -Nutrients, -Light, -Carbon, -Water, -Other_Manipulation, -Treatment_Year, -Block, -Num_Manipulations)%>%
  mutate(Treatment_Year=Experiment_Year,
         Nutrients=1,
         Light=0,
         Carbon=0,
         Water=1,
         Other_Manipulations=0,
         Num_Manipulations=2)

anpp14<-read.csv("anpp2014.csv")%>%
  select(-ID, -Line, -Nutrients, -Light, -Carbon, -Water, -Other_Manipulation, -Treatment_Year, -Block, -Num_Manipulations)%>%
  mutate(Treatment_Year=Experiment_Year,
         Nutrients=1,
         Light=0,
         Carbon=0,
         Water=1,
         Other_Manipulations=0,
         Num_Manipulations=2)

anpp<-rbind(anpp11, anpp12, anpp14)

write.csv(anpp,"MNR_watfer.csv")


treat_info<-sp%>%
  select(-abundance, -species, -Plot_ID, -Calendar_Year, -Treatment_Year, -Experiment_Year)%>%
  unique()

write.csv(treat_info, "Exp_Info.csv")

##with exp info
###Ask what species clagara is in 2014.

sp11<-read.csv("sp2011.csv")%>%
  select(-ID, -Line)%>%
  mutate(Treatment_Year=Experiment_Year, plot_id=Plot_ID, community_type=0, block=0)%>%
  gather(species, abundance, Achmol:Vulmyu)%>%
  select(-Plot_ID, -Experiment_Year)


sp12<-read.csv("sp2012.csv")%>%
  select(-ID, -Line)%>%
  mutate(Treatment_Year=Experiment_Year, plot_id=Plot_ID, community_type=0, block=0)%>%
  gather(species, abundance, Amsmen:Vulmyu)%>%
  select(-Plot_ID, -Experiment_Year)


sp14<-read.csv("sp2014.csv")%>%
  select(-ID, -Line)%>%
  mutate(Treatment_Year=Experiment_Year, plot_id=Plot_ID, community_type=0, block=0)%>%
  gather(species, abundance, Achmol:Vulmyu)%>%
  select(-Plot_ID, -Experiment_Year)

sp<-rbind(sp11, sp12, sp14)
sp2<-merge(sp, species, by="species", all=T)

