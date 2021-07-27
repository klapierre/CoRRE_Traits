setwd("~/Dropbox/CoRRE_database")

library(tidyr)
library(dplyr)

knz<-read.csv("Data/OriginalData/Sites/KNZ_KNP_GFP/KNZSpComp.csv")%>%
  select(-X)%>%
  mutate(plot2=ifelse(graze=="Grazed", 1, 2), plot3=ifelse(precip=="Rainout", 1,2), plot_id=paste(plot, plot2, plot3,sep="_"), treatment=paste(precip, graze, sep="_"), site_code="KNZ", project_name="GFP", community_type=burn)%>%
  gather(genus_species, abundance, Andropogon.gerardii:Euphorbia.marginatat)%>%
  select(-burn)%>%
  na.omit

knp<-read.csv("Data/OriginalData/Sites/KNZ_KNP_GFP/KNPSpComp.csv")%>%
  select(-X)%>%
  gather(genus_species, abundance, Bothriochloa.radicans:Unknown.Seedling)%>%
  mutate(plot2=ifelse(graze=="Grazed", 1, 2), plot3=ifelse(precip=="Rainout", 1,2), plot_id=paste(plot, plot2, plot3,sep="_"), treatment=paste(precip, graze, sep="_"), site_code="KNP", project_name="GFP", community_type=0)%>%
  select(-burn)%>%
  na.omit

species<-rbind(knp, knz)

treatment_year<-knz%>%
  select(year)%>%
  unique()%>%
  mutate(treatment_year=seq(1,3, by=1))

species2<-merge(treatment_year, species, by="year")%>%
  select(-precip, -graze, -plot, -plot2, -plot3)

write.csv(species2,"Data/CleanedData/Sites/Species csv/KNZ_KNP_GFP/KNZ_KNP_GFP.csv")


anppknz<-read.csv("Data/OriginalData/Sites/KNZ_KNP_GFP/KonzaBiomass_EndofSeason2011_forMeghan.csv")%>%
  mutate(plot2=ifelse(graze=="Grazed", 1, 2), plot3=ifelse(precip=="Rainout", 1,2), plot_id=paste(plot, plot2, plot3,sep="_"), treatment=paste(precip, graze, sep="_"))%>%
  tbl_df()%>%
  group_by(burn, plot_id, treatment)%>%
  summarize(anpp=mean(anpp))%>%
  mutate(calendar_year=2011, treatment_year=3, site_code="KNZ", project_name="GFP", community_type=burn)%>%

anppknz2<-anppknz[,-1]



anppknp<-read.csv("Data/OriginalData/Sites/KNZ_KNP_GFP/KrugerBiomass_forMeghan.csv")%>%
                    mutate(plot2=ifelse(graze=="Grazed", 1, 2), plot3=ifelse(precip=="Rainout", 1,2), plot_id=paste(plot, plot2, plot3,sep="_"), treatment=paste(precip, graze, sep="_"))%>%
                    tbl_df()%>%
                    group_by(plot_id, treatment)%>%
                    summarize(anpp=mean(anpp))%>%
                    mutate(calendar_year=2011, treatment_year=3, site_code="KNP", project_name="GFP", community_type=0)

anpp<-rbind(anppknp, anppknz2)

write.csv(anpp, "Data/CleanedData/Sites/ANPP csv/KNZ_KNP_GFP/KNZ_KNP_GFP_anpp.csv")
