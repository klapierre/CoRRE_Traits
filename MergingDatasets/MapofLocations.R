
setwd('C:\\Users\\lapie\\Dropbox (Smithsonian)\\working groups\\CoRRE\\converge_diverge\\datasets\\longform') #kim's laptop
setwd('C:\\Users\\komatsuk\\Dropbox (Smithsonian)\\working groups\\CoRRE\\CoRRE_database\\Data\\CompiledData') #kim's desktop

setwd('C:\\Users\\mavolio2\\Dropbox\\converge_diverge\\datasets\\LongForm')
setwd("~/Dropbox/converge_diverge/datasets/LongForm")


library(ggmap)
library(tidyverse)
library(maps)
library(mapdata)
library(mapproj)

loc<-read.csv("siteLocationClimate.csv")%>%
  left_join(read.csv('ExperimentInfo.csv'))%>%
  left_join(read.csv('siteBiotic.csv'))%>%
  group_by(site_code)%>%
  summarise(lat=mean(Latitude), long=mean(Longitude), gamma=mean(rrich))%>%
  ungroup()

anppsites<-loc%>%
  filter(site_code=="ANG"|site_code=="CDR"|site_code=="DL"|site_code=="IMGERS"|site_code=="KBS"|site_code=="KLU"|site_code=="KNZ"|site_code=="maerc"|site_code=="NWT"|site_code=="SERC"|site_code=="SEV")

##try with ggplot, don't love this

mapworld <- borders("world", colour="gray80", fill="gray80") # create a layer of borders

ggplot() +
  mapworld +
  geom_point(data=loc, aes(x=long, y=lat, color=gamma), position=position_jitter(width=.1, height=.1), size=3) +
  theme(panel.border = element_blank()) +
  theme(panel.background = element_blank()) +
  theme(axis.ticks = element_blank()) +
  theme(axis.text = element_blank()) +
  scale_color_continuous(name="Gamma\nDiversity") +
  labs(x=NULL, y=NULL)
#export at 2000x1000

anpp_loc<-read.csv("siteList_LatLong_ANPP paper.csv")%>%
  mutate(site_code=name)%>%
  left_join(read.csv('SiteExperimentDetails_Dec2016.csv'))%>%
  group_by(site_code)%>%
  summarise(lat=mean(latitude), long=mean(longitude), MAP=mean(MAP))%>%
  ungroup()

##try with ggplot, don't love this

mapworld <- borders("world", colour="gray80", fill="gray80") # create a layer of borders

ggplot() +
  mapworld +
  geom_point(data=anpp_loc, aes(x=long, y=lat, color=MAP), position=position_jitter(width=.1, height=.1), size=1) +
  theme(panel.border = element_blank()) +
  theme(panel.background = element_blank()) +
  theme(axis.ticks = element_blank()) +
  theme(axis.text = element_blank()) +
  scale_color_continuous(name="Mean Annual\nPrecipitatoin") +
  labs(x=NULL, y=NULL)
#export at 2000x1000

