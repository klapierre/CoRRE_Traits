library(tidyverse)

setwd('C:\\Users\\komatsuk\\Dropbox (Smithsonian)\\nutrient network\\NutNet data') #kim's desktop

cover <- read.csv('full-cover-07-December-2020.csv')%>%
  mutate(keep=ifelse(site_code %in% c('shps.us', 'lancaster.uk', 'cbgb.us', 'lake.us', 'potrok.ar', 'yarra.au', 'cdcr.us', 'sier.us', 'chilcas.ar'), 1, ifelse(site_code=='temple.us' & trt %in% c('Control', 'NPK'), 1, 0)))%>%
  filter(keep==1)%>%
  select(-keep)
write.csv(cover, 'C:\\Users\\komatsuk\\Dropbox (Smithsonian)\\working groups\\CoRRE\\CoRRE_database\\Data\\OriginalData\\2020 update\\nutnet\\nutnet_cover_01052021.csv')


anpp <- read.csv('full-biomass-07-December-2020.csv')%>%
  mutate(keep=ifelse(site_code %in% c('shps.us', 'lancaster.uk', 'cbgb.us', 'lake.us', 'potrok.ar', 'yarra.au', 'cdcr.us', 'sier.us', 'chilcas.ar'), 1, ifelse(site_code=='temple.us' & trt %in% c('Control', 'NPK'), 1, 0)))%>%
  filter(keep==1)%>%
  select(-keep)
write.csv(anpp, 'C:\\Users\\komatsuk\\Dropbox (Smithsonian)\\working groups\\CoRRE\\CoRRE_database\\Data\\OriginalData\\2020 update\\nutnet\\nutnet_anpp_01052021.csv')


siteData <- read.csv('comb-by-plot-clim-soil-07-December-2020.csv')%>%
  mutate(keep=ifelse(site_code %in% c('shps.us', 'lancaster.uk', 'cbgb.us', 'lake.us', 'potrok.ar', 'yarra.au', 'cdcr.us', 'sier.us', 'chilcas.ar'), 1, ifelse(site_code=='temple.us' & trt %in% c('Control', 'NPK'), 1, 0)))%>%
  filter(keep==1)%>%
  select(-keep)
write.csv(siteData, 'C:\\Users\\komatsuk\\Dropbox (Smithsonian)\\working groups\\CoRRE\\CoRRE_database\\Data\\OriginalData\\2020 update\\nutnet\\nutnet_siteData_01052021.csv')
