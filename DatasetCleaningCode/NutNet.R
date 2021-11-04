library(tidyverse)

setwd('C:\\Users\\komatsuk\\Dropbox (Smithsonian)\\nutrient network\\NutNet data') #kim's desktop
setwd('C:\\Users\\lapie\\Dropbox (Smithsonian)\\nutrient network\\NutNet data') #kim's laptop

cover <- read.csv('full-cover-18-October-2021.csv')%>%
  mutate(keep=ifelse(site_code %in% c('shps.us', 'yarra.au', 'cdcr.us', 'sier.us', 'veluwe.nl'), 1, ifelse(site_code %in% c('temple.us', 'bayr.de') & trt %in% c('Control', 'NPK'), 1, ifelse(site_code=='cbgb.us' & trt %in% c('N', 'P', 'K', 'NP', 'NK', 'PK', 'NPK'), 1, 0))))%>%
  filter(keep==1)%>%
  select(-keep)
# write.csv(cover, 'C:\\Users\\lapie\\Dropbox (Smithsonian)\\working groups\\CoRRE\\CoRRE_database\\Data\\OriginalData\\Sites\\NutNet\\nutnet_cover_11042021.csv')


anpp <- read.csv('full-biomass-18-October-2021.csv')%>%
  mutate(keep=ifelse(site_code %in% c('shps.us', 'yarra.au', 'cdcr.us', 'sier.us', 'veluwe.nl'), 1, ifelse(site_code %in% c('temple.us', 'bayr.de') & trt %in% c('Control', 'NPK'), 1, ifelse(site_code=='cbgb.us' & trt %in% c('N', 'P', 'K', 'NP', 'NK', 'PK', 'NPK'), 1, 0))))%>%
  filter(keep==1)%>%
  select(-keep)
# write.csv(anpp, 'C:\\Users\\lapie\\Dropbox (Smithsonian)\\working groups\\CoRRE\\CoRRE_database\\Data\\OriginalData\\Sites\\NutNet\\nutnet_anpp_11042021.csv')


siteData <- read.csv('comb-by-plot-clim-soil-18-October-2021.csv')%>%
  mutate(keep=ifelse(site_code %in% c('shps.us', 'yarra.au', 'cdcr.us', 'sier.us', 'veluwe.nl'), 1, ifelse(site_code %in% c('temple.us', 'bayr.de') & trt %in% c('Control', 'NPK'), 1, ifelse(site_code=='cbgb.us' & trt %in% c('N', 'P', 'K', 'NP', 'NK', 'PK', 'NPK'), 1, 0))))%>%
  filter(keep==1)%>%
  select(-keep)
# write.csv(siteData, 'C:\\Users\\lapie\\Dropbox (Smithsonian)\\working groups\\CoRRE\\CoRRE_database\\Data\\OriginalData\\Sites\\NutNet\\nutnet_siteData_11042021.csv')
