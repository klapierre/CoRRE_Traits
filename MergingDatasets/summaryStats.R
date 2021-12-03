setwd('C:\\Users\\lapie\\Dropbox (Smithsonian)\\working groups\\CoRRE\\CoRRE_database\\Data\\CompiledData')

library(tidyverse)

expInfo <- read.csv('ExperimentInfo.csv')

length <- expInfo%>%
  group_by(site_code, project_name, community_type)%>%
  summarise(length=max(treatment_year))



