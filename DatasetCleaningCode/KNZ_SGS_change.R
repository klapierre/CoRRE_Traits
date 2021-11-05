setwd("~/Dropbox/CoRRE_database")
setwd('C:\\Users\\lapie\\Dropbox (Smithsonian)\\working groups\\CoRRE\\CoRRE_database\\Data')

library(tidyverse)

cover <- read.csv("OriginalData\\Sites\\KNZ_SGS_change\\ChANGE_SGSandKNZ_sppcomp_2020 update_for Meghan.csv")%>%
  rename(site_code=site, project_name=experiment, plot_id=plot, calendar_year=year, genus_species=species, abundance=cover, treatment=nitrogen)%>%
  mutate(treatment_year=calendar_year-min(calendar_year))%>%
  filter(abundance>0)

# write.csv(cover,"CleanedData/Sites/Species csv/KNZ_SGS_change.csv", row.names=F)


anpp <- read.csv("OriginalData\\Sites\\KNZ_SGS_change\\ChANGE_SGSandKNZ_ANPP_2019 update_for Meghan.csv")%>%
  rename(site_code=site, project_name=experiment, plot_id=plot, calendar_year=year, treatment=nitrogen)%>%
  mutate(treatment_year=calendar_year-min(calendar_year))

# write.csv(anpp, "CleanedData/Sites/ANPP csv/KNZ_SGS_change_anpp.csv", row.names=F)
