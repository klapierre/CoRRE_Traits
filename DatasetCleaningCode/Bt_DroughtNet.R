######################
## Bt_DroughtNet #####
#####################

setwd("~/Dropbox/CoRRE_database")
setwd('C:\\Users\\lapie\\Dropbox (Smithsonian)\\working groups\\CoRRE\\CoRRE_database')

# library
library(readxl)
library(tidyverse)

####### Cover ########
temp = list.files(path = "./Data/OriginalData/Sites/Bt/Bt_DroughtNet/")
myfiles = lapply(temp, function(x) read_excel(path = paste("Data/OriginalData/Sites/Bt/Bt_DroughtNet",x, sep = "/"), sheet = "cover"))

#first four years have note_date and plot_replication - do not need this information for data cleaning
for(i in 1:4){
  myfiles[[i]] <- myfiles[[i]][,-c(7,8)]
}
# bind all separate files into one
dat <- do.call("rbind", myfiles)
dat1 <- dat[which(dat$note_cover %in% c("peak_season_species_specific_cover", "2_peak_season_species_specific_cover")),]
dat2 <- dat[which(is.na(dat$note_cover)),]
dat3 <- rbind(dat1, dat2)%>%
  rename(plot_id=plot)%>%
  mutate(treatment=ifelse(plot_id %in% c('NR-SC-1', 'NR-SC-2', 'NR-SC-3', 'NR-SC-4', 'NR-SC-5'), 'control', 'drought'))%>%
  filter(cover>0, cover!='NA')%>%
  mutate(calendar_year=as.numeric(format(date, format = "%Y")))%>%
  mutate(treatment_year=ifelse(calendar_year<2015, 0, (calendar_year - 2015+1)))%>%
  mutate(site_code='Bt', project_name='DroughtNet', community_type=0, data_type='cover')%>%
  rename(abundance=cover, genus_species=taxa)%>%
  select(-date, -site, -note_cover)%>%
  group_by(site_code, project_name, community_type, calendar_year, treatment_year, treatment, plot_id, genus_species, data_type)%>%
  summarise(abundance=max(abundance))%>%
  ungroup()

controlplots <- dat3[dat3$treatment == "control",]
write.csv(controlplots,"Data/OriginalData/Sites/Bt/clean_control_data.csv")
write.csv(dat3, "Data/CleanedData/Sites/Species csv/Bt_DroughtNet.csv", row.names = FALSE)

####### Biomass ########
temp1 = list.files(path = "./Data/OriginalData/Sites/Bt/Bt_DroughtNet/")
myfiles1 = lapply(temp1, function(x) read_excel(path = paste("Data/OriginalData/Sites/Bt/Bt_DroughtNet",x, sep = "/"), sheet = "biomass"))

#first four years have note_date and plot_replication - do not need this information for data cleaning
for(i in 1:4){
  myfiles1[[i]] <- myfiles1[[i]][,-c(8,9)]
}
# pull out last year biomass file
temp2 <- myfiles1[[7]]
# bind all separate files into one
dat <- do.call("rbind", myfiles1[c(1:6)])
dat1 <- dat[which(dat$note_biomass == "peak_season_harvest"),]
dat2 <- dat[which(is.na(dat$note_biomass)),]
dat3 <- rbind(dat1, dat2)%>%
  rename(plot_id=plot)%>%
  mutate(treatment=ifelse(plot_id %in% c('NR-SC-1', 'NR-SC-2', 'NR-SC-3', 'NR-SC-4', 'NR-SC-5'), 'control', 'drought'))%>%
    mutate(calendar_year=as.numeric(format(date, format = "%Y")))%>%
  mutate(treatment_year=ifelse(calendar_year<2015, 0, (calendar_year - 2015+1)))%>%
  mutate(site_code='Bt', project_name='DroughtNet', community_type=0, data_type='cover')%>%
  filter(taxa!='Total')%>%
  group_by(site_code, project_name, community_type, calendar_year, treatment_year, treatment, plot_id)%>%
  summarise(anpp=sum(mass))%>%
  ungroup()

cntplots <- dat3[which(dat3$treatment == "control"),]
write.csv(cntplots,"Data/OriginalData/Sites/Bt/biomass_cntrol.csv", row.names = FALSE)
write.csv(dat3, "Data/CleanedData/Sites/ANPP csv/Bt_DroughtNet_anpp.csv")
