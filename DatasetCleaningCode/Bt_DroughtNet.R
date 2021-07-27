######################
## Bt_DroughtNet #####
#####################

setwd("~/Dropbox/CoRRE_database")

# library
library(readxl)
library(tidyr)
library(stringr)

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
dat <- rbind(dat1, dat2)
dat$calendar_year <- as.numeric(format(dat$date, format = "%Y"))
dat$treatment_year <- dat$calendar_year - 2014
dat$treatment_year[dat$treatment_year == -1] <- 0 # collected pre-treatment data 2 years before treatment started

plot_id <- data.frame(plot = unique(dat$plot))
plot_id$plot_id <- seq(1:nrow(plot_id))
dat <- merge(dat, plot_id)
dat <- dat[-which(dat$plot_id %in% c(11:15)),] # these are fertilization treatments, not sure why they are in here...

dat <- dat[-which(dat$cover == 0),]
dat$treatment <-substr(dat$plot,1,2)
dat$treatment[dat$treatment == "CR"] <- "drought"
dat$treatment[dat$treatment == "NR"] <- "control"
dat <- dat[,-c(1,2,5,6)]
dat$site_code <- "Bt"
dat$project_name <- "DroughtNet"
dat$data_type <- "cover"
names(dat)[c(1,2)] <- c("genus_species", "abundance")

controlplots <- dat[dat$treatment == "control",]
write.csv(controlplots,"Data/OriginalData/Sites/Bt/clean_control_data.csv")
write.csv(dat, "Data/CleanedData/Sites/Species csv/Bt_DroughtNet.csv", row.names = FALSE)

####### Biomass ########
temp1 = list.files(path = "./Data/OriginalData/Sites/Bt/Bt_DroughtNet/")
myfiles1 = lapply(temp, function(x) read_excel(path = paste("Data/OriginalData/Sites/Bt/Bt_DroughtNet",x, sep = "/"), sheet = "biomass"))

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
dat <- rbind(dat1, dat2)
dat$calendar_year <- as.numeric(format(dat$date, format = "%Y"))
dat$treatment <-substr(dat$plot,1,2)
dat$treatment[dat$treatment == "CR"] <- "drought"
dat$treatment[dat$treatment == "NR"] <- "control"
dat <- merge(dat, plot_id) #using same plot_ids as from cover
dat <- aggregate(dat$mass, by = list(calendar_year = dat$calendar_year, plot_id = dat$plot_id, treatment = dat$treatment), FUN = sum)
names(dat)[4] <- "anpp"

temp2 <- temp2[-which(is.na(temp2$biomass)),]
temp2$calendar_year <- as.numeric(format(temp2$date, format = "%Y"))
temp2 <- aggregate(temp2$biomass, by = list(plot = temp2$plot, calendar_year = temp2$calendar_year), FUN = sum)
names(temp2)[3]<- "anpp"
temp2 <- merge(temp2, plot_id)
temp2$treatment <-substr(temp2$plot,1,2)
temp2$treatment[temp2$treatment == "CR"] <- "drought"
temp2$treatment[temp2$treatment == "NR"] <- "control"
temp2 <- temp2[,-1]

dat <- rbind(dat, temp2)

dat$treatment_year <- dat$calendar_year - 2014
dat$treatment_year[dat$treatment_year == -1] <- 0 # collected pre-treatment data 2 years before treatment started
dat$site_code <- "Bt"
dat$project_name <- "DroughtNet"

cntplots <- dat[which(dat$treatment == "control"),]
write.csv(cntplots,"Data/OriginalData/Sites/Bt/biomass_cntrol.csv", row.names = FALSE)
write.csv(dat, "Data/CleanedData/Sites/ANPP csv/Bt_DroughtNet_anpp.csv")
