####################
#### SGS_PRECIP ####
###################

setwd("~/Dropbox/CoRRE_database")
setwd('C:\\Users\\komatsuk\\Dropbox (Smithsonian)\\working groups\\CoRRE\\CoRRE_database')

# library
library(readxl)
library(tidyr)

# read in data
dat <- read_excel("Data/OriginalData/Sites/SGS/SGS_Precip.xlsx")
sp.list <- read_excel("Data/OriginalData/Sites/SGS/plant_list-SGS.xls")

dat <- dat[,-53]
# Long to wide format
dat <- gather(dat, key = "sp_code", value = "abundance", 4:52)

#Code in treatments
dat$TREAT[dat$TREAT == 1] <- "reduction"
dat$TREAT[dat$TREAT == 2] <- "control"
dat$TREAT[dat$TREAT == 3] <- "add"

dat$treatment_year <- dat$YEAR - 2007

# irrigation started in 2008
for (i in 1:nrow(dat)){
  if(dat$TREAT[i] == "add"){
    dat$treatment_year[i] <- dat$YEAR[i] - 2008
  }
}

# get rid of 0's
dat <- dat[which(dat$abundance > 0),]

dat$site_code <- "SGS"
dat$project_name <- "Precip"
dat$data_type <- "cover"

names(dat)[c(1:3)] <- c("calendar_year", "plot_id", "treatment")

# Species data provided by Kerry - used 6 letters for sp_code, 
# but does not know exactly how they line up with the USDA Plants database which only uses 4
# Gave me a partial list of species found at SGS site, so am using that to match to the species 
# codes in the dataset
# get column needed
sp.list <- sp.list[,2]
# Add column name
names(sp.list)<- "genus_species"
sp.list <- sp.list[-which(is.na(sp.list$genus_species)),]
# split genus and species into separate columns
temp <- strsplit(sp.list$genus_species, " ")
# add columns to df
sp.list$genus <- sapply(temp, "[[", 1)
sp.list$species <- sapply(temp, "[[", 2)
#pull out first 3 letters of genus
sp.list$genus3 <- substr(sp.list$genus, 1, 3)
# pull out first 3 letters of sp
sp.list$sp3 <- substr(sp.list$species, 1, 3)
# combine genus and species 3 letters to get code
sp.list$sp_code <- paste(sp.list$genus3, sp.list$sp3, sep = "")
# make sp_code all uppercase like in dataframe
sp.list$sp_code <- toupper(sp.list$sp_code)


# get unique species from dataframe
sp.dat <- data.frame(sp_code = unique(dat$sp_code))
sp.match <- merge(sp.dat, sp.list[,c(1,6)], all.x= TRUE)
unmatched <- sp.match[which(is.na(sp.match$genus_species)),]

# unmatched species
# ALITEX = Allium textile
# ARIPUR = ??
# CHR = Chrysothamnus viscidiflorus
# SPURGE = Unknown spurge
# UNK = Unknown

unmatched$genus_species <- c("Allium textile", "Aristida purpurea", "Chrysothamnus viscidiflorus", "Unknown spurge", "Unknown")
sp.list <-sp.list[,c(1,6)]
sp.list <- rbind(sp.list, unmatched)
sp.match <- merge(sp.list, sp.dat, all.y = TRUE)

dat <- merge(dat, sp.match)

dat <- dat[,-1]


#split into irrigation vs drought
irg <- dat%>%
  mutate(project_name='Irg')%>%
  filter(treatment!='reduction', calendar_year>2008)%>%
  mutate(treatment_year=calendar_year-2008)

# write.csv(irg, "Data/CleanedData/Sites/Species csv/SGS_Irg.csv", row.names = FALSE)


#split into irrigation vs drought
drought <- dat%>%
  mutate(project_name='Drought')%>%
  filter(treatment!='add')

# write.csv(drought, "Data/CleanedData/Sites/Species csv/SGS_Drought.csv", row.names = FALSE)


