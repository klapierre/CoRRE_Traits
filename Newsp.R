# species list updates

#libraries
library(dplyr)
library(Taxonstand)
library(plyr)
library(dplyr)
library(stringr)

setwd("/Users/kaitlinkimmel/Dropbox/CoRRE_database")

# import all datasets new in 2020
WAG <- read.csv("Data/CleanedData/Sites/Species csv/WAG_Shet.csv")
WAG <- data.frame(genus_species = unique(as.character(WAG$genus_species)))
SORBAS <- read.csv("Data/CleanedData/Sites/Species csv/SORBAS_CLIMARID.csv")
SORBAS <- data.frame(genus_species = unique(as.character(SORBAS$genus_species)))
SIU <- read.csv("Data/CleanedData/Sites/Species csv/SIU_TON.csv")
SIU <- data.frame(genus_species = unique(as.character(SIU$genus_species)))
SGS <- read.csv("Data/CleanedData/Sites/Species csv/SGS_Precip.csv")
SGS <- data.frame(genus_species = unique(as.character(SGS$genus_species)))
NutNet <- read.csv("Data/CleanedData/Sites/Species csv/NutNet.csv")
NutNet <- data.frame(genus_species = unique(as.character(NutNet$genus_species)))
KUFS<- read.csv("Data/CleanedData/Sites/Species csv/KUFS_E6.csv")
KUFS <- data.frame(genus_species = unique(as.character(KUFS$genus_species)))
HAYS <- read.csv("Data/CleanedData/Sites/Species csv/HAYS_Precip.csv")
HAYS <- data.frame(genus_species = unique(as.character(HAYS$genus_species)))
Hayoka <- read.csv("Data/CleanedData/Sites/Species csv/Hayoka_WarmNit.csv")
Hayoka<- data.frame(genus_species = unique(as.character(Hayoka$genus_species)))
Glen <- read.csv("Data/CleanedData/Sites/Species csv/Glen_Fert.csv")
Glen<- data.frame(genus_species = unique(as.character(Glen$genus_species)))
DCIMC1 <- read.csv("Data/CleanedData/Sites/Species csv/DCMIC_GCME.csv")
DCIMC1<- data.frame(genus_species = unique(as.character(DCIMC1$genus_species)))
DCIMC2 <- read.csv("Data/CleanedData/Sites/Species csv/DCMIC_GCME2.csv")
DCIMC2 <- data.frame(genus_species = unique(as.character(DCIMC2$genus_species)))
DCIMC3 <- read.csv("Data/CleanedData/Sites/Species csv/DCMIC_Precip.csv")
DCIMC3 <- data.frame(genus_species = unique(as.character(DCIMC3$genus_species)))
AZI <- read.csv("Data/CleanedData/Sites/Species csv/AZI_EELplot.csv")
AZI <- data.frame(genus_species = unique(as.character(AZI$genus_species)))
ANR <- read.csv("Data/CleanedData/Sites/Species csv/ANR_Fert1.csv")
ANR <- data.frame(genus_species = unique(as.character(ANR$genus_species)))
ANR2 <- read.csv("Data/CleanedData/Sites/Species csv/ANR_Fert2.csv")
ANR2 <- data.frame(genus_species = unique(as.character(ANR2$genus_species)))
KUFS$genus_species <- gsub("[.]", " ", KUFS$genus_species)

dat  <-rbind(WAG, SORBAS, SIU, SGS, NutNet, KUFS, HAYS, Hayoka, Glen, DCIMC1, DCIMC2, DCIMC3, AZI, ANR, ANR2)
dat <- dat %>% mutate(genus_species = trimws(genus_species, 'both')) %>%
  mutate(genus_species = gsub("\\s\\s"," ",genus_species, perl = TRUE)) %>%
  mutate(genus_species = gsub("\\s\\s"," ",genus_species, perl = TRUE)) %>%
  mutate(genus_species = gsub("[[:space:]]",".",genus_species)) %>%
  mutate(space_check = str_count(genus_species,"\\.\\."))

# This has a length of 859 unique species
dat = data.frame(genus_species = unique(dat$genus_species))
dat$genus_species <- tolower(dat$genus_species)

#load in old species data
oldsp <- read.csv("Data/CompiledData/CoRRE_TRY_species_list.csv", row.names = 1)
oldspfull <- rbind(data.frame(genus_species = oldsp$genus_species), data.frame(genus_species = oldsp$species_matched))
oldspfull$genus_species <- tolower(oldspfull$genus_species)
oldspfull$genus_species <- gsub(" ", ".", oldspfull$genus_species)
oldspfull<-data.frame(genus_species = unique(oldspfull$genus_species))
oldspfull$oldsp <- 1

mergedsp <-merge(dat, oldspfull, all.x = TRUE)

newsp <- mergedsp[which(is.na(mergedsp$oldsp)),]

newsp1 <- newsp[!grepl("species", newsp$genus_species),]
newsp2 <- newsp1[!grepl(".sp.",fixed = TRUE, newsp1$genus_species),]
newsp3 <- newsp2[!grepl("unknown",newsp2$genus_species),]
newsp4 <- newsp3[grepl("[.]", newsp3$genus_species),]
ssp <- newsp4[grepl("ssp", newsp4$genus_species),]
spp <- newsp4[grepl("spp", newsp4$genus_species),]
var <- newsp4[grepl("var", newsp4$genus_species),]




