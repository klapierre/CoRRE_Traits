####################
## HAYS_PRECIP ####
###################

setwd("/Users/kaitlinkimmel/Dropbox/CoRRE_database")

# library
library(readxl)
library(tidyr)

# read in data

dat <- read_excel("Data/OriginalData/2020 update/Data/HAYS/HAYS_Precip.xlsx")
sp.list <- read_excel("Data/OriginalData/2020 update/Data/HAYS/HAYS_splist.xlsx",col_names = FALSE)

# get rid of last 9 columns of data - unknown sp and summaries by functinal group
dat <- dat[,c(1:64)]
# Long to wide format
dat <- gather(dat, key = "sp_code", value = "abundance", 6:length(dat))

# species composition should be sum of 10 replicated
dat <- aggregate(dat$abundance, by = list(calendar_year = dat$YEAR, treatment = dat$TREAT, 
                                          plot_id = dat$PLOT, sp_code = dat$sp_code, 
                                          block = dat$Block),FUN = sum)

names(dat)[6] <- "abundance"

#code in treatments

dat$treatment[dat$treatment ==1] <- "reduction"
dat$treatment[dat$treatment ==2] <- "control"
dat$treatment[dat$treatment ==3] <- "add"

# get rid of 0s
dat <- dat[which(dat$abundance > 0),]

# Add in other columns
dat$site_code <- "HAYS"
dat$project_name <- "Precip"
dat$treatment_year <- dat$calendar_year - 2007
dat$data_type <- "cover"


# Species data provided by Kerry - used 6 letters for sp_code, 
# but does not know exactly how they line up with the USDA Plants database which only uses 4
# Gave me a partial list of species found at HAYS site, so am using that to match to the species 
# codes in the dataset

# Add column name
names(sp.list)<- "genus_species"
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

# Guesses for unmatched species- took 4 letter code(1,2,4,5) and put into USDA Plants, 
# checked to see what would fix 6-letter code, made sure range was over HAYS

#ARGMER = Argythamnia mercurialina
#CACT = Cactuses (lumped) - Kerry provided this info
#CASSES = Castilleja sessiliflora 
#CROCAP = Croton capitatus
#HELMAX = Helianthus maximiliani
#HYBVER = Hybanthus verticillatus
#HYMSCAB = Hymenopappus scabiosaeus
#HYMSCAP = Tetraneuris scaposa
#MELOFI = Melilotus officinalis
#OLIRIG = Oligoneuron rigidum
#OXYLAM = Oxytropis lambertii
#SALIBE = Salsola iberica
#SYMOBL = Symphyotrichum oblongifolium

# add to sp.list
unmatched <- unmatched[,c(1,2)]
unmatched$genus_species <- c("Argythamnia mercurialina", "Cacti", "Castilleja sessiliflora", "Croton capitatus",
 "Helianthus maximiliani", "Hybanthus verticillatus", "Hymenopappus scabiosaeus", "Tetraneuris scaposa", "Melilotus officinalis",
 "Oligoneuron rigidum","Oxytropis lambertii", "Salsola iberica", "Symphyotrichum oblongifolium")
sp.list <-sp.list[,c(1,6)]
sp.list <- rbind(sp.list, unmatched)
sp.match <- merge(sp.list, sp.dat, all.y = TRUE) # everything matches now

dat <- merge(dat, sp.match)
dat <- dat[,-1]

write.csv(dat, "Data/CleanedData/Sites/Species csv/HAYS_Precip.csv", row.names = FALSE)
