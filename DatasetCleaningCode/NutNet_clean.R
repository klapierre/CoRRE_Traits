######################
## NutNet Batch ####
####################

setwd("/Users/kaitlinkimmel/Dropbox/CoRRE_database")

# libraries
library(Hmisc)

## data
dat <- read.csv("Data/OriginalData/2020 update/Data/NutNet/nutnet_cover_01272021.csv", row.names = 1)
bio_dat <- read.csv("Data/OriginalData/2020 update/Data/NutNet/nutnet_anpp_012752021.csv", row.names = 1)
# sites included in this dataset:
# "cdcr.us", "cbgb.us", "lake.us", "lancaster.uk", "chilcas.ar" , "potrok.ar", 
# "shps.us","sier.us", "temple.us", "veluwe.nl",  "yarra.au"


# fix names & get rid of unnecessary columns
dat <- dat[,-c(2,9,12:17)] #check with Kim on subplot column
names(dat) <- c("calendar_year", "site_code", "block", "plot_id", "subplot", "treatment_year",
                "treatment", "genus_species", "live", "abundance")

# get live abundance
dat <- dat[which(dat$live == 1),]
dat <- dat[,-9]# get rid of "live" column

# Species names to first letter capital only
dat$genus_species <- tolower(dat$genus_species)
dat$genus_species <- capitalize(dat$genus_species)

# add in other information
dat$project_name <- "NutNet"
dat$data_type <- "cover"

write.csv(dat, "Data/CleanedData/Sites/Species csv/NutNet.csv", row.names = FALSE)

# get live biomass only
bio_dat <- bio_dat[which(bio_dat$live ==1),]
# sum up categories to get total biomass

bio_dat <- aggregate(bio_dat$mass, by = list(calendar_year = bio_dat$year, treatment_year = bio_dat$year_trt,
                                             treatment = bio_dat$trt, site_code = bio_dat$site_code, block = bio_dat$block,
                                             plot_id = bio_dat$plot, subplot = bio_dat$subplot), FUN = sum)
names(bio_dat)[8] <- "anpp"
# add in other information
bio_dat$project_name <- "NutNet"
bio_dat$data_type <- "biomass"

write.csv(bio_dat, "Data/CleanedData/Sites/ANPP csv/NutNet_anpp.csv", row.names = FALSE)


