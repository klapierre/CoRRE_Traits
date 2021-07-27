########################
### Hayoka_WarmNit ####
######################

setwd("~/Dropbox/CoRRE_database")

# library
library(readxl)
library(tidyr)

# data
dat <- read_excel("Data/OriginalData/Sites/Hayoka_WarmNit_data.xlsx")

# get rid of column 7
dat <- dat[,-7]
# Update column names

names(dat) <- c("genus_species", "treatment", "2013_pin", "2104_pin", "2015_pin", "2016_pin",
                        "2013_cover", "2014_cover", "2015_cover", "2016_cover")
# Get rid of first row
dat <- dat[-1,]

# going to use cover instead of pin hits
dat <- dat[,-c(3:6)]
# add in plot_id
dat$plot_id <- rep(c(1:24), 10)
# wide to long
dat <- gather(dat, key= calendar_year, value = abundance, 3:6)

# Get rid of _cover after year
dat$calendar_year <- as.numeric(substr(dat$calendar_year,1,nchar(dat$calendar_year)-6))

# get rid of species with 0 abundance
dat <- dat[dat$abundance >0, ]

# add in other information

dat$site_code <- "Hayoka"
dat$project_name <- "WarmNit"
dat$data_type <- "cover"
dat$treatment_year <- dat$calendar_year-2012

write.csv(dat, "Data/CleanedData/Sites/Species csv/Hayoka_WarmNit.csv", row.names = FALSE)
