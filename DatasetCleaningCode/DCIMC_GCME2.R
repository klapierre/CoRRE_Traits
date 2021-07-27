###################
## DCIMC_GCME2 ####
##################

setwd("~/Dropbox/CoRRE_database")

# library
library(readxl)
library(tidyr)

#Load in data
# Get names from all of the tabs in excel sheet
tab_names <- excel_sheets(path = "Data/OriginalData/Sites/DCIMC_GCME2.xlsx")
# import all tabs so that each is a separate element in a list
myfiles <- lapply(tab_names, function(x) read_excel(path = "Data/OriginalData/Sites/DCIMC_GCME2.xlsx", sheet = x))
sp <- read.csv("./Data/OriginalData/Sites/DCIMC_GCME/DCIMC_GCME_sp.csv")

for (i in 1:length(myfiles)){
  colnames(myfiles[[i]]) <- myfiles[[i]][1,]
  myfiles[[i]] <- myfiles[[i]][-1,] # get rid of first row
  #wide to long format
  myfiles[[i]] <- gather(myfiles[[i]], key = "SP_ID", value = "abundance",3:length(myfiles[[i]]))
  myfiles[[i]]$abundance <- as.numeric(myfiles[[i]]$abundance)
  # change NA to 0
  myfiles[[i]]$abundance[which(is.na(myfiles[[i]]$abundance))] <- 0
  myfiles[[i]]$calendar_year <- i + 2004
  myfiles[[i]]$treatment_year <- i
}

# combine into one dataframe
dat <- do.call("rbind", myfiles)
# create plot_id
plot_id <- data.frame(Treatment = unique(dat$Treatment))
plot_id$plot_id <- seq(1:nrow(plot_id))
dat<- merge(dat, plot_id)
# get rid of numbers after treatment
dat$treatment <- substr(dat$Treatment,1,nchar(dat$Treatment)-1)
dat <- dat[,-c(1,2)]
# get rid of 0s
dat<- dat[which(dat$abundance >0),]

# add other project info
dat$site_code <- "DCIMC"
dat$project_name <- "GCME2"
dat$data_type <- "cover"

dat <- merge(dat, sp)
dat <- dat[,-1]

write.csv(dat, "Data/CleanedData/Sites/Species csv/DCMIC_GCME2.csv", row.names = FALSE)
