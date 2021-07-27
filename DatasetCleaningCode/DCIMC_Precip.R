###################
## DCIMC_GCME2 ####
##################

setwd("~/Dropbox/CoRRE_database")

# library
library(readxl)
library(tidyr)

#Load in data
# Get names from all of the tabs in excel sheet
tab_names <- excel_sheets(path = "Data/OriginalData/Sites/DCIMC_Precip.xlsx")
# import all tabs so that each is a separate element in a list
myfiles <- lapply(tab_names, function(x) read_excel(path = "Data/OriginalData/Sites/DCIMC_Precip.xlsx", sheet = x))


for (i in 1:length(myfiles)){
  myfiles[[i]] <- myfiles[[i]][-1,] # get rid of first row
  names(myfiles[[i]])[c(1,2)] <- c("plot_id", "treatment") #add column names
  #wide to long format
  myfiles[[i]] <- gather(myfiles[[i]], key = "genus_species", value = "abundance",3:length(myfiles[[i]]))
  myfiles[[i]]$abundance <- as.numeric(myfiles[[i]]$abundance)
  # change NA to 0
  myfiles[[i]]$abundance[which(is.na(myfiles[[i]]$abundance))] <- 0
  myfiles[[i]]$calendar_year <- i + 2009
  myfiles[[i]]$treatment_year <- i
}

# combine into one dataframe
dat <- do.call("rbind", myfiles)

# get rid of 0s
dat<- dat[which(dat$abundance >0),]

# add other project info
dat$site_code <- "DCIMC"
dat$project_name <- "Precip"
dat$data_type <- "cover"

write.csv(dat, "Data/CleanedData/Sites/Species csv/DCMIC_Precip.csv", row.names = FALSE)