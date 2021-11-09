###################
## DL_GCME ####
##################

setwd("~/Dropbox/CoRRE_database")
setwd('C:\\Users\\lapie\\Dropbox (Smithsonian)\\working groups\\CoRRE\\CoRRE_database')

# library
library(readxl)
library(tidyr)

temp = list.files(path = "./Data/OriginalData/Sites/DCIMC_GCME")
temp = temp[c(1:16)]
myfiles = lapply(paste("./Data/OriginalData/Sites/DCIMC_GCME",temp, sep ="/"), read_excel)

#pull out sp data from community data
sp <- read.csv("./Data/OriginalData/Sites/DCIMC_GCME/DCIMC_GCME_sp.csv")%>%
  rename(SP_ID=ï..SP_ID)

for (i in 1:length(myfiles)){
  colnames(myfiles[[i]]) <- myfiles[[i]][1,] # make first row column names
  myfiles[[i]] <- myfiles[[i]][-1,]# get rid of first row
  #wide to long format
  myfiles[[i]] <- gather(myfiles[[i]], key = "SP_ID", value = "abundance",3:length(myfiles[[i]]))
  myfiles[[i]]$abundance <- as.numeric(myfiles[[i]]$abundance)
  # get rid of letters in treatment so can average over two samples per plot
  if(i != 14){ # 2018 only one sample collected per plot
    myfiles[[i]]$Treatment <- substr(myfiles[[i]]$Treatment,1,nchar(myfiles[[i]]$Treatment)-1)
  }
  #change NA values to 0
  myfiles[[i]]$abundance[which(is.na(myfiles[[i]]$abundance))] <- 0
  # average over 2 samples per plot
  myfiles[[i]] <- aggregate(myfiles[[i]]$abundance, 
            by = list(treatment = myfiles[[i]]$Treatment, SP_ID = myfiles[[i]]$SP_ID),
            FUN = mean)
  names(myfiles[[i]])[3] <- "abundance"
  myfiles[[i]]$calendar_year <- i + 2004
  myfiles[[i]]$treatment_year <- i
}

# combine into one dataframe
dat <- do.call("rbind", myfiles)
# create plot_id
plot_id <- data.frame(treatment = unique(dat$treatment))
plot_id$plot_id <- seq(1:nrow(plot_id))
dat<- merge(dat, plot_id)
# get rid of numbers after treatment
dat$treatment <- substr(dat$treatment,1,nchar(dat$treatment)-1)
# get rid of 0s
dat<- dat[which(dat$abundance >0),]

# add other project info
dat$site_code <- "DL"
dat$project_name <- "GCME"
dat$data_type <- "cover"

# merge with species names
dat2 <- merge(dat, sp)
dat2 <- dat2[,-c(1,11:13)]

write.csv(dat2, "Data/CleanedData/Sites/Species csv/DL_GCME.csv", row.names = FALSE)
