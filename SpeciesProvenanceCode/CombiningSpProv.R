## Compile all species provedence files into one file
setwd("~/Dropbox/CoRRE_database")


file_names <- dir(path ="~/Dropbox/CoRRE_database/Data/OriginalData/Species Provenance", pattern=".csv") # get names all .csv files 
newdf <- do.call(rbind,lapply(paste("~/Dropbox/CoRRE_database/Data/OriginalData/Species Provenance/",file_names, sep =""), read.csv)) #combine all .csv files
newdf <- as.data.frame(apply(newdf, 2, function(x) gsub("^$|^ $", NA, x)))# replace blank cells with NA values           

# Cleaning Native Status column
newdf$Native_status[which(newdf$Native_status %in% c("Exotic", "Non ", "Non-native"))] <- "Non"
newdf$Native_status[which(newdf$Native_status %in% c("Na", "Unk", "n/a", "Unknown"))] <- NA
newdf$Native_status[which(newdf$Native_status %in% c("native", "Native "))] <- "Native"

names(newdf)[c(1,2)] <- c("site_code", "project_name")

write.csv(newdf, "Data/CompiledData/SpeciesProvBySite.csv", row.names = FALSE) # save
