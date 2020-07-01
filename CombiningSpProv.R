## Compile all species provedence files into one file
setwd("/Users/kaitlinkimmel/Dropbox/CoRRE_database")


file_names <- dir(path ="/Users/kaitlinkimmel/Dropbox/CoRRE_database/Data/OriginalData/Species Provenance", pattern=".csv") # get names all .csv files 
newdf <- do.call(rbind,lapply(paste("/Users/kaitlinkimmel/Dropbox/CoRRE_database/Data/OriginalData/Species Provenance/",file_names, sep =""), read.csv)) #combine all .csv files
newdf <- as.data.frame(apply(newdf, 2, function(x) gsub("^$|^ $", NA, x)))# replace blank cells with NA values           
write.csv(newdf, "Data/CleanedData/Traits/SpeciesProvBySite.csv", row.names = FALSE) # save
