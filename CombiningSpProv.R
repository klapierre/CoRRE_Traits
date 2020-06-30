## Compile all species provedence files into one file
setwd("/Users/kaitlinkimmel/Dropbox/CoRRE_database")


file_names <- dir(path ="/Users/kaitlinkimmel/Dropbox/CoRRE_database/Data/OriginalData/Species Provenance", pattern=".csv")
newdf <- do.call(rbind,lapply(paste("/Users/kaitlinkimmel/Dropbox/CoRRE_database/Data/OriginalData/Species Provenance/",file_names, sep =""), read.csv))
                 
write.csv(newdf, "Data/CleanedData/Traits/SpeciesProvBySite.csv", row.names = FALSE)
