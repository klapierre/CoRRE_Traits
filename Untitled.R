#### Online dataset update code ####

setwd("~/Documents/Research/CoRREdatabase/CoRRE/DatasetCleaningCode/")

# working directory and file paths for saving may need to be updated
# if have an error with web address, may need to update in original code. 
# As of July 30, 2021 all the web addresses were working

source("CDR_BioCON.R")
write.csv(biocon.species, "~/Dropbox/CoRRE_database/Data/CleanedData/Sites/Species csv/CDR_BioCON.csv", row.names = FALSE)
write.csv(biocon.ANPP, "~/Dropbox/CoRRE_database/Data/CleanedData/Sites/ANPP csv/CDR_BioCON_anpp.csv", row.names = FALSE)

source("CDR_e001.R")
write.csv(e001.species, "~/Dropbox/CoRRE_database/Data/CleanedData/Sites/Species csv/CDR_e001.csv", row.names = FALSE)
write.csv(biocon.anpp, "~/Dropbox/CoRRE_database/Data/CleanedData/Sites/ANPP csv/CDR_e001_anpp.csv", row.names = FALSE)

source("KNZ_BGP.R")
write.csv(bgp.species, "~/Dropbox/CoRRE_database/Data/CleanedData/Sites/Species csv/KNZ_BGP.csv", row.names = FALSE)
write.csv(bgp.anpp, "~/Dropbox/CoRRE_database/Data/CleanedData/Sites/ANPP csv/KNZ_BGP_anpp.csv", row.names = FALSE)

source("KNZ_IRG.R")
write.csv(irg.species, "~/Dropbox/CoRRE_database/Data/CleanedData/Sites/Species csv/KNZ_BGP.csv", row.names = FALSE)

source("KNZ_pplots.R")
write.csv(pplots.species, "~/Dropbox/CoRRE_database/Data/CleanedData/Sites/Species csv/KNZ_pplots.csv", row.names = FALSE)
write.csv(pplots.anpp, "~/Dropbox/CoRRE_database/Data/CleanedData/Sites/ANPP csv/KNZ_pplots_anpp.csv", row.names = FALSE)


source("KNZ_RHPs.R")
write.csv(rhps.species, "~/Dropbox/CoRRE_database/Data/CleanedData/Sites/Species csv/KNZ_RHPs.csv", row.names = FALSE)
write.csv(rhps.anpp, "~/Dropbox/CoRRE_database/Data/CleanedData/Sites/ANPP csv/KNZ_RHPs_anpp.csv", row.names = FALSE)

source("KUFS_E2.R")
write.csv(E2.species, "~/Dropbox/CoRRE_database/Data/CleanedData/Sites/Species csv/KUFS_E2.csv", row.names = FALSE)
write.csv(E2.anpp, "~/Dropbox/CoRRE_database/Data/CleanedData/Sites/ANPP csv/KUFS_E2_anpp.csv", row.names = FALSE)

source("KUFS_E6.R")
write.csv(E6.species, "~/Dropbox/CoRRE_database/Data/CleanedData/Sites/Species csv/KUFS_E6.csv", row.names = FALSE)
write.csv(E6.anpp, "~/Dropbox/CoRRE_database/Data/CleanedData/Sites/ANPP csv/KUFS_E6_anpp.csv", row.names = FALSE)

source("NWT_snow.R")
write.csv(snow.species, "~/Dropbox/CoRRE_database/Data/CleanedData/Sites/Species csv/NWT_snow.csv", row.names = FALSE)
write.csv(snow.anpp, "~/Dropbox/CoRRE_database/Data/CleanedData/Sites/ANPP csv/NWT_snow_anpp.csv", row.names = FALSE)




