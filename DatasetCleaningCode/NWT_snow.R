###################
#### NWT_snow ####
#################
setwd("/Users/kaitlinkimmel/Dropbox/CoRRE_database")

file <- "https://pasta.lternet.edu/package/data/eml/knb-lter-nwt/13/1/4eb00e2fd65329c4463732974907061f"

df <- read.csv(file, header= TRUE)


df$site_code <- "NWT"
df$project_name <- "snow"
df$data_type <- "hits"
df$treatment_year <- df$year-2006

df <- df[,c(3,7,6,5,18,19,16,17,13,15)]
names(df)[c(1,2,3,4,9,10)] <- c("calendar_year", "treatment", "plot_id", "block", "genus_species", "abundance")

write.csv(df, "Data/CleanedData/Sites/Species csv/NWT_snow.csv", row.names = FALSE)

file1 <- "https://pasta.lternet.edu/package/data/eml/knb-lter-nwt/13/2/9a2e3f9fc5a38f5831065088ee9a75bf"
df1 <- read.csv(file1, header = TRUE)

df1$project_name <- "snow"
df1$treatment_year <- df1$year - 2006
df1$data_type <- "biomass"

df1 <- df1[df1$spp == "live", c(3,7,6,5,16,15,1,14,12)]
names(df1)[c(1:4,7,9)] <- c("calendar_year", "treatment", "plot_id", "block", "site_code", "anpp")

write.csv(df1, "Data/CleanedData/Sites/ANPP csv/NWT_snow_anpp.csv", row.names = FALSE)           
           