##################
#### KNZ_IRG ####
################
setwd("~/Dropbox/CoRRE_database")

file <- "https://pasta.lternet.edu/package/data/eml/knb-lter-knz/72/13/2798ee2d63b042202cdb97fa25fe3a30"
df <- read.csv(file, header = TRUE)
sp_list <- read.csv("Data/OriginalData/Sites/knz/sp_list.csv")
sp_list <- sp_list[,c(1,4,5)]
names(sp_list)[1] <- "Spcode"

# convert cover classes to midpoints
df$Cover[df$Cover == 1] <- 0.5
df$Cover[df$Cover == 2] <- 3
df$Cover[df$Cover == 3] <- 15
df$Cover[df$Cover == 4] <- 37.5
df$Cover[df$Cover == 5] <- 62.5
df$Cover[df$Cover == 6] <- 85
df$Cover[df$Cover == 7] <- 97.5

df <- merge(df, sp_list)
df$genus_species <- paste(df$genus, df$species, sep = ".")
df <- df[,-c(1,2,3,5,6,10,11,13:16)]

names(df)[c(1:5)] <- c("calendar_year", "treatment", "community", "plot_id", "abundance")
df$site_code <- "KNZ"
df$project_name <- "IRG"
df$data_type <- "cover"
df$treatment_year <- df$calendar_year - 1990
df <- df[,c(1,2,4,3,9,10,7,8,6,5)]
write.csv(df, "Data/CleanedData/Sites/Species csv/KNZ_IRG.csv", row.names = FALSE)

file1 <- "https://pasta.lternet.edu/package/data/eml/knb-lter-knz/72/13/d7d500227665f76533332ebade88deeb"
df1 <- read.csv(file1, header = TRUE)
