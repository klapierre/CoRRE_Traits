##################
#### KNZ_BGP ####
################
setwd("/Users/kaitlinkimmel/Dropbox/CoRRE_database")

# biomass data
file <- "https://portal.edirepository.org/nis/dataviewer?packageid=knb-lter-knz.57.10&entityid=eaf1b05d4c7578a1fe1efc5173b80953"
df <- read.csv(file, header = TRUE, na.strings = c(NA, ""))


df$CUYRDD[df$CUYRDD == "."] <- 0
df$CUYRDD[which(is.na(df$CUYRDD))] <- 0
df$CUYRDD <- as.numeric(df$CUYRDD)

# change n+p to b to fit metadata
for (i in 1:nrow(df)){
  if(df$NUTRIENT[i] == "n+p"){
    df$NUTRIENT[i] = "b"
  }
}

# combine treatment data into single column
df$treatment <- paste(df$BURN, df$MOW, df$NUTRIENT, sep = "_")
df <- df[df$MOW == "u",]

df <- df[-which(is.na(df$FORBS) & is.na(df$LVGRASS)),]

df$anpp <- df$LVGRASS + df$FORBS + df$CUYRDD

df <- aggregate(df$anpp, by = list(calendar_year = df$RECYEAR, plot_id = df$PLOT, 
                                     treatment = df$treatment), FUN = mean)
df$site_code <- "KNZ"
df$project_name <- "BGP"
df$data_type <- "anpp"
df$treatment_year <- df$calendar_year - 1985
names(df)[4] <- "anpp"
df$anpp <- df$anpp*10 # mulitply by 10 to get g/m2, currently g/0.1 m2

df <- df[,c(1,3,2,7,8,5,6,4)]

write.csv(df, "Data/CleanedData/Sites/ANPP csv/KNZ_BGP_anpp.csv", row.names = FALSE)


file1 <- "https://pasta.lternet.edu/package/data/eml/knb-lter-knz/17/11/410e032a0651ce990c8c497be62c68f7"
df1 <- read.csv(file1, header = TRUE)
file2 <- "http://lter.konza.ksu.edu/file/1607/download"
spdf <- read.csv(file2, header = TRUE)
trtdf <- read.csv("Data/OriginalData/Sites/KNZ_BGP/BGP_treatments.csv")

## Convert cover class to mid-points
# 1 = 0.5, 2 = 3.5, 3 = 15, 4 = 37.5, 5=62.5, 6 = 85, 7 = 97.5

# Taking midpoints of cover classes 
df1$CoverClass[df1$CoverClass == 1] <- 0.5
df1$CoverClass[df1$CoverClass == 2] <- 3.5
df1$CoverClass[df1$CoverClass == 3] <- 15
df1$CoverClass[df1$CoverClass == 4] <- 37.5
df1$CoverClass[df1$CoverClass == 5] <- 62.5
df1$CoverClass[df1$CoverClass == 6] <- 85
df1$CoverClass[df1$CoverClass == 7] <- 97.5

 
df1 <- aggregate(df1$CoverClass, 
                  by = list(calendar_year = df1$RecYear, gen = df1$Ab_genus,
                            spec = df1$Ab_species, plot_id = df1$Plot), FUN = mean)

names(df1)[5] <- "abundance"
df1$site_code <- "KNZ"
df1$project_name <- "BGP"
df1$treatment_year <- df1$calendar_year - 1985

df1 <- merge(df1, spdf, all.x = TRUE)
df1$genus_species <- paste(df1$genus, df1$species, sep = " ")
df1$data_type <- "cover"

# need to add treatments from biomass data to cover data
df2 <- merge(df1, trtdf, by = "plot_id", all.x = TRUE)
df2$treatment <- paste(df2$burned, df2$mowed, df2$nutrient, sep = "_")

df1 <- df2[,c(1,4:8,17,18,22)]

write.csv(df1, "Data/CleanedData/Sites/Species csv/KNZ_BGP.csv", row.names = FALSE)

