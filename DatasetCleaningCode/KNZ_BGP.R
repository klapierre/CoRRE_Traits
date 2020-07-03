##################
#### KNZ_BGP ####
################
setwd("/Users/kaitlinkimmel/Dropbox/CoRRE_database")

file <- "https://portal.edirepository.org/nis/dataviewer?packageid=knb-lter-knz.57.10&entityid=eaf1b05d4c7578a1fe1efc5173b80953"
df <- read.csv(file, header = TRUE)


for (i in 1:nrow(df)){
  if(df$NUTRIENT[i] == "n+p"){
    df$NUTRIENT[i] = "b"
  }
}

df$treatment <- paste(df$BURN, df$MOW, df$NUTRIENT, sep = "_")

df <- df[df$MOW == "u",]

df$anpp <- df$LVGRASS + df$FORBS

df <- aggregate(df$anpp, by = list(calendar_year = df$RECYEAR, plot_id = df$PLOT, 
                                     treatment = df$treatment), FUN = mean)
df$site_code <- "KNZ"
df$project_name <- "BGP"
df$data_type <- "anpp"
df$treatment_year <- df$calendar_year - 1985
names(df)[4] <- "anpp"
df$anpp <- df$anpp*10 # mulitply by 10 to get g/m2, currently g/0.1 m2

df <- df[,c(1,3,2,7,8,5,6,4)]

write.csv(df, "Data/CleanedData/Sites/ANPP csv/KNZ_BGP_anpp.csv")

