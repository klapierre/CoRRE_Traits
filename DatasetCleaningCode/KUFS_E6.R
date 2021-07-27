#################
#### KUFS_E6 ####
################
setwd("~/Dropbox/CoRRE_database")

library(tidyr)

file <- "https://foster.ku.edu/sites/foster.ku.edu/files/files/E1%20Plant%20Species%20Compostion%206%2016.csv"
df <- read.csv (file, header = TRUE)

df <- gather(df, genus_species, abundance, Acalypha.virginica:Zizia.aurea)
df <- df[df$abundance>0,]

df$site_code <- "KUFS"
df$project_name <- "E6"
df$data_type <- "cover"

df <- df[,c(2,4,5,6,8:15)]
df$treatment <- paste(df$N, df$P, df$Seed)

for (i in 1:nrow(df)){
  if(df$treatment[i] == "0 0 0"){
    df$treatment[i] <- "N0P0S0"
  }
  if(df$treatment[i] == "4 0 0"){
    df$treatment[i] <- "N4P0S0"
  }
  if(df$treatment[i] == "8 0 0"){
    df$treatment[i] <- "N8P0S0"
  }
  if(df$treatment[i] == "16 0 0"){
    df$treatment[i] <- "N16P0S0"
  }
  if(df$treatment[i] == "0 8 0"){
    df$treatment[i] <- "N0P8S0"
  }
  if(df$treatment[i] == "4 8 0"){
    df$treatment[i] <- "N4P8S0"
  }
  if(df$treatment[i] == "8 8 0"){
    df$treatment[i] <- "N8P8S0"
  }
  if(df$treatment[i] == "16 8 0"){
    df$treatment[i] <- "N16P8S0"
  }
  if(df$treatment[i] == "0 0 1"){
    df$treatment[i] <- "N0P0S1"
  }
  if(df$treatment[i] == "4 0 1"){
    df$treatment[i] <- "N4P0S1"
  }
  if(df$treatment[i] == "8 0 1"){
    df$treatment[i] <- "N8P0S1"
  }
  if(df$treatment[i] == "16 0 1"){
    df$treatment[i] <- "N16P0S1"
  }
  if(df$treatment[i] == "0 8 1"){
    df$treatment[i] <- "N0P8S1"
  }
  if(df$treatment[i] == "4 8 1"){
    df$treatment[i] <- "N4P8S1"
  }
  if(df$treatment[i] == "8 8 1"){
    df$treatment[i] <- "N8P8S1"
  }
  if(df$treatment[i] == "16 8 1"){
    df$treatment[i] <- "N16P8S1"
  }
}

df <- df[,-c(5:7)]
names(df)[c(1:4)] <- c("calendar_year", "plot_id", "block", "community_type")
df$treatment_year <- df$calendar_year - 2001
write.csv(df, "Data/CleanedData/Sites/Species csv/KUFS_E6.csv", row.names = FALSE)

## ANPP data
file1 <- "https://foster.ku.edu/sites/foster.ku.edu/files/files/E1%20Plant%20Biomass%206%2016.csv"
df1 <- read.csv(file1, header = TRUE)

df1$site_code <- "KUFS"
df1$project_name <- "E6"
df1$data_type <- "biomass"

df1$treatment <- paste(df1$N, df1$P, df1$Seed)

for (i in 1:nrow(df1)){
  if(df1$treatment[i] == "0 0 0"){
    df1$treatment[i] <- "N0P0S0"
  }
  if(df1$treatment[i] == "4 0 0"){
    df1$treatment[i] <- "N4P0S0"
  }
  if(df1$treatment[i] == "8 0 0"){
    df1$treatment[i] <- "N8P0S0"
  }
  if(df1$treatment[i] == "16 0 0"){
    df1$treatment[i] <- "N16P0S0"
  }
  if(df1$treatment[i] == "0 8 0"){
    df1$treatment[i] <- "N0P8S0"
  }
  if(df1$treatment[i] == "4 8 0"){
    df1$treatment[i] <- "N4P8S0"
  }
  if(df1$treatment[i] == "8 8 0"){
    df1$treatment[i] <- "N8P8S0"
  }
  if(df1$treatment[i] == "16 8 0"){
    df1$treatment[i] <- "N16P8S0"
  }
  if(df1$treatment[i] == "0 0 1"){
    df1$treatment[i] <- "N0P0S1"
  }
  if(df1$treatment[i] == "4 0 1"){
    df1$treatment[i] <- "N4P0S1"
  }
  if(df1$treatment[i] == "8 0 1"){
    df1$treatment[i] <- "N8P0S1"
  }
  if(df1$treatment[i] == "16 0 1"){
    df1$treatment[i] <- "N16P0S1"
  }
  if(df1$treatment[i] == "0 8 1"){
    df1$treatment[i] <- "N0P8S1"
  }
  if(df1$treatment[i] == "4 8 1"){
    df1$treatment[i] <- "N4P8S1"
  }
  if(df1$treatment[i] == "8 8 1"){
    df1$treatment[i] <- "N8P8S1"
  }
  if(df1$treatment[i] == "16 8 1"){
    df1$treatment[i] <- "N16P8S1"
  }
}

df1$treatment_year <- df1$Year - 2001
df1 <- df1[,c(2,16,4,5,6,15,17,13,14,11)]
names(df1)[c(1,3:5,10)] <- c("calendar_year", "plot_id", "block", "community_type", "anpp")

write.csv(df1, "Data/CleanedData/Sites/ANPP csv/KUFS_E6_anpp.csv")
  
  
  
  
  