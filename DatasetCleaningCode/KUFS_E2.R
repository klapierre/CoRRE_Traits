##################
#### KUFS_E2 ####
#################

# New site with 2020 update!

setwd("/Users/kaitlinkimmel/Dropbox/CoRRE_database")

###############
#### ANPP ####
#############

file <- "https://foster.ku.edu/sites/foster.ku.edu/files/files/E2%20Plant%20Biomass%206%2016.csv"

df <- read.csv(file, header = TRUE)


# Create Combined treatment column
df$treatment <- paste(df$Fert, df$Seed, df$Hay, sep = "_")

for (i in 1:nrow(df)){
  if(df$treatment[i] == "0_0_0"){
    df$treatment[i] <- "N0S0H0"
  }
  if(df$treatment[i] == "0_0_1"){
    df$treatment[i] <- "N0S0H1"
  }
  if(df$treatment[i] == "0_1_0"){
    df$treatment[i] <- "N0S1H0"
  }
  if(df$treatment[i] == "1_0_0"){
    df$treatment[i] <- "N1S0H0"
  }
  if(df$treatment[i] == "0_1_1"){
    df$treatment[i] <- "N0S1H1"
  }
  if(df$treatment[i] == "1_0_1"){
    df$treatment[i] <- "N1S0H1"
  }
  if(df$treatment[i] == "1_1_0"){
    df$treatment[i] <- "N1S1H0"
  }
  if(df$treatment[i] == "1_1_1"){
    df$treatment[i] <- "N1S1H1"
  }
}


# Add other experiment details
df$site_code <- "KUFS"
df$project_name <- "E2"
df$data_type <- "biomass"
df$treatment_year <- df$Year - 2000

# order columns and get rid of unnecessary ones
df <- df[,c(2,12,5,4,15,16,13,14,10)]
# rename
names(df)[c(1,3,4,9)] <- c("calendar_year", "plot_id", "block", "anpp")
# save
write.csv(df, "Data/CleanedData/Sites/ANPP csv/KUFS_E2_anpp.csv", row.names = FALSE)

##############################
#### Species composition ####
#############################

library(tidyr)

#online location
file1 <- "https://foster.ku.edu/sites/foster.ku.edu/files/files/E2%20Plant%20Species%20Composition%206%2016.csv"
# read in file
df1 <- read.csv(file1, header= TRUE)

# put data in long format
df1 <- gather(df1, genus_species, abundance, 10:length(df1))
df1 <- df1[df1$abundance>0,]

# Create Combined treatment column
df1$treatment <- paste(df1$Fert, df1$Seed, df1$Hay, sep = "_")

for (i in 1:nrow(df1)){
  if(df1$treatment[i] == "0_0_0"){
    df1$treatment[i] <- "N0S0H0"
  }
  if(df1$treatment[i] == "0_0_1"){
    df1$treatment[i] <- "N0S0H1"
  }
  if(df1$treatment[i] == "0_1_0"){
    df1$treatment[i] <- "N0S1H0"
  }
  if(df1$treatment[i] == "1_0_0"){
    df1$treatment[i] <- "N1S0H0"
  }
  if(df1$treatment[i] == "0_1_1"){
    df1$treatment[i] <- "N0S1H1"
  }
  if(df1$treatment[i] == "1_0_1"){
    df1$treatment[i] <- "N1S0H1"
  }
  if(df1$treatment[i] == "1_1_0"){
    df1$treatment[i] <- "N1S1H0"
  }
  if(df1$treatment[i] == "1_1_1"){
    df1$treatment[i] <- "N1S1H1"
  }
}

#add in project details
df1$site_code <- "KUFS"
df1$project_name <- "E2"
df1$data_type <- "biomass"
df1$treatment_year <- df1$Year - 2000

#order and get rid of columns
df1 <- df1[,c(2,4,6,10,11:16)]
#rename
names(df1)[c(1,2,3)] <- c("calendar_year", "block", "plot_id")

#save
write.csv(df1, "Data/CleanedData/Sites/Species csv/KUFS_E2.csv", row.names = FALSE)
