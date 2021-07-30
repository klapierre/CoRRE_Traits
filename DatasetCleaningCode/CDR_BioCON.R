#################
## CDR_BioCON ##
################
library(stringr)

# Pull data from LTER site
# biomass data
file <- "https://portal.edirepository.org/nis/dataviewer?packageid=knb-lter-cdr.302.12&entityid=7dfa36f6d8adbc6a669d9501beaa30bf"
df <- read.delim(file, header=TRUE)

# cover data
file1 <- "https://pasta.lternet.edu/package/data/eml/knb-lter-cdr/301/8/c41fc5de0beadc305668d5d2a4d40b7b"
df1 <- read.delim(file1, header = TRUE)

# get 16 planted species
planted <- unique(df1$Monospecies)
planted <- planted[c(2:17)]

# Only using 16 species plots
df <- df[df$CountOfSpecies == 16,]

# Get August sampling only
df$Date <- as.Date(df$Date,"%m/%d/%Y")
df$Month <- as.numeric(format(df$Date, format = "%m"))
df$calendar_year <- as.numeric(format(df$Date, format = "%Y"))
df <- df[df$Month == 8,]
# Create treatment year column 
df$treatment_year <- df$calendar_year - 1997

# Correct CO2 treatments
df$CO2.Treatment[df$CO2.Treatment == "Cenriched "]<- "Cenrich"
# Create treatment column
df$treatment <- paste(df$CO2.Treatment, df$Nitrogen.Treatment, sep = "_")


df <- df[,c(17, 19, 3, 4, 18, 14,15)]
df$site_code <-"CDR"
df$project_name <- "BioCON"
df$data_type <- "biomass"

names(df)[c(3,4,6,7)] <- c("plot_id", "block", "genus_species", "abundance")

df <- df[,c(1:4,10,5,8,9,6,7)]

## Cover data ####
# get 16 species plots sampled in August
df1 <- df1[df1$CountOfSpecies == 16 & df1$Season == "August", ]
# Create treatment column
df1$treatment <- paste(df1$CO2.Treatment, df1$Nitrogen.Treatment, sep = "_")

df1$treatment_year <- df1$Year - 1997
df1$site_code <- "CDR"
df1$project_name <- "BioCON"
df1$data_type <- "cover"
df1 <- df1[,c(3,17, 4, 5,21, 18, 19, 20, 15, 16)]
names(df1)[c(1,3,4,9,10)] <- c("calendar_year", "plot_id", "block", "genus_species", "abundance")

df2 <- rbind(df1, df)

# clean species names to get only 16 planted species
df2$genus_species <-str_trim(df2$genus_species, side = "right")
df2$genus_species <- gsub("poa pratensis", "Poa pratensis", df2$genus_species)
df2$genus_species <- gsub("schizachyrium scoparium", "Schizachyrium scoparium", df2$genus_species)
df2$genus_species <- gsub("amorpha canescens", "Amorpha canescens", df2$genus_species)
df2$genus_species <- gsub("bromus inermis", "Bromus inermis", df2$genus_species)
df2$genus_species <- gsub("poa compressa", "Poa compressa", df2$genus_species)

biocon.species <- df2[df2$genus_species %in% planted,]

#write.csv(df2, "Data/CleanedData/Sites/Species csv/CDR_BioCON.csv", row.names = FALSE)

### ANPP data ###

ANPP <- aggregate(df$abundance, by = list(calendar_year = df$calendar_year, treatment = df$treatment,  
                                        plot_id = df$plot_id, block = df$block), FUN = sum)
names(ANPP)[5] <- "anpp"

ANPP$site_code <- "CDR"
ANPP$project_name <- "BioCON"
ANPP$data_type <- "biomass"
ANPP$treatment_year <- ANPP$calendar_year -1997

biocon.ANPP <- ANPP[,c(1:4,8,6,7,5,9)]

#write.csv(ANPP, "Data/CleanedData/Sites/ANPP csv/CDR_BioCON_anpp.csv", row.names = FALSE)
                  