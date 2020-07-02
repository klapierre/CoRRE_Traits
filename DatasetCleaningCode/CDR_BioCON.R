#################
## CDR_BioCON ##
################
setwd("/Users/kaitlinkimmel/Dropbox/CoRRE_database")
# Pull data from LTER site

file <- "https://portal.lternet.edu/nis/dataviewer?packageid=knb-lter-cdr.302.9&entityid=7dfa36f6d8adbc6a669d9501beaa30bf"

df <- read.delim(file, header=TRUE)

# Only using 16 species plots
df <- df[df$CountOfSpecies == 16,]

# Get August sampling only
df$Date <- as.Date(df$Date,"%m/%d/%Y")
df$Month <- as.numeric(format(df$Date, format = "%m"))
df$calendar_year <- as.numeric(format(df$Date, format = "%Y"))
df <- df[df$Month == 8,]
# Create treatment year column 
df$treatmet_year <- df$calendar_year - 1997

# Create treatment column
df$treatment <- paste(df$CO2.Treatment, df$Nitrogen.Treatment, sep = "_")


df <- df[,c(17, 19, 3, 4, 18, 14,15)]
df$site_code <-"CDR"
df$project_name <- "BioCON"

names(df)[c(3,4,6,7)] <- c("plot_id", "block", "genus_species", "biomass")

df <- df[,c(1:5,8,9,6,7)]

write.csv(df, "Data/CleanedData/Sites/Species csv/CDR_BioCON.csv", row.names = FALSE)

### ANPP data ###

ANPP <- aggregate(df$biomass, by = list(calendar_year = df$calendar_year, treatment = df$treatment,  
                                        plot_id = df$plot_id, block = df$block), FUN = sum)
names(ANPP)[5] <- "ANPP"

ANPP$site_code <- "CDR"
ANPP$project_name <- "BioCON"

ANPP <- ANPP[,c(1:4,6,7,5)]

write.csv(ANPP, "Data/CleanedData/Sites/ANPP csv/CDR_BioCON_anpp.csv")
                  