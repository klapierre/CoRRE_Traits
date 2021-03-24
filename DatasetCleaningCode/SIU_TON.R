################
## SIU_TON ####
###############

setwd("/Users/kaitlinkimmel/Dropbox/CoRRE_database")

#### Cover ####
df <- read.csv("Data/OriginalData/2020 update/Data/SIU_TON/TON_Div.csv")

# Convert from cover classes to cover
# Using only cover measured from cov.1 (cover under 1 m off the ground)
df$cov.1[df$cov.1 == 1] <- 0.5
df$cov.1[df$cov.1 == 2] <- 3.5
df$cov.1[df$cov.1 == 3] <- 15
df$cov.1[df$cov.1 == 4] <- 37.5
df$cov.1[df$cov.1 == 5] <- 62.5
df$cov.1[df$cov.1 == 6] <- 85
df$cov.1[df$cov.1 == 7] <- 97.5

# combine fertilizer and mowing treatments into one column
df$treatment <- paste(df$trt.fert, df$trt.mow, sep = "")
# Change plot to block - need to create a unique plot number for each treatment within a block
names(df)[8] <- "block"
plts <- unique(df[,c("block", "treatment")]) # getting unique block and treatment combos
plts$plot_id <- seq(1, nrow(plts), by = 1) # adding a unique plot_id
df <- merge(df,plts)
df <- df[df$Month == "9"|df$Month == "10"|df$Month == 8,] # filter out late summer/early fall records. 

df$site_code <- "SIU"
df$project_name <- "TON"
df$treatment_year <- df$Year - 1995
df$data_type <- "cover"
df <- df[,c(1,2,5,9,12,15:19)]
names(df)[c(3:5)] <- c("genus_species", "calendar_year", "abundance")
# get rid of block 1 because was destroyed in 2013
df <- df[which(df$block != 1),]
df <- df[which(df$abundance >0),]
### some duplicated rows, get rid of these
df <- df[which(duplicated(df) == FALSE),]

write.csv(df, "Data/CleanedData/Sites/Species csv/SIU_TON.csv", row.names = FALSE)


#### Biomass ####

df1 <- read.csv("Data/OriginalData/2020 update/Data/SIU_TON/TON_Biomass.csv")

df1$treatment <- paste(df1$trt.fert, df1$trt.mow, sep = "") # make treatment column
names(df1)[5] <- "block" #change PL column to block so can merge over plts df from cover data
df1 <- merge(df1, plts)
# Need to change biomass columns from characters to numeric columns
df1$forb <- as.numeric(df1$forb)
df1$grass <- as.numeric(df1$grass)
df1$wood <- as.numeric(df1$wood)
df1$Lespedeza.cuneata <- as.numeric(df1$Lespedeza.cuneata)
df1$Cirsium.vulgare <- as.numeric(df1$Cirsium.vulgare)
# Change NA values to 0
df1[is.na(df1)] <- 0
# get total biomass without litter
df1$biomass <- df1$forb + df1$grass + df1$Lespedeza.cuneata + df1$Cirsium.vulgare + df1$wood # including woody now, can remove if necessary
#need to average over subplots
df1 <- aggregate(df1$biomass, by = list(calendar_year = df1$Year, plot_id = df1$plot_id, block = df1$block, 
                                       treatment = df1$treatment), FUN = mean)
df1$site_code <- "SIU"
df1$project_name <- "TON"
df1$treatment_year <- df1$calendar_year - 1995
df1$data_type <-"biomass"
names(df1)[5] <- "anpp"

write.csv(df1, "Data/CleanedData/Sites/ANPP csv/SIU_TON_anpp.csv", row.names = FALSE)
