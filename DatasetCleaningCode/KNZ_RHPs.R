###################
#### KNZ_RHPs ####
##################
library(tidyr)
setwd("/Users/kaitlinkimmel/Dropbox/CoRRE_database")

file <- "https://portal.lternet.edu/nis/dataviewer?packageid=knb-lter-knz.103.2&entityid=366e428fea83641acf1926ef18adc72e"
df <- read.csv(file, header = TRUE)
species <- read.csv("Data/OriginalData/Sites/KNZ_RHPs/Species_list.csv", header = FALSE)
species <- as.data.frame(t(species))
names(species) <- c("genus_species", "genus_species_code")

# columns imported as characters instead of numeric
for(i in 12:99){
  df[,i] <- as.numeric(df[,i])
}

# get rid of control only plots
df <- df[df$WPTRT == "maxhet",]

# species here are the cover and separate column for ANPP
# Each plot has 12 subplots, 2 reps of each treatment type. Get average of two subplots for each plot
df1 <- aggregate(df[,c(12:99)], by = list(calendar_year = df$RecYear,treatment = df$TRTCOMB,  plot = df$PLOT, block = df$BLOCK), FUN = mean, na.action = na.omit)
df1$treatment_year <- df1$calendar_year - 1997
df1$site_code <- "KNZ"
df1$project_name <- "RPHs"
df1$trt_plots <- paste(df1$treatment, df1$plot, sep = "")


# Need unique plot_id values, each treatment plot value will get a unique plot_id
trt_plots <- as.data.frame(unique(df1$trt_plots))
trt_plots <- cbind(seq(1:24), trt_plots)
names(trt_plots)<- c("plot_id", "trt_plots")
df1 <- merge(df1, trt_plots)

# coverdata
df2 <- df1[,c(2,3,5, 10:97)]
df2[is.na(df2)] <- 0
df2 <- gather(df2, key = "genus_species_code", value = "abundance", 4:87)
df2 <- merge(species, df2)
df2$data_type <- "cover"
df2 <- df2[,c(3,4,9,5,11,6,7,8,2,10)]

write.csv(df2, "Data/CleanedData/Sites/Species csv/KNZ_RHPs.csv", row.names = FALSE)

# ANPP data
df3 <- df1[-which(is.na(df1$ANPP)),c(2:6,94:97)] 
df3$data_type <- "biomass"

df3 <- df3[,c(1,2,9,4,10,6,7,8,5)]
names(df3)[9] <- "anpp"

write.csv(df3, "Data/CleanedData/Sites/ANPP csv/KNZ_RHPs_anpp.csv", row.names = FALSE)
