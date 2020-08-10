#################
####DL_NSFC ####
################
library(tidyr)

setwd("/Users/kaitlinkimmel/Dropbox/CoRRE_database")
### biomass data from 2013-2016
df <- read.csv("Data/OriginalData/Sites/DL_NSFC/DL species biomass-3-16.csv")

# imported a lot of empty columns and rows for some reason...
df <- df[,c(1:58)]
df <- df[-which(is.na(df$Stipa.krylovii)),]

# get total biomass production
df$anpp <- rowSums(df[,-c(1,2)])
#adding other columns
df$site_code <- "DL"
df$project_name <- "NSFC"
df$data_type <- "biomass"
df$treatment_year <- df$Y - 2005

# making a unique plot number for each treatment-plot combination
trt_plot <- as.data.frame(unique(df$species))
trt_plot <- cbind(seq(1:28), trt_plot)
names(trt_plot)<- c("plot_id", "species")
# take out number in front of treatment
trt_plot$treatment <- sub('.', '', trt_plot$species)
trt_plot$block <- substr(trt_plot$species, 1, 1)
# add data back into df
df <- merge(df, trt_plot)


df1 <- gather(df, key = genus_species, value = abundance, 3:58)
df1 <- df1[,c(2,9,8,6,7,4,5,10,11)]
names(df1)[1] <- "calendar_year"

write.csv(df1, "Data/CleanedData/Sites/Species csv/DL_NSFC20132016.csv", row.names = FALSE)

## pull out anpp dataset
df2 <- df[,c(2,59:66)]
names(df2)[1] <- "calendar_year"
df2 <- df2[,c(1,8,7,5,6,9,3,4,2)]
write.csv(df2, "Data/CleanedData/Sites/ANPP csv/DL_NSFC_anpp20132016.csv", row.names = FALSE)

