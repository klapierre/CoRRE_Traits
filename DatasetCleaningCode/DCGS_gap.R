setwd("~/Dropbox/CoRRE_database")

library(readxl)
library(tidyr)

df <- read.csv("Data/OriginalData/Sites/DCGS_gap/data20082018.csv")
sp_names <- read_excel("Data/OriginalData/Sites/DCGS_gap/DCGS sp list_2018update.xlsx")
sp_names <- sp_names[,c(1,2)]

df <- gather(df, match_col, abundance, 6:144)

names(sp_names) <- c("match_col","genus_species")

df <- merge(df,sp_names, all.x = TRUE)

df[is.na(df)] <- 0
df <- df[df$Block %in% c(4,7),]
df <- df[df$match_col != "ExpUnID",]

df <- aggregate(df$abundance, by = list(calendar_year = df$year, treatment = df$trtmt, plot = df$plot,
                                          block = df$Block, genus_species = df$genus_species, 
                                        match_col = df$match_col), FUN = mean)
for (i in 1:nrow(df)){
  if(df$genus_species[i] == 0){
    df$genus_species[i] <- df$match_col[i]
  }
}

names(df)[7] <- "abundance"
df$plotmerge <- paste(df$treatment, df$plot, sep = "::")
plot_id <- data.frame(plotmerge = unique(df$plotmerge))
plot_id$plot_id <- seq(1:nrow(plot_id))
df <- merge(df, plot_id)
df <- df[,-c(1,4)]

df$site_code <- "DCGS"
df$project_name <- "gap"
df$treatment_year <- df$calendar_year - 1994
df$data_type <- "cover"
df <- df[,-5]

write.csv(df, "Data/CleanedData/Sites/Species csv/dcgs_gap.csv", row.names = FALSE)


# setwd("~/Documents/Dropbox/converge_diverge/datasets/checked data_04292014/not_perfect")
# dir()
# gap<-read.delim("DCGS_gap.txt")
# library(reshape2)
# gap2<-gap[-5]
# 
# melt<-melt(gap, id="replicate", "calendar_year")
# 
# means<-aggregate(gap2[,6:130], gap2[,1:5], mean)
# write.table(means, "gap_means.txt")
