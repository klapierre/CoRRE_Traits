setwd("~/Dropbox/CoRRE_database")
setwd("C:\\Users\\lapie\\Dropbox (Smithsonian)\\working groups\\CoRRE\\CoRRE_database\\")


library(readxl)
library(tidyverse)

df <- read.csv("Data/OriginalData/Sites/DCGS_gap/data20082018.csv")
sp_names <- read_excel("Data/OriginalData/Sites/DCGS_gap/DCGS sp list_2018update.xlsx")
sp_names <- sp_names[,c(1,2)]

df <- gather(df, match_col, abundance, 6:144)

names(sp_names) <- c("match_col","genus_species")

df2 <- merge(df,sp_names, all.x = TRUE)%>%
  filter(abundance>0, abundance!='NA', genus_species!='NA')%>%
  mutate(plot_id=paste(Block, plot, trtmt, sep='_'))%>%
  group_by(year, Block, plot_id, trtmt, genus_species)%>%
  summarise(abundance=mean(abundance))%>%
  ungroup()%>%
  mutate(site_code='DCGS', project_name='gap', community_type=0, data_type='cover')%>%
  rename(calendar_year=year, treatment=trtmt, block=Block)%>%
  mutate(treatment_year=calendar_year-min(calendar_year)+1)


write.csv(df2, "Data/CleanedData/Sites/Species csv/dcgs_gap.csv", row.names = FALSE)


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
