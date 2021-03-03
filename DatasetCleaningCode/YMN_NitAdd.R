#####################
#### YMN_NitAdd ####
####################

# NO = plots 1-6; N5 = plots 7-12; N10 = plots 13-18; 
# N20 = plots 19-24; N40 = plots 25-30; N80 = plots 31-36
setwd("/Users/kaitlinkimmel/Dropbox/CoRRE_database")
library(readxl) # Reading in as excel files. can update later to be csv files
library(tidyr)
# get all file names
file_names <- dir(path ="/Users/kaitlinkimmel/Dropbox/CoRRE_database/Data/OriginalData/Sites/YMN_NitAdd/2020_update", pattern=".xlsx") # get names all .csv files
#combine into one dataframe
newdf <- lapply(paste("/Users/kaitlinkimmel/Dropbox/CoRRE_database/Data/OriginalData/Sites/YMN_NitAdd/2020_update/",file_names, sep =""), read_excel) #combine all .csv files
# delete empty rows & add year column
for (i in 1:length(newdf)){
  df <- newdf[[i]]
  df<- df[colSums(!is.na(df)) > 0]
  df <- df[rowSums(is.na(df)) != ncol(df),]
  df$calendar_year <- i + 2012
  df$treatment_year <- i
  df$site_code <- "YMN"
  df$project_name <- "NitAdd"
  df$data_type <- "biomass"
  newdf[[i]] <- df
}

#combine list into dataframe
newdf <- do.call(rbind, newdf)

# Pull anpp data
df1 <- newdf[newdf$Species == "Aboveground biomass" | newdf$Species == "Above biomass",]
df1 <- gather(df1, key = plot_id, value = anpp, 2:37)
df1$treatment <- NA
df1$treatment[df1$plot_id %in% c(1:6)] <- "N0"
df1$treatment[df1$plot_id %in% c(7:12)] <- "N5"
df1$treatment[df1$plot_id %in% c(13:18)] <- "N10"
df1$treatment[df1$plot_id %in% c(19:24)] <- "N20"
df1$treatment[df1$plot_id %in% c(25:30)] <- "N40"
df1$treatment[df1$plot_id %in% c(31:36)] <- "N80"
df1 <- df1[,c(2,9,7,6,3,4,5,8)]
write.csv(df1, "Data/CleanedData/Sites/ANPP csv/YMN_NitAdd_anpp.csv", row.names = FALSE)

# species data
df2 <- newdf[newdf$Species != "Aboveground biomass",]
df2 <- df2[df2$Species != "Above biomass",]
df2 <- gather(df2, key = plot_id, value = abundance, 2:37)
df2$treatment <- NA
df2$treatment[df2$plot_id %in% c(1:6)] <- "N0"
df2$treatment[df2$plot_id %in% c(7:12)] <- "N5"
df2$treatment[df2$plot_id %in% c(13:18)] <- "N10"
df2$treatment[df2$plot_id %in% c(19:24)] <- "N20"
df2$treatment[df2$plot_id %in% c(25:30)] <- "N40"
df2$treatment[df2$plot_id %in% c(31:36)] <- "N80"
names(df2)[1] <- "genus_species"

df2 <- df2[,c(2,9,7,6,3,4,5,1,8)]

write.csv(df2, "Data/CleanedData/Sites/Species csv/YMN_NitAdd.csv", row.names = FALSE)
# setwd("~/Dropbox/converge_diverge/datasets/ORIGINAL_DATASETS/YMN_NitAdd")
# 
# library(dplyr)
# library(tidyr)
# 
# sp13<-read.csv("sp2013.csv")
# 
# sp14<-read.csv("sp2014.csv")
# 
# sp15<-read.csv("sp2015.csv")
# all<-rbind(sp13, sp14, sp15)
# 
# sp<-all%>%
#   gather(genus_species, abundance,Heteropappus.altaicus: Ixeridium.chinense)
# 
# treatment_year<-sp%>%
#   select(calendar_year)%>%
#   unique()%>%
#   mutate(treatment_year=seq(1,3, by=1))
# 
# sp2<-merge(sp, treatment_year, by="calendar_year")%>%
#   mutate(site_code="YMN",
#          project_name="NitAdd")%>%
#   select(-rep)
# 
# speciesdata<-sp2%>%
#   select(-anpp)
# 
# write.csv(speciesdata, "YMN_NitAdd.csv")
# 
# anppdata<-sp2%>%
#   select(site_code, project_name, treatment, calendar_year, treatment_year, plot_id, anpp)%>%
#   unique()

write.csv(anppdata, "YMN_NitAdd_anpp.csv")


