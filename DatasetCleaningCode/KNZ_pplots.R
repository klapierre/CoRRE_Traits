######################
##### KNZ_pplots ####
#####################
setwd("/Users/kaitlinkimmel/Dropbox/CoRRE_database")

#Pull data from LTER site
file <- "https://pasta.lternet.edu/package/data/eml/knb-lter-knz/127/2/904b175920ac68a39f68969138b82475"
# Read in data
df <- read.csv(file, header= TRUE)
df$Treatment <- toupper(df$Treatment)
df$genus_species <- paste(df$Genus, df$Species, sep = " ")
df$site_code <- "KNZ"
df$project_name <- "pplots"
df$treatment_year <- df$RecYear - 2002
df$data_type  <- "cover"

df <- df[,c(3,5,4,13,12,10,11,9,6)]
names(df)[c(1,2,3,9)] <- c("calendar_year", "treatment", "plot_id", "abundance")

write.csv(df, "Data/CleanedData/Sites/Species csv/KNZ_pplots.csv", row.names = FALSE)



##### ANPP ####
file1 <- "https://pasta.lternet.edu/package/data/eml/knb-lter-knz/127/2/8e254ef79b10d53bb995e6e479d9d1af"
df1 <- read.csv(file1, header = TRUE)

df1$site_code <- "KNZ"
df1$project_name <- "pplots"
df1$treatment_year <- df1$RecYear - 2002
df1$data_type <- "biomass"

df1 <- df1[,c(3,5,4,10,9,7,8,6)]
names(df1)[c(1,2,3,8)] <- c("calendar_year", "treatment", "plot_id", "anpp")

write.csv(df1, "Data/CleanedData/Sites/ANPP csv/KNZ_pplots_anpp.csv")


# 
# setwd("~/Dropbox/converge_diverge/datasets/ORIGINAL_DATASETS/KNZ_pplots_newdata")
# 
# library(tidyr)
# library(dplyr)
# 
# #the species codes are all messed up I have to use this new data
# species<-read.csv("PPLOTS_SpCom_Plot_SpAcross.csv")%>%
#   select(-nitro, -phos, -N, -P)%>%
#   gather(spnum2, abundance, spp2:spp194)%>%
#   mutate(site_code="KNZ", project_name="pplots")
# 
# treatment_year<-species%>%
#   select(calendar_year)%>%
#   unique()%>%
#   mutate(treatment_year=seq(0,12, by=1))
# 
# species2<-merge(species, treatment_year, by="calendar_year")
# 
# specieslist<-read.csv("konza_spplist.csv")%>%
#   mutate(genus_species=paste(genus, spp.1, sep="_"))%>%
#   select(spnum2, genus_species)
# 
# species3<-merge(species2, specieslist, by="spnum2")%>%
#   select(-spnum2)
# 
# write.csv(species3, "KNZ_PPLOTS.csv")
# 
# 
# #biomass
# old<-read.delim("KNZ_PPLOTS_anpp.txt")%>%
#   select(site_code, project_name, treatment, calendar_year, treatment_year, plot_id, anpp)
# new<-read.csv("PPlots_Biomass_02-14.csv")
# plotinfo<-read.csv("treatments.csv")
# 
# new2<-merge(plotinfo, new, by=c("row","plot"))%>%
#   mutate(anpp=(grass+forb+woody)*10, site_code="KNZ",project_name="PPLOTS")%>%
#   select(site_code, project_name, calendar_year, plot_id, anpp)%>%
#   tbl_df()%>%
#   group_by(site_code, project_name, calendar_year, plot_id)%>%
#   summarize(anpp=mean(anpp))
# 
# treatinfo<-species%>%
#   select(plot_id, treatment)%>%
#   unique()
# 
# new3<-merge(treatinfo, new2, by="plot_id")
# new4<-merge(treatment_year, new3, by="calendar_year")
# 
# anpp<-rbind(new4, old)
# 
# write.csv(anpp, "KNZ_PPLOTS_anpp.csv")
