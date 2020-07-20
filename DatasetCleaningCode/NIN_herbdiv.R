######################
#### NIN_herbdiv ####
####################
setwd("/Users/kaitlinkimmel/Dropbox/CoRRE_database")

df <- read.csv("Data/OriginalData/Sites/NIN_HerbDiv/HerbDiv_2019_Cover_Tx_Trait_toKaitlin.csv")

# Looks like no "Psuedocage" and "Trnch CTL" from previous years, will exclude those for now

df <- df[df$EXCL_Code %in% 1:5,]
df$Fert[df$Fert == "NoFert"] <- "NF"
df$Fert[df$Fert == "Fert"] <- "F"
df$treatment <- paste(df$EXCL_Code, df$Fert, sep = "")
df$site_code <- "NIN"
df$project_name <- "HerbDiv"
df$treatment_year <- df$Year - 2009
df$data_type <- "cover"

df <- df[,c(3,18,1,5,22,21,19,20,9,4)]
names(df)[c(1,3,4,9,10)] <- c("calendar_year", "plot_id", "block", "genus_species", "abundance")

write.csv(df, "Data/CleanedData/Sites/Species csv/NIN_herbdiv.csv", row.names = FALSE)



# setwd("~/Dropbox/converge_diverge/datasets/ORIGINAL_DATASETS/houseman and russell/herbdiv")
# 
# library(tidyr)
# library(dplyr)
# 
# treatinfo<-read.delim('NIN_herbdiv.txt')%>%
#   select(plot_id, treatment)%>%
#   unique()
# 
# species<-read.csv("HerbDiv Plant Comp2015 CovDivgWrkGrp.csv")
# 
# treatment_year<-species%>%
#   select(calendar_year)%>%
#   unique()%>%
#   mutate(treatment_year=seq(1,6,by=1))
# 
# species2<-merge(species, treatment_year, by="calendar_year")
# species3<-merge(species2, treatinfo, by="plot_id")%>%
#   mutate(site_code="NIN", project_name="herbdiv")
# spcode<-read.csv("sp_codes.csv")
# species4<-merge(species3,spcode, by="genus_species")%>%
#   mutate(genus_species=Scientific.name)%>%
#   select(-Scientific.name)
# 
# write.csv(species4, "NIN_herbdiv.csv")
# 
# ###can ignore everything below
# 
# specieslist<-species%>%
#   select(genus_species)%>%
#   unique()
# 
# write.csv(specieslist, "NIN_species.csv")
# 
# #plot_id probelm - use plots from ealier the other 16 are structural controls
# 
# plots1<-species%>%
#   select(plot_id)%>%
#   unique()
# 
# plots<-merge(treatinfo, plots1, by="plot_id", all=T)
# write.csv(plots, "NIN_plots.csv")
