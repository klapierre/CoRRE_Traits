###################
#### CDR_e001 ####
##################
setwd("/Users/kaitlinkimmel/Dropbox/CoRRE_database")
file <- "https://pasta.lternet.edu/package/data/eml/knb-lter-cdr/14/8/057e39850bd748d364df8a5ef60bb08d"
df <- read.delim(file, header = TRUE)
df <- df[,c(2,3,4,5,9,10)]
df$treatment_year <- df$Year - 1981
df$data_type <- "biomass"
df$site_code <- "CDR"
df$project_name <- "e001"
names(df)[c(1:6)] <- c("calendar_year", "community_type", "plot_id", "treatment", "genus_species", "abundance")
df <- df[,c(1,4,3,2,8,7,9,10,5,6)]

# several duplicated rows mostly from 2014, get rid of these
df <- df[which(duplicated(df) == FALSE),]

write.csv(df, "Data/CleanedData/Sites/Species csv/CDR_e001.csv", row.names = FALSE)

anpp <- aggregate(df$abundance, by = list(calendar_year = df$calendar_year, treatment = df$treatment, 
                                          plot_id = df$plot_id, community_type = df$community, 
                                          data_type = df$data_type, treatment_year = df$treatment_year, 
                                          site_code = df$site_code, project_name = df$project_name), 
                  FUN = sum)
names(anpp)[9] <- "anpp"
write.csv(anpp, "Data/CleanedData/Sites/ANPP csv/CDR_e001_anpp.csv", row.names = FALSE)

# setwd("~/Dropbox/converge_diverge/datasets/ORIGINAL_DATASETS/CDR e001")
# 
# library(dplyr)
# library(tidyr)
# 
# splist<-read.csv("e001mega13.csv")%>%
#   mutate(treatment=NTrt,
#          calendar_year=Year,
#          plot_id=Plot,
#          community_type=Field)%>%
#   gather(genus_species, abundance, Acernegu:Ziziapte)
# 
# treatment_year<-splist%>%
#   select(calendar_year)%>%
#   arrange(calendar_year)%>%
#   unique()%>%
#   mutate(treatment_year=seq(1,31, by=1))
# 
# sp3<-merge(treatment_year, splist, by="calendar_year")%>%
#   select(calendar_year, treatment_year, community_type, plot_id, treatment, genus_species, abundance)%>%
#   mutate(site_code="CDR", project_name="e001")%>%
#   na.omit%>%
#   filter(abundance>0)
# 
# write.csv(sp3, "CDR_e001.csv")
# 
# anpp<-sp3%>%
#   ungroup()%>%
#   filter(genus_species!="Fungi"&genus_species!="Misclitt")%>%
#   group_by(calendar_year, treatment_year, community_type, plot_id, treatment, site_code, project_name)%>%
#   summarize(anpp=sum(abundance))
# 
# 
