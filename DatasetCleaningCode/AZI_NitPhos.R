#####################
### AZI_NitPhos ####
###################
setwd("/Users/kaitlinkimmel/Dropbox/CoRRE_database")

dat <- read.csv("Data/OriginalData/Sites/AZI_NitPhos/Azi_quadrat.csv")

dat$year <- as.numeric(dat$year)
dat$repeat. <- as.numeric(dat$repeat.)
dat$cover <- as.numeric(dat$cover)
dat$mass <- as.numeric(dat$mass)
dat$treatment_year <- dat$year - 2010
dat <- dat[c(1,3,4,11,14,15,17)]

for (i in 1:nrow(dat)){
  if(dat$trt[i] == "CK"){
    dat$trt[i] = "N0P0"
  }
  if(dat$trt[i] == "N5"){
    dat$trt[i] = "N1P0"
  }
  if(dat$trt[i] == "N10"){
    dat$trt[i] = "N2P0"
  }
  if(dat$trt[i] == "N15"){
    dat$trt[i] = "N3P0"
  }
  if(dat$trt[i] == "P2"){
    dat$trt[i] = "N0P1"
  }
  if(dat$trt[i] == "P4"){
    dat$trt[i] = "N0P2"
  }
  if(dat$trt[i] == "P8"){
    dat$trt[i] = "N0P3"
  }
  if(dat$trt[i] == "NP2"){
    dat$trt[i] = "N2P1"
  }
  if(dat$trt[i] == "NP4"){
    dat$trt[i] = "N2P2"
  }
  if(dat$trt[i] == "NP8"){
    dat$trt[i] = "N2P3"
  }
}

cover_dat <- dat[,-5]
cover_dat <- cover_dat[-which(is.na(cover_dat$cover)),]
cover_dat$site_code <- "AZI"
cover_dat$project_name <- "NitPhos"
cover_dat$data_type <- "cover"
names(cover_dat)[c(1:5)] <- c("calendar_year", "treatment", "plot_id", "abundance", "genus_species")
cover_dat <- cover_dat[,c(1,2,3,9,6,7,8,5,4)]

write.csv(cover_dat, "Data/CleanedData/Sites/Species csv/AZI_NitPhos.csv", row.names = FALSE)



ANPP <- dat[,-4]
ANPP <- ANPP[-which(is.na(ANPP$mass)),]
ANPP <- aggregate(ANPP$mass, by = list(calendar_year = ANPP$year, treatment = ANPP$trt,
                                       plot_id = ANPP$repeat., treatment_year = ANPP$treatment_year),
                  FUN = sum)
names(ANPP)[5] <- "anpp"
ANPP$data_type <- "biomass"
ANPP$site_code <- "AZI"
ANPP$project_name <- "NitPhos"
ANPP <- ANPP[,c(3,6,2,4,1,7,8,5)]

write.csv(ANPP, "Data/CleanedData/Sites/ANPP csv/AZI_NitPhos.csv", row.names = FALSE)

# setwd("~/Dropbox/converge_diverge/datasets/ORIGINAL_DATASETS/AZI_NitPhos")
# 
# library(dplyr)
# library(tidyr)
# 
# dat2014<-read.csv("data2014.csv")
# dat2012<-read.csv("data2012.csv")
# dat2013<-read.csv("data2013.csv")
# 
# data<-rbind(dat2014, dat2013, dat2012)
# 
# treatment_year<-data%>%
#   select(calendar_year)%>%
#   unique()%>%
#   arrange(calendar_year)%>%
#   mutate(treatment_year=seq(2,4, by=1))
# 
# data2<-merge(data, treatment_year, by="calendar_year")%>%
#   mutate(site_code="AZI", project_name="NitPhos")
# 
# species<-data2%>%
#   filter(data_type=="cover")%>%
#   gather(genus_species, abundance, Scirpus.pumilus:Vicia.sepium.Linn)%>%
#   na.omit
# 
# write.csv(species, "AZI_NitPhos.csv")
# 
# anpp<-data2%>%
#   filter(data_type=="biomass")%>%
#   gather(genus_species, biomass, Scirpus.pumilus:Vicia.sepium.Linn)%>%
#   mutate(biomass2=as.numeric(biomass))%>%
#   na.omit%>%
#   tbl_df()%>%
#   group_by(plot_id, data_type, treatment, treatment_year, calendar_year, site_code, project_name)%>%
#   summarize(anpp=sum(biomass2)*4)
# 
# write.csv(anpp, "AZI_NitPhos_anpp.csv")
