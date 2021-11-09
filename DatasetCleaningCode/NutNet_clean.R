######################
## NutNet Batch ####
####################

setwd("~/Dropbox/CoRRE_database")
setwd('C:\\Users\\komatsuk\\Dropbox (Smithsonian)\\working groups\\CoRRE\\CoRRE_database') #kim's desktop
setwd('C:\\Users\\lapie\\Dropbox (Smithsonian)\\working groups\\CoRRE\\CoRRE_database') #kim's laptop

# libraries
library(Hmisc)

## data
dat <- read.csv("Data/OriginalData/Sites/NutNet/nutnet_cover_11042021.csv", row.names = 1)
bio_dat <- read.csv("Data/OriginalData/Sites/NutNet/nutnet_anpp_11042021.csv", row.names = 1)
# sites included in this dataset:
# "cdcr.us", "cbgb.us", "lake.us", "lancaster.uk", "chilcas.ar" , "potrok.ar", 
# "shps.us","sier.us", "temple.us", "veluwe.nl",  "yarra.au", "bayr.de"


# fix names & get rid of unnecessary columns
dat <- dat%>%
  rename(calendar_year=year, plot_id=plot, treatment_year=year_trt, treatment=trt, genus_species=Taxon, abundance=max_cover)%>%
  filter(live==1)%>% # get live abundance
  select(calendar_year, treatment_year, site_code, block, treatment, plot_id, genus_species, abundance)

# Species names to first letter capital only
dat$genus_species <- tolower(dat$genus_species)
dat$genus_species <- capitalize(dat$genus_species)

# add in other information
dat$project_name <- "NutNet"
dat$data_type <- "cover"
dat2 <- dat%>%
  mutate(site_code=ifelse(site_code=='bayr.de', 'Bt', ifelse(site_code=='cdcr.us', 'CDR', as.character(site_code))))

# write.csv(dat2, "Data/CleanedData/Sites/Species csv/NutNet.csv", row.names = FALSE)

# get live biomass only
bio_dat <- bio_dat[which(bio_dat$live ==1),]
# sum up categories to get total biomass

bio_dat <- aggregate(bio_dat$mass, by = list(calendar_year = bio_dat$year, treatment_year = bio_dat$year_trt,
                                             treatment = bio_dat$trt, site_code = bio_dat$site_code, block = bio_dat$block,
                                             plot_id = bio_dat$plot), FUN = sum)
names(bio_dat)[7] <- "anpp"
# add in other information
bio_dat$project_name <- "NutNet"
bio_dat$data_type <- "biomass"
bio_dat2 <- bio_dat%>%
  mutate(site_code=ifelse(site_code=='bayr.de', 'Bt', ifelse(site_code=='cdcr.us', 'CDR', as.character(site_code))))

# write.csv(bio_dat2, "Data/CleanedData/Sites/ANPP csv/NutNet_anpp.csv", row.names = FALSE)



##get number of reps
rep <- bio_dat2%>%
  select(site_code, treatment, plot_id)%>%
  unique()%>%
  group_by(site_code, treatment)%>%
  summarise(num_reps=length(plot_id))%>%
  ungroup()

##get number of reps
rep2 <- dat2%>%
  select(site_code, treatment, plot_id)%>%
  unique()%>%
  group_by(site_code, treatment)%>%
  summarise(num_reps=length(plot_id))%>%
  ungroup()
