library(tidyverse)
library(data.table)
library(Hmisc)
library(utf8)

#Code originally from Habacuc

#meghan's
setwd("C:/Users/mavolio2/Dropbox/CoRRE_database/Data/")


#kim's
setwd('C:\\Users\\lapie\\Dropbox (Smithsonian)\\working groups\\CoRRE\\CoRRE_database\\Data')

#I am not sure what thi is for....
# checkcorre<-read.csv("../SpeciesRelativeAbundance_Nov2019.csv")

#load clean taxonomy for try
taxdat <- read_csv("TRYCoRREMerge/taxon_updates.csv")

#get rid of species matched by taxize
ii <- is.na(taxdat$species_Taxonstand) & !is.na(taxdat$species_taxize)

taxdat$species_matched[ii] <- taxdat$species[ii]

#select species and matched species columns
taxdat<-taxdat %>%
  select(species, species_matched)

#load corre species
corre <- read_csv("CompiledData/Species_lists/fullsp_list2020.csv")

corre<-corre %>%
  filter(remove==0)%>%#this filters out only to sp., unknowns, and non-vasular plants except ferns.
  select(species, species_matched)%>%
  rename(genus_species=species)

#get rid of taxize matches - we manually did all this
# ii <- is.na(taxcorre$species_Taxonstand) & !is.na(taxcorre$species_taxize)
# 
# taxcorre$species_matched[ii] <- taxcorre$species[ii]
#select submitted name and matched name

#read try 
try <- fread("TRYCoRREMerge/TryAccSpecies.txt",sep = "\t",data.table = FALSE,stringsAsFactors = FALSE,strip.white = TRUE)
#preprocess try
try %>%
  mutate(species= as_utf8(AccSpeciesName))->try
#capitalize records
try$species <- Hmisc::capitalize(tolower(try$AccSpeciesName))#why are we doing this?
try$match<-ifelse(try$species==try$AccSpeciesName,1,0)

#join try to updated taxonomu
try <- left_join(try,taxdat, by = c("AccSpeciesName"="species"))%>%
  select(-species)

#join corre to try
corre2try <- left_join(corre,try, by="species_matched")%>%
  unique()

#write.csv(corre2try, "TRYCoRREMerge/corre2trykey.csv", row.names=F)

#make comma separted row to submit to try 

try_list <- corre2try[["AccSpeciesID"]][!is.na(corre2try$AccSpeciesID)]

#write_delim(x = as.data.frame(t(try_list)), "TRYCoRREMerge/splist_for_try_request.csv",delim = ",",col_names = FALSE)

#do this for just new species

new<-read.csv("CompiledData/Species_lists/newsp2020.csv")%>%
  select(-X, -new.sp, -old.sp, -Family)

try_list_new<-corre2try%>%
  right_join(new)

try_list_new2 <- try_list_new[["AccSpeciesID"]][!is.na(try_list_new$AccSpeciesID)]

write_delim(x = as.data.frame(t(try_list_new2)), "TRYCoRREMerge/splist_for_try_request_Feb2021.csv",delim = ",",col_names = FALSE)

###generating list for phylogeney
#want to include all columns, and indicate where a moss/lichen, plus include anything that is identified to genera
taxcorreAll <- read.csv("CompiledData/Species_lists/fullsp_list2020.csv")%>%
  filter(remove!=3)%>% #this filters out unknowns, but keeps mosses/lichens and anything that was IDed to genus.
  select(species, species_matched, remove)%>%
  mutate(type=ifelse(remove==2, 'moss/lichen', ifelse(remove==1, 'identified genus', 'identified species')))%>%
  select(-remove)

correTaxonomyAll <- taxcorreAll%>%
  na.omit()%>%
  unique()%>%
  rename(genus_species=species)

write.csv(correTaxonomyAll, 'TRYCoRREMerge/CoRRE_TRY_species_list.csv')
