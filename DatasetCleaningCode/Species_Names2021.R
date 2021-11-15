###########################
### Species cleaning ####
########################

## In this script the species names are cleaned so that they are standardized
## between experiments and to be merged with TRY.
## Two files are read in - the full species list created in the Abundance_merge.R
## file and the list of the corrected names from version 1.0 (note: no code for this
## because a lot of the cleaning for this list was done by hand)
## the full species list is run through the TPL() function from Taxonstand
## to save time so the species list does not always have to be run through TPL()
## intermediate stages are saved to run the rest of the code

##this file creates 1) a full list of species names that are cleaned to merge with TRY, FullList_Nov2021, 2) a list of new species that need categorical traits for newsp2021.csv, and 3) a list of family, and whether it is a tree species for all cleaned names. This needs to then be filled by hand for those that are new and unclear species_families_trees_2021_toclean.csv

# Set working directory
setwd("~/Dropbox/CoRRE_database/")
setwd("C:\\Users\\mavolio2\\Dropbox\\CoRRE_database/")

# load libraries
library(Taxonstand)
library(tidyverse)
library(WorldFlora)

# import data
full.list <- read.csv ("Data/CompiledData/Species_lists/SpeciesList_Nov2021.csv")
oldsp <- read.csv("Data/CompiledData/Species_lists/fullsp_list2020.csv")%>%
  mutate(oldsp=1)%>% # create a column to indicate name was in old species list
  rename(genus_species=species)

# moss <- read.delim("Data/OriginalData/Traits/Bryophytes.txt", sep = ",", header = FALSE) #from: http://www.mobot.org/mobot/tropicos/most/bryolist.shtml
tree.spold <- read.csv("Data/TRYCoRREMerge/species_families_trees_compelete_2020.csv")

WFO.file<-read.delim("Data/CompiledData/Species_lists/WFO_Backbone/classification.txt")


#Merge new list with old list
merged.list<-full.list%>%
  left_join(oldsp)

#species already matched
newmatched<-merged.list%>%
  filter(oldsp==1)

# get species that are not in old list 
unidentified.sp<-merged.list%>%
  filter(is.na(oldsp))%>%
  select(genus_species)

##export this list and note what are unknowns
#write.csv(unidentified.sp, "Data/CompiledData/Species_lists/splist_toclean.csv", row.names=F)
#read in and drop unknowns

unid<-read.csv("Data/CompiledData/Species_lists/splist_toclean_cleaned.csv")

unknowns<-unid%>%
  filter(species_matched=="Unknown")%>%
  mutate(type="Unknown", remove=1)

unid2<-unid%>%
  filter(species_matched!="Unknown")%>%
  select(-species_matched)

# Run species which were not identified in the old list through the TPL function to match with accepted species names
clean.list <- TPL(unid2$genus_species)

# Pull out matched species (i.e. those that had a match in The Plant List) -
# These do not need to be checked further. 

matched.sp<-clean.list%>%
  filter(New.Taxonomic.status=="Accepted"|New.Taxonomic.status=="Unresolved"&Typo==FALSE)%>%
  mutate(species_matched=paste(New.Genus, New.Species, sep=" "))%>%
  select(Taxon, species_matched)%>%
  rename(genus_species=Taxon)%>%
  mutate(type="identified species", remove=0)

## Pull out species with no match in TPL to hand fix names
#unmatched.sp2 <- clean.list%>%
  filter(New.Taxonomic.status==""|(New.Taxonomic.status=="Unresolved"&Typo==TRUE))

genera<-unmatched.sp2%>%
  filter(Typo==FALSE&Species=="sp"|Species=="spp"|Species=="species")%>%
  filter(Genus!="")%>%
  select(Taxon, Genus, Species, New.Genus, New.Species)%>%
  rename(genus_species=Taxon)%>%
  mutate(species_matched=paste(New.Genus, "sp.", sep = " "))%>%
  mutate(type="identified genus", remove=1)%>%
  select(-Genus, -Species, -New.Genus, -New.Species)

g<-genera%>%
  mutate(g=1)

#sptoclean<-unmatched.sp2%>%
  rename(genus_species=Taxon)%>%
  left_join(g)%>%
  filter(is.na(g))%>%
  select(genus_species, New.Genus, New.Species)

#write.csv(sptoclean, "Data/CompiledData/Species_lists/splist_dobyhand.csv", row.names=F)

lastsp<-read.csv("Data/CompiledData/Species_lists/splist_dobyhand_cleaned.csv")%>%
  select(-New.Genus, -New.Species)

newlist<-newmatched%>%
  select(-oldsp)%>%
  bind_rows(unknowns, matched.sp, genera, lastsp)%>%
  unique()

# caplitalize first letter of species names function modified from https://rstudio-pubs-static.s3.amazonaws.com/408658_512da947714740b99253228f084a08a9.html
CapStr <- function(y) { 
  c <- strsplit(y, " ")[[1]][1]
  d <- strsplit(y, " ")[[1]][2]
  paste(paste(toupper(substring(c, 1,1)), substring(c, 2),
              sep="", collapse=" "), d, sep = " ")
}

newlist$species_matched2 <- sapply(newlist$species_matched, CapStr)

newlist2<-newlist%>%
  select(-species_matched)%>%
  rename(species_matched=species_matched2)

#export full cleaned list with whether to remove or not for all species
#write.csv(newlist2, "Data/CompiledData/Species_lists/FullList_Nov2021.csv", row.names=F)

newlist2<-read.csv("Data/CompiledData/Species_lists/FullList_Nov2021.csv")%>%
  select(-X)

uniquesp<-newlist2%>%
  filter(remove==0)%>%
  select(species_matched)%>%
  unique()
  
##figure out which are fully new species that we need to do traits for
unique.newsp<-matched.sp%>%
  bind_rows(lastsp)%>%
  filter(remove==0)%>%
  select(species_matched)%>%
  mutate(species_matched=tolower(species_matched))%>%
  unique()%>%
  mutate(new.sp=1)

unique.oldsp<-newmatched%>%
  filter(remove==0)%>%
  select(species_matched)%>%
  mutate(species_matched=tolower(species_matched))%>%
  unique()%>%
  mutate(old.sp=1)

needtraits<-unique.oldsp%>%
  full_join(unique.newsp)%>%
  filter(is.na(old.sp))

needtraits$species_matched2 <- sapply(needtraits$species_matched, CapStr)

needtraits2<-needtraits%>%
  select(species_matched2)%>%
  rename(species_matched=species_matched2)

#export new list of speices for categorical traits
write.csv(needtraits2, "Data/CompiledData/Species_lists/newsp2021.csv", row.names = F)



### Create file to indicate if the species is a tree or not and add family information

#getting family info with WorldFlora 
#i can't get this to work. I am taking a manual approach
#families<-WFO.match(spec.data=uniquesp, WFO.data=WFO.file)

join<-uniquesp%>%
  rename(scientificName=species_matched)%>%
  left_join(WFO.file)%>%
  select(scientificName, family)%>%
  unique()%>%
  rename(species_matched=scientificName,
         newfamily=family)

#get list of those tree species i still need
tree.sp<-tree.spold%>%
  right_join(join)%>%
  mutate(tree.non.tree2=ifelse(is.na(tree.non.tree)&newfamily %in% c("Betulaceae", "Fagaceae", "Juglandaceae", "Pinaceae"), "tree", ifelse(!is.na(tree.non.tree), tree.non.tree, NA)))%>%
  select(species_matched, newfamily, tree.non.tree2)%>%
  rename(family=newfamily, tree.non.tree=tree.non.tree2)

write.csv(tree.sp, "Data/CompiledData/Species_lists/species_families_trees_2021_toclean.csv", row.names=F)
