################################################################################
##  ChinaTraits.R: Gathering data from the China Plant 2 Trait Database for all CoRRE species.
##
##  Authors: Meghan Avolio, Kimberly Komatsu
################################################################################

library(tidyverse)

setwd('C:\\Users\\kjkomatsu\\Dropbox (Smithsonian)\\working groups\\CoRRE\\CoRRE_database\\Data') #Kim
setwd('E:\\Dropbox\\CoRRE_database\\Data') #Meghan

#species from China Plant Trait Database 2 that are in CoRRE database
spList <- read.csv('OriginalData\\Traits\\ChinaPlant2\\Species translations.csv') %>% 
  unite(col='species_matched', ACCEPTED.GENUS:ACCEPTED.SPECIES, sep=' ') %>% 
  select(species_matched, Site.ID, SAMPLE.ID) %>% 
  left_join(read.csv("CompiledData\\Species_lists\\species_families_trees_2021.csv")) %>% 
  filter(tree.non.tree=='non-tree')


#trait data
chem <- read.csv("OriginalData\\Traits\\ChinaPlant2\\Chemical traits.csv") %>% 
  filter(flagged=="") %>% 
  mutate(leaf_area=Average.LA*1000000) %>% #unit conversion to TRY standards: m2 to mm2
  mutate(LDMC=LDMC/1000) %>% #unit conversion to TRY standards: mg/g to g/g
  select(-LMA,-Narea, -Parea, -Karea, -d13C.12C, -d15N.14N, -flagged, -Average.LA) %>% 
  rename(leaf_C=Cmass, 
         leaf_N=Nmass,
         leaf_P=Pmass, 
         leaf_K=Kmass) %>% 
  pivot_longer(SLA:leaf_area, names_to="CleanTraitName", values_to="StdValue") %>% 
  right_join(spList) %>% 
  na.omit()

photo <- read.csv("OriginalData\\Traits\\ChinaPlant2\\Photosynthetic traits.csv") %>% 
  filter(flagged=="") %>% 
  select(SAMPLE.ID, Vcmax, Jmax) %>% 
  rename(Vc_max=Vcmax,
         J_max=Jmax) %>% 
  pivot_longer(Vc_max:J_max, names_to='CleanTraitName', values_to='StdValue') %>% 
  right_join(spList) %>% 
  na.omit()

#bind together
traits <- rbind(chem, photo) %>% 
  mutate(DatabaseID='CPTD2') %>% 
  rename(DatasetID=Site.ID,
         ObservationID=SAMPLE.ID) %>% 
  select(-tree.non.tree)


# write.csv(traits, 'OriginalData\\Traits\\ChinaPlant2\\CPTD2_March2023.csv', row.names=F)