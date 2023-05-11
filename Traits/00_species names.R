################################################################################
##  species names.R: Merging CoRRE and GEx species lists.
##
##  Authors: Kimberly Komatsu
################################################################################

library(tidyverse)

setwd('C:\\Users\\kjkomatsu\\Dropbox (Smithsonian)\\working groups\\CoRRE\\CoRRE_database\\Data')

gex <- read.csv('OriginalData\\Traits\\GEx_species_family.csv') %>% 
  rename(species_matched=clean_ejf,
         family=family_ejf) %>% 
  mutate(database='GEx') %>% 
  select(database, family, genus_species, species_matched)
  

corre <- read.csv('CompiledData\\Species_lists\\FullList_Nov2021.csv') %>% 
  select(genus_species, species_matched) %>% 
  full_join(read.csv('CompiledData\\Species_lists\\species_families_trees_2021.csv')) %>% 
  mutate(database='CoRRE') %>% 
  select(database, family, genus_species, species_matched)

allNames <- rbind(corre, gex)

# write.csv(allNames, 'OriginalData\\Traits\\CoRRE_GEx_species_family_April2023.csv', row.names=F)

tree <- read.csv('C:\\Users\\kjkomatsu\\Desktop\\GEx_family_list.csv') %>% 
  left_join(allNames) %>% 
  select(-genus_species) %>% 
  filter(database=='GEx') %>% 
  unique()

# write.csv(tree, 'OriginalData\\Traits\\GEx_tree_April2023.csv', row.names=F)