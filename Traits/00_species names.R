################################################################################
##  species names.R: Merging CoRRE and GEx species lists.
##
##  Authors: Kimberly Komatsu
################################################################################

library(tidyverse)

setwd('C:\\Users\\kjkomatsu\\Dropbox (Smithsonian)\\working groups\\CoRRE\\CoRRE_database\\Data')

gexOriginal <- read.csv('OriginalData\\Traits\\GEx_species_family.csv') %>% 
  rename(species_matched=clean_ejf,
         family=family_ejf) %>% 
  select(family, species_matched, genus_species)

gex <- read.csv('OriginalData\\Traits\\GEx_species_tree_complete.csv') %>% 
  mutate(database='GEx') %>% 
  left_join(gexOriginal) %>% 
  filter(tree.non.tree=='non.tree') %>% 
  select(database, family, genus_species, species_matched)
  
corre <- read.csv('CompiledData\\Species_lists\\FullList_Nov2021.csv') %>% 
  select(genus_species, species_matched) %>% 
  full_join(read.csv('CompiledData\\Species_lists\\species_families_trees_2021.csv')) %>% 
  mutate(database='CoRRE') %>% 
  select(database, family, genus_species, species_matched)

allNames <- rbind(corre, gex) %>% 
  na.omit() %>% 
  separate(species_matched, into=c('genus', 'species', 'subspp'), sep=' ') %>% 
  filter(!is.na(species)) %>% 
  unite(col='species_matched', genus:species, sep=' ', remove=T) %>% 
  select(-subspp)

# write.csv(allNames, 'OriginalData\\Traits\\CoRRE_GEx_species_family_May2023.csv', row.names=F)