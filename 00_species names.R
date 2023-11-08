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
  filter(tree.non.tree %in% c('non-tree')) %>% 
  select(database, family, genus_species, species_matched) %>% 
  na.omit() %>% 
  separate(species_matched, into=c('genus', 'species', 'subspp'), sep=' ') %>% 
  filter(!is.na(species)) %>% 
  unite(col='species_matched', genus:species, sep=' ', remove=T) %>% 
  select(-subspp)

# write.csv(gex, 'OriginalData\\Traits\\GEx_species_family_May2023.csv', row.names=F)


#### New spp from GEx ####
corre <- read.csv('OriginalData\\Traits\\TRY\\corre2trykey_2021.csv') %>% 
  select(species_matched) %>% 
  mutate(database2='corre') %>% 
  unique()

gexOnly <- read.csv('OriginalData\\Traits\\GEx_species_family_May2023.csv') %>% 
  full_join(corre) %>% 
  filter(is.na(database2)) %>% 
  select(family, species_matched) %>% 
  unique()

# write.csv(gexOnly, 'OriginalData\\Traits\\GEx_uniqueSpp_June2023.csv', row.names=F)