################################################################################
##  TiPleaf data.R: Gathering TiP Leaf trait data for CoRRE database plant species.
##
##  Authors: Kimberly Komatsu
################################################################################

library(tidyverse)
library(readxl)

#kim's
setwd('C:\\Users\\kjkomatsu\\Dropbox (Smithsonian)\\working groups\\CoRRE\\CoRRE_database\\Data')

# Import CoRRE species names
correSpecies <- read.csv("CompiledData\\Species_lists\\FullList_Nov2021.csv") %>%  #species names are standardized
  left_join(read.csv("CompiledData\\Species_lists\\species_families_trees_2021.csv")) %>% 
  filter(tree.non.tree != "tree") %>% #Remove trees
  separate(species_matched, into=c('genus', 'species', 'subspp'), sep=' ') %>% 
  filter(species!='sp.') %>% 
  unite(col='species_matched', genus:species, sep=' ', remove=T) %>% 
  select(family, species_matched) %>% 
  unique()

# Import GEx species names
GExSpecies <- read.csv('OriginalData\\Traits\\GEx_species_tree_complete.csv') %>% 
  filter(tree.non.tree=='non-tree') %>% 
  select(family, species_matched) %>% 
  unique()


# Combine species lists
allSpecies <- rbind(correSpecies, GExSpecies) %>% 
  unique()


# Import trait data and subset to our species of interest
tip <- read_xlsx('OriginalData\\Traits\\TiP_leaf\\The TiP-Leaf dataset.xlsx', sheet='plant traits') %>% 
  rename(species_matched=Species) %>%
  filter(species_matched!='/') %>% 
  mutate(ObservationID=row_number()) %>% 
  mutate(DatasetID='1', DatabaseID='TIPleaf') %>% 
  left_join(allSpecies) %>% 
  mutate(LCC=as.numeric(ifelse(LCC=='/', NA, LCC)),
         LNC=as.numeric(ifelse(LNC=='/', NA, LNC)),
         LPC=as.numeric(ifelse(LPC=='/', NA, LPC)),
         SLA=SLA/10) %>% #unit conversion to TRY standards: cm2/g to mm2/mg 
  select(DatabaseID, DatasetID, ObservationID, species_matched, LT, DW, LDMC, LA, SLA, LCC, LNC, LPC) %>% 
  pivot_longer(LT:LPC, names_to='trait_name', values_to='StdValue') %>% 
  unique()

tip$CleanTraitName <- recode(tip$trait_name, 
                             'LT'='leaf_thickness',
                             'DW'='leaf_dry_mass',
                             'LA'='leaf_area',
                             'LCC'='leaf_C',
                             'LNC'='leaf_N',
                             'LPC'='leaf_P')

# write.csv(tip, 'OriginalData\\Traits\\TiP_leaf\\TiP_leaf_May2023.csv', row.names=F)