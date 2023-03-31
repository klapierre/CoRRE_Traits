################################################################################
##  TiPleaf data.R: Gathering TiP Leaf trait data for CoRRE database plant species.
##
##  Authors: Kimberly Komatsu
################################################################################

library(tidyverse)
library(readxl)

#kim's
setwd('C:\\Users\\kjkomatsu\\Dropbox (Smithsonian)\\working groups\\CoRRE\\CoRRE_database\\Data')


tip <- read_xlsx('OriginalData\\Traits\\TiP_leaf\\The TiP-Leaf dataset.xlsx', sheet='plant traits') %>% 
  rename(species_matched=Species, ObservationID=Site) %>% 
  mutate(DatasetID='1', DatabaseID='TIPleaf') %>% 
  left_join(read.csv("CompiledData\\Species_lists\\species_families_trees_2021.csv")) %>% 
  filter(tree.non.tree != "tree") %>% 
  mutate(LCC=as.numeric(ifelse(LCC=='/', NA, LCC)),
         LNC=as.numeric(ifelse(LNC=='/', NA, LNC)),
         LPC=as.numeric(ifelse(LPC=='/', NA, LPC)),
         SLA=SLA/10) %>% #unit conversion to TRY standards: cm2/g to mm2/mg 
  select(DatabaseID, DatasetID, ObservationID, species_matched, family, LT, DW, LDMC, LWC, LA, SLA, LCC, LNC, LPC) %>% 
  pivot_longer(LT:LPC, names_to='trait_name', values_to='StdValue')

tip$CleanTraitName <- recode(tip$trait_name, 
                             'LT'='leaf_thickness',
                             'DW'='leaf_dry_mass',
                             'LWC'='water_content',
                             'LA'='leaf_area',
                             'LCC'='leaf_C',
                             'LNC'='leaf_N',
                             'LPC'='leaf_P')

# write.csv(tip, 'OriginalData\\Traits\\TiP_leaf\\TiP_leaf_March2023.csv', row.names=F)