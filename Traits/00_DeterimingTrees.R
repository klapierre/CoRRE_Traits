################################################################################
##  DeterminingTrees.R: Figuring out which species are trees from the GEx database to remove for trait collection.
##
##  Authors: Meghan Avolio
################################################################################

library(tidyverse)

setwd("C://Users//mavolio2//Dropbox//CoRRE_Database//Data//CompiledData//Species_lists//")

treefam<-read.csv("species_families_trees_2021.csv")

gex<-read.csv("C://Users\\mavolio2\\Dropbox\\CoRRE_database\\Data\\OriginalData\\Traits\\GEx_species_family.csv") %>% 
  rename(species_matched=clean_ejf, family=family_ejf) %>% 
  select(species_matched, family) %>% 
  left_join(treefam)

#write list of families to fill in data
# gex_family<-gex %>% 
#   select(family) %>% 
#   unique %>% 
#   mutate(tree.non.tree=NA)
# write.csv(gex_family, "C://Users\\mavolio2\\Dropbox\\CoRRE_database\\Data\\OriginalData\\Traits\\GEx_family_list.csv", row.names=F)

#read in gex famlies
gex_family_clean<-read.csv("C://Users\\mavolio2\\Dropbox\\CoRRE_database\\Data\\OriginalData\\Traits\\GEx_family_list_filled.csv") %>% 
  rename(tree=tree.non.tree)

gex_tree<-gex %>% 
  left_join(gex_family_clean) %>% 
  rename(istree=tree.non.tree) %>% 
  mutate(tree.non.tree=ifelse(!is.na(istree), istree, tree)) %>% 
  select(species_matched, family, tree.non.tree)

write.csv(gex_tree, "C://Users\\mavolio2\\Dropbox\\CoRRE_database\\Data\\OriginalData\\Traits\\GEx_tree_list_tofill.csv", row.names=F)