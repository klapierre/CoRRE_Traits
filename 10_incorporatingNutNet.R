################################################################################
##  10_incorporatingNutNet.R: Adding NutNet species, generated from codominance2 repository.
##
##  Authors: Kimberly Komatsu
##  Date created: April 17, 2026
################################################################################

#### Set up working space ####

# rm(list=ls()) clean up workspace
library(readxl)
library(tidyverse)


##### CoRRE and GEx Traits from EDI #####
correGExTraitsContinuous <- read.csv('https://pasta.lternet.edu/package/data/eml/edi/1533/3/169fc12d10ac20b0e504f8d5ca0b8ee8')

correGExTraitsCategorical <- read.csv('https://pasta.lternet.edu/package/data/eml/edi/1533/3/5ebbc389897a6a65dd0865094a8d0ffd') %>% 
  mutate(trait_value=ifelse(family=='Cactaceae' & trait=='leaf_type', 'modified', trait_value),
         trait_value=ifelse(family=='Cactaceae' & trait=='leaf_compoundness', 'simple', trait_value),
         trait_value=ifelse(species=='Adesmia lotoides' & trait=='leaf_compoundness', 'compound', trait_value))

##### NutNet Traits - imputed/gathered for this project #####
nutnetTraitsContinuous <- read.csv('C:\\Users\\kjkomatsu\\OneDrive - UNCG\\manuscripts\\1_first author\\codominance\\data\\NutNet/NutNet_continuousTraitData_imputed_20240711.csv') %>% 
  filter(!species %in% correGExTraitsContinuous$species) %>% 
  rename(family=Family)

## NutNet N-fixers ##
nutnetNfixer <- read.csv('C:\\Users\\kjkomatsu\\OneDrive - UNCG\\manuscripts\\1_first author\\codominance\\data\\NutNet/NutNet_species_list_N-fixers.csv') %>% 
  select(species_matched, n_fixer) %>% 
  rename(species=species_matched,
         n_fixation_type=n_fixer) %>% 
  mutate(n_fixation_type_source='Werner')

nutnetTraitsCategorical <- read_xlsx('C:\\Users\\kjkomatsu\\OneDrive - UNCG\\manuscripts\\1_first author\\codominance\\data\\NutNet/NutNet_categorical_traits_2024b.xlsx') %>% 
  select(family, species_matched, leaf_type, leaf_type_source, leaf_compoundness, leaf_compoundness_source, growth_form, growth_form_source,
         photosynthetic_pathway, photosynthetic_pathway_source, lifespan, lifespan_source, stem_support, stem_support_source, clonal, clonal_source) %>% 
  rename(species=species_matched) %>% 
  full_join(nutnetNfixer) %>% 
  pivot_longer(leaf_type:n_fixation_type_source, names_to='trait2', values_to='trait_value') %>% 
  unique() %>% 
  filter(!species %in% correGExTraitsCategorical$species) %>% 
  mutate(type=ifelse(str_detect(trait2, "_source"), 'source', 'trait_value'),
         trait=str_remove(trait2, "_source$")) %>% 
  select(-trait2) %>% 
  pivot_wider(names_from=type, values_from=trait_value) %>% 
  mutate(error_risk_overall=case_when(
    trait=='leaf_type' ~ 0.002,
    trait=='leaf_compoundness' ~ 0.002,
    trait=='stem_support' ~ 0.033,
    trait=='growth_form' ~ 0.009,
    trait=='photosynthetic_pathway' ~ 0.017,
    trait=='lifespan' ~ 0.033,
    trait=='clonal' ~ 0.050,
    trait=='n_fixation_type' ~ NA
  ))
  


##### rbind #####
continuousTraits <- rbind(correGExTraitsContinuous, nutnetTraitsContinuous)
# write.csv(continuousTraits, 'C:\\Users\\kjkomatsu\\OneDrive - UNCG\\manuscripts\\1_first author\\2024_corre traits_Nature Scientific Data\\with BIEN and TIPleaf\\trait data for EDI\\v3\\CoRRE_continuousTraitData_Apr2026.csv', row.names=F)

length(unique(continuousTraits$species))
length(unique(continuousTraits$family))

categoricalTraits <- rbind(correGExTraitsCategorical, nutnetTraitsCategorical) %>% 
  filter(!is.na(family)) %>% 
  mutate(trait_value=ifelse(trait_value=='awl', 'scale', trait_value))
# write.csv(categoricalTraits, 'C:\\Users\\kjkomatsu\\OneDrive - UNCG\\manuscripts\\1_first author\\2024_corre traits_Nature Scientific Data\\with BIEN and TIPleaf\\trait data for EDI\\v3\\CoRRE_categoricalTraitData_Apr2026.csv', row.names=F)

length(unique(categoricalTraits$species))
length(unique(categoricalTraits$family))




##### source data for imputation #####
nutnetSource <- read.csv('C:\\Users\\kjkomatsu\\OneDrive - UNCG\\manuscripts\\1_first author\\codominance\\data\\nutnet\\nutnet_trait database_combo_continuous_20250701_long.csv') %>% 
  rename(species=species_matched,
         trait=CleanTraitName,
         trait_value=StdValue)

correGExSource <- read.csv('https://pasta.lternet.edu/package/data/eml/edi/1533/3/f21fe032152862d12f85d7d4b0eda94a') 
