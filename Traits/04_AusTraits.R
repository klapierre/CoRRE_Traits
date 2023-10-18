################################################################################
##  AusTraits_get and clean.R: Gathering data from AusTraits database for CoRRE database plant species.
##
##  Authors: Kimberly Komatsu, Meghan Avolio
################################################################################

#kim's
setwd('C:\\Users\\kjkomatsu\\Dropbox (Smithsonian)\\working groups\\CoRRE\\CoRRE_database\\Data\\')

#meghan's
setwd('C:\\Users\\mavolio2\\Dropbox\\CoRRE_database\\Data\\OriginalData\\Traits')

# remotes::install_github("traitecoevo/austraits",
#                         dependencies = TRUE, upgrade = "ask",
#                         build_vignettes = TRUE, force=T)

# vignette("austraits")
library(austraits)
library(tidyverse)

austraits <- load_austraits(version = "4.1.0", path = "data/austraits")

traits <- summarise_austraits(austraits, "trait_name")

#doesn't have seed number, stem specific density, rooting depth
data <- extract_trait(austraits, c('leaf_area',
                                   # 'leaf_C_per_dry_mass', 
                                   # 'leaf_CN_ratio',
                                   # 'leaf_dark_respiration_per_dry_mass',
                                   # 'leaf_density',
                                   'leaf_dry_mass', 
                                   'leaf_dry_matter_content', 
                                   # 'leaf_K_per_area',
                                   # 'leaf_K_per_dry_mass',
                                   # 'leaf_lifespan',
                                   'leaf_mass_per_area', #need to inverse this
                                   'leaf_N_per_dry_mass', 
                                   # 'leaf_N_per_area',
                                   # 'leaf_NP_ratio',
                                   # 'leaf_P_per_dry_mass', 'leaf_P_per_area',
                                   # 'leaf_photosynthesis_Jmax_per_area',
                                   # 'leaf_photosynthesis_Jmax_per_mass',
                                   # 'leaf_photosynthesis_Vcmax_per_area',
                                   # 'leaf_photosynthesis_Vcmax_per_mass',
                                   # 'leaf_stomatal_conductance_per_area_ambient',
                                   # 'leaf_thickness',
                                   # 'leaf_transpiration',
                                   # 'leaf_water_content_per_dry_mass',
                                   # 'leaf_water_content_per_saturated_mass',
                                   # 'leaf_width',
                                   'plant_height', 
                                   # 'root_C_per_dry_mass',
                                   # 'root_diameter',
                                   # 'root_N_per_dry_mass',
                                   # 'root_P_per_dry_mass',
                                   # 'root_shoot_ratio',
                                   'root_specific_root_length', 
                                   'seed_dry_mass'
                                   # 'seed_length',
                                   #, 'leaf_water_content_per_area', 'leaf_water_content_per_fresh_mass', 'leaf_water_content_per_saturated_mass'
                                   ))


traitData <- data$traits %>%
  mutate(DatabaseID='AusTraits') %>%
  rename(DatasetID=dataset_id,
         ObservationID=observation_id,
         species_matched=taxon_name,
         StdValue=value) %>% 
  mutate(StdValue=ifelse(trait_name=='leaf_mass_per_area', (1/StdValue)*1000, 
                  ifelse(trait_name=='root_specific_root_length', (StdValue*100), StdValue)),
         trait_name=ifelse(trait_name=='leaf_mass_per_area', 'specific_leaf_area', trait_name))


# Import CoRRE species names
correSpecies <- read.csv("CompiledData\\Species_lists\\FullList_Nov2021.csv") %>%  #species names are standardized
  left_join(read.csv("CompiledData\\Species_lists\\species_families_trees_2021.csv")) %>% 
  filter(tree.non.tree != "tree") %>% #Remove trees
  separate(species_matched, into=c('genus', 'species', 'subspp'), sep=' ') %>% 
  filter(species!='sp.') %>% 
  unite(col='species_matched', genus:species, sep=' ', remove=T) %>% 
  select(species_matched) %>% 
  unique()

# Import GEx species names
GExSpecies <- read.csv('OriginalData\\Traits\\GEx_species_tree_complete.csv') %>% 
  filter(tree.non.tree=='non-tree') %>% 
  select(species_matched) %>% 
  unique()


# Combine species lists
allSpecies <- rbind(correSpecies, GExSpecies) %>% 
  unique() %>% 
  mutate(keep='y')

# Gather trait data for only our species
traitDataCoRRE <- traitData %>%
  left_join(allSpecies) %>%
  filter(keep=='y') %>%
  group_by(DatabaseID, DatasetID, ObservationID, species_matched, trait_name) %>% 
  summarize(StdValue=max(StdValue)) %>% 
  ungroup() %>% 
  mutate(drop=ifelse(trait_name=='plant_height' & StdValue>40, 1,
              ifelse(trait_name=='seed_dry_mass' & StdValue>600, 1, 0))) %>% 
  filter(drop==0) %>% 
  select(-drop)

traitDataCoRRE$CleanTraitName <- recode(traitDataCoRRE$trait_name, 
                                        # 'leaf_C_per_dry_mass'='leaf_C', 
                                        # 'leaf_CN_ratio'='leaf_C:N',
                                        # 'leaf_dark_respiration_per_dry_mass'='dark_resp_rate',
                                        'leaf_dry_matter_content'='LDMC', 
                                        # 'leaf_K_per_area'='52',
                                        # 'leaf_K_per_dry_mass'='leaf_K',
                                        # 'leaf_lifespan'='leaf_longevity',
                                        'specific_leaf_area'='SLA', 
                                        'leaf_N_per_dry_mass'='leaf_N', 
                                        # 'leaf_N_per_area'='50',
                                        # 'leaf_NP_ratio'='leaf_N:P',
                                        # 'leaf_P_per_dry_mass'='leaf_P', 
                                        # 'leaf_P_per_area'='51',
                                        # 'leaf_photosynthesis_Jmax_per_area'='J_max',
                                        # 'leaf_photosynthesis_Jmax_per_mass'='270',
                                        # 'leaf_photosynthesis_Vcmax_per_area'='Vc_max',
                                        # 'leaf_photosynthesis_Vcmax_per_mass'='185',
                                        # 'leaf_stomatal_conductance_per_area_ambient'='stomatal_conductance',
                                        # 'leaf_transpiration'='leaf_transp_rate',
                                        # 'leaf_water_content_per_dry_mass'='water_content',
                                        # 'leaf_water_content_per_saturated_mass'='3122',
                                        'plant_height'='plant_height_vegetative', 
                                        # 'root_C_per_dry_mass'='root_C',
                                        # 'root_N_per_dry_mass'='root_N',
                                        # 'root_P_per_dry_mass'='root_P',
                                        # 'root_shoot_ratio'='root:shoot',
                                        'root_specific_root_length'='SRL')


spp <- traitDataCoRRE %>%
  select(species_matched) %>%
  unique()

# write.csv(traitDataCoRRE, 'OriginalData\\Traits\\AusTraits_2022\\AusTraits_CoRRE_June2023.csv')
