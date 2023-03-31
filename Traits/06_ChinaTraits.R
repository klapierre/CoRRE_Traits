library(tidyverse)

chinakey<-read.csv("E:\\Dropbox\\CoRRE_database\\Data\\OriginalData\\Traits\\ChinaPlant2\\Species translations.csv")%>% 
  mutate(species_matched=paste(ACCEPTED.GENUS, ACCEPTED.SPECIES, sep = " "))

correSpecies <- read.csv("E:\\Dropbox\\CoRRE_database\\Data\\CompiledData\\Species_lists\\FullList_Nov2021.csv")

overlap<-correSpecies %>% 
  left_join(chinakey) %>% 
  filter(!is.na(SPECIES.ID)) %>% 
  select(species_matched, SPECIES.ID, SAMPLE.ID) %>% 
  unique() %>% 
  separate(species_matched, into=c("genus", "species", sep=" "), remove = F) %>% 
  filter(species!="sp") %>% 
  select(species_matched, SPECIES.ID, SAMPLE.ID)

#read in traits
chem<-read.csv("E:\\Dropbox\\CoRRE_database\\Data\\OriginalData\\Traits\\ChinaPlant2\\Chemical traits.csv") %>% 
  right_join(overlap) %>% 
  filter(flagged=="") %>% 
  select(-LMA,-Narea, -Parea, -Karea, -d13C.12C, -d15N.14N, -flagged) %>% 
  rename(leaf_area=Average.LA,
         leaf_C=Cmass, 
         leaf_N=Nmass,
         leaf_P=Pmass, 
         leaf_K=Kmass) %>% 
  pivot_longer(leaf_area:leaf_K, names_to="CleanTraitName", value_to="StdValue")


morph<-read.csv("E:\\Dropbox\\CoRRE_database\\Data\\OriginalData\\Traits\\ChinaPlant2\\Morphometric traits.csv")
photo<-read.csv("E:\\Dropbox\\CoRRE_database\\Data\\OriginalData\\Traits\\ChinaPlant2\\Photosynthetic traits.csv")

#doesn't have seed number, stem specific density, rooting depth
data <- extract_trait(austraits, c('leaf_area',
                                   'leaf_C_per_dry_mass', 
                                   'leaf_CN_ratio',
                                   'leaf_dark_respiration_per_dry_mass',
                                   'leaf_density',
                                   'leaf_dry_mass', 
                                   'leaf_dry_matter_content', 
                                   'leaf_K_per_area',
                                   'leaf_K_per_dry_mass',
                                   'leaf_lifespan',
                                   'leaf_mass_per_area', #need to inverse this
                                   'leaf_N_per_dry_mass', 'leaf_N_per_area',
                                   'leaf_NP_ratio',
                                   'leaf_P_per_dry_mass', 'leaf_P_per_area',
                                   'leaf_photosynthesis_Jmax_per_area',
                                   'leaf_photosynthesis_Jmax_per_mass',
                                   'leaf_photosynthesis_Vcmax_per_area',
                                   'leaf_photosynthesis_Vcmax_per_mass',
                                   'leaf_stomatal_conductance_per_area_ambient',
                                   'leaf_thickness',
                                   'leaf_transpiration',
                                   'leaf_water_content_per_dry_mass',
                                   'leaf_water_content_per_saturated_mass',
                                   'leaf_width',
                                   'plant_height', 
                                   'root_C_per_dry_mass',
                                   'root_diameter',
                                   'root_N_per_dry_mass',
                                   'root_P_per_dry_mass',
                                   'root_shoot_ratio',
                                   'root_specific_root_length', 
                                   'seed_dry_mass',
                                   'seed_length'
                                   #, 'leaf_water_content_per_area', 'leaf_water_content_per_fresh_mass', 'leaf_water_content_per_saturated_mass'
))
