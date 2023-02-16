remotes::install_github("traitecoevo/austraits", 
                        dependencies = TRUE, upgrade = "ask", 
                        build_vignettes = TRUE, force=T)

vignette("austraits")
library(austraits)
library(tidyverse)

austraits <- load_austraits(version = "4.1.0", path = "data/austraits")

traits <- summarise_austraits(austraits, "trait_name")

#doesn't have seed number, stem specific density, rooting depth
data <- extract_trait(austraits, c('seed_dry_mass',
                                   'leaf_CN_ratio', 
                                   'leaf_N_per_dry_mass', 'leaf_N_per_area',
                                   'leaf_C_per_dry_mass', 
                                   'leaf_P_per_dry_mass', 'leaf_P_per_area',
                                   'leaf_dry_matter_content', 
                                   'leaf_dry_mass', 
                                   'plant_height', 
                                   'leaf_mass_per_area', #need to inverse this
                                   'root_specific_root_length', 
                                   'leaf_water_content_per_dry_mass'#, 'leaf_water_content_per_area', 'leaf_water_content_per_fresh_mass', 'leaf_water_content_per_saturated_mass'
                                   ))


traitData <- data$traits%>%
  mutate(DatabaseID='AusTraits')%>%
  rename(DatasetID=dataset_id,
         ObservationID=observation_id,
         species_matched=taxon_name,
         StdValue=value) %>% 
  mutate(StdValue=ifelse(trait_name=='leaf_mass_per_area', (1/StdValue)*1000, StdValue),
         trait_name=ifelse(trait_name=='leaf_mass_per_area', 'specific_leaf_area', trait_name))

names <- read.csv('C:\\Users\\kjkomatsu\\Dropbox (Smithsonian)\\working groups\\CoRRE\\CoRRE_database\\Data\\CompiledData\\Species_lists\\FullList_Nov2021.csv')%>%
  select(-X)%>%
  filter(remove==0)%>%
  select(species_matched)%>%
  unique()%>%
  mutate(corre='y')

traitDataCoRRE <- traitData%>%
  left_join(names)%>%
  filter(corre=='y')%>%
  mutate(CleanTraitName=ifelse(trait_name=='seed_mass', 'seed_dry_mass',
                               ifelse(trait_name=='leaf_CN_ratio', 'leaf_C:N',
                                      ifelse(trait_name=='leaf_N_per_dry_mass', 'leaf_N',
                                             ifelse(trait_name=='leaf_N_per_area', '50',
                                                    ifelse(trait_name=='leaf_C_per_dry_mass', 'leaf_C',
                                                           ifelse(trait_name=='leaf_P_per_dry_mass', 'leaf_P',
                                                                  ifelse(trait_name=='leaf_P_per_area', '51',
                                                                         ifelse(trait_name=='leaf_dry_matter_content', 'LDMC',
                                                                                ifelse(trait_name=='plant_height', 'plant_height_vegetative',
                                                                                       ifelse(trait_name=='specific_leaf_area', 'SLA',
                                                                                              ifelse(trait_name=='root_specific_root_length
', 'SRL',
                                                                                                     ifelse(trait_name=='leaf_water_content_per_dry_mass', 'water_content', trait_name)))))))))))))

spp <- traitDataCoRRE%>%
  select(species_matched)%>%
  unique()

# write.csv(traitDataCoRRE, 'C:\\Users\\kjkomatsu\\Dropbox (Smithsonian)\\working groups\\CoRRE\\CoRRE_database\\Data\\OriginalData\\Traits\\AusTraits_2022\\AusTraits_CoRRE_Feb2023.csv')
