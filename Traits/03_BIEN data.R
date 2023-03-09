################################################################################
##  gather BIEN data.R: Gathering BIEN trait data for CoRRE database plant species.
##
##  Authors: Tim Ohlert, Meghan Avolio, Kimberly Komatsu
################################################################################

library(BIEN)
library(tidyverse)

#kim's
setwd('C:\\Users\\kjkomatsu\\Dropbox (Smithsonian)\\working groups\\CoRRE\\CoRRE_database\\Data')


# Import CoRRE species names
correSpecies <- read.csv("CompiledData\\Species_lists\\FullList_Nov2021.csv") %>%  #species names are standardized 
  select(genus_species, species_matched) %>% 
  unique()

sp.vector <- unique(correSpecies$species_matched)

# Get BIEN data
bienData <- BIEN_trait_species(species=sp.vector)

# Subset to data that we want
continuous <- bienData %>% 
  filter(trait_name %in% c('leaf area', 'leaf area per dry mass', 'leaf carbon content per leaf dry mass', 
                           'leaf carbon content per leaf nitrogen content', 'leaf dry mass', 'leaf dry mass per leaf fresh mass',
                           'leaf life span', 'leaf nitrogen content per leaf area', 'leaf nitrogen content per leaf dry mass',
                           'leaf phosphorus content per leaf area', 'leaf phosphorus content per leaf dry mass',
                           'leaf photosynthetic rate per leaf area', 'leaf stomatal conductance for H2O per leaf area',
                           'leaf stomatal conductance per leaf area', 'leaf thickness', 'seed length', 'seed mass', 'stem wood density',
                           'whole plant height')) %>% 
  mutate(trait_value=as.numeric(trait_value)) %>% 
  # Standardize units to fit TRY
  mutate(clean_trait_value=ifelse(trait_name=='leaf dry mass per leaf fresh mass', trait_value/1000, #LDMC (BIEN mg/g   TRY g/g)
                           ifelse(trait_name=='leaf area per leaf dry mass', trait_value*1000, #SLA (BIEN m2/kg   TRY mm2/g)
                           ifelse(trait_name=='leaf carbon content per area', trait_value*1000, #leaf C per area (BIEN kg/m2  TRY g/m2)
                           ifelse(trait_name=='leaf nitrogen content per area', trait_value*1000, #leaf N per area (BIEN kg/m2  TRY g/m2)
                           ifelse(trait_name=='leaf phosphorous content per area', trait_value*1000, #leaf P per area (BIEN kg/m2  TRY g/m2)
                           ifelse(trait_name=='leaf dry mass', trait_value*1000, #leaf dry mass (BIEN g   TRY mg)
                           ifelse(trait_name=='leaf stomatal conductance for H2O per leaf area', trait_value*1000, #stomatal conductance (BIEN mol/m2/s   TRY millimol/m2/s
                           ifelse(trait_name=='leaf stomatal conductance per leaf area', trait_value/1000, #stomatal conductance (BIEN micromol/m2/s   TRY millimol/m2/s
                                  trait_value)))))))))
    
# Change BIEN trait names to fit TRY trait names
continuous$trait_name <- recode(continuous$trait_name, 
                                'leaf area'='leaf_area',
                                'leaf area per dry mass'='SLA',
                                'leaf carbon content per leaf dry mass'='leaf_C',
                                'leaf carbon content per leaf nitrogen content'='leaf_C:N',
                                'leaf dry mass'='leaf_dry_mass',
                                'leaf dry mass per leaf fresh mass'='LDMC',
                                'leaf life span'='leaf_longevity',
                                'leaf nitrogen content per leaf area'='50',
                                'leaf nitrogen content per leaf dry mass'='leaf_N',
                                'leaf phosphorus content per leaf area'='51',
                                'leaf phosphorus content per leaf dry mass'='leaf_P',
                                'leaf photosynthetic rate per leaf area'='photosynthesis_rate',
                                'leaf stomatal conductance for H2O per leaf area'='stomatal_conductance',
                                'leaf stomatal conductance per leaf area'='stomatal_conductance',
                                'leaf thickness'='leaf_thickness',
                                'seed length'='seed_length',
                                'seed mass'='seed_dry_mass',
                                'stem wood density'='stem_spec_density',
                                'whole plant height'='plant_height_generative')


# Unify with other dataset columns
continuousFinal <- continuous %>% 
  mutate(DatabaseID='BIEN') %>% 
  rename(DatasetID=project_pi,
         ObservationID=id,
         species_matched=scrubbed_species_binomial,
         CleanTraitName=trait_name,
         StdValue=clean_trait_value) %>% 
  select(DatabaseID, DatasetID, ObservationID, species_matched, CleanTraitName, StdValue) %>% 
#Remove trees
  left_join(read.csv("CompiledData\\Species_lists\\species_families_trees_2021.csv")) %>% 
  filter(tree.non.tree != "tree") %>% 
# Make a genus column
  separate(species_matched, into = c("genus","species"), sep=" ", remove=FALSE) %>% 
  select(-species)


# write.csv(continuousFinal, "OriginalData\\Traits\\BIEN\\BIEN_for_scorre_20230309.csv")



# ######################################
# #Categorical
# categorical_for_corre <- subset(bien_data, 
#                                 trait_name == "whole plant growth form"|
#                                   trait_name =="whole plant vegetative phenology"|
#                                   trait_name ==  "flower pollination syndrome"|
#                                   trait_name ==  "whole plant sexual system"|
#                                   trait_name ==   "leaf compoundness"|
#                                   trait_name ==  "whole plant dispersal syndrome")
# 
# #categorical
# 
# categorical_for_corre$TRY_trait <- revalue(categorical_for_corre$trait_name, c(
#   "whole plant growth form" = "Plant life form (Raunkiaer life form)",
#   "whole plant vegetative phenology" = "Plant vegetative phenology (leaf phenology)",
#   "flower pollination syndrome" = "Pollination syndrome",
#   "whole plant sexual system" = "Flower secual syndrome (dichogamy, cleistogamy, dioecious, monoecious)",
#   "whole plant dispersal syndrome" = "Dispersal syndrome",
#   "leaf compoundness" = "Leaf compoundness"
# ))
# 
# 
# final_categorical <- merge(categorical_for_corre, corre_species, by.x="scrubbed_species_binomial",by.y="Name_matched",all.x = TRUE)
# #trim unnecessary columns to simplify the spreadsheets before uploading to Dropbox
# final_categorical <- final_categorical[,c("trait_value","method",#"id",
#                                           "TRY_trait","genus species")]
# final_categorical <- unique(final_categorical)
# 
# 
# #write.csv(final_categorical,"BIEN_categorical_traits_2-20-20.csv")









