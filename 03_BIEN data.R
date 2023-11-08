################################################################################
##  gather BIEN data.R: Gathering BIEN trait data for CoRRE database plant species.
##
##  Authors: Kimberly Komatsu, Tim Ohlert, Meghan Avolio
################################################################################

library(BIEN)
library(tidyverse)

#kim's
setwd('C:\\Users\\kjkomatsu\\Dropbox (Smithsonian)\\working groups\\CoRRE\\CoRRE_database\\Data')


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
GExSpecies <- read.csv('OriginalData\\Traits\\GEx_species_family_May2023.csv') %>% 
  select(species_matched) %>% 
  unique()

allSpecies <- rbind(correSpecies, GExSpecies) %>% 
  unique()

sp.vector <- unique(allSpecies$species_matched)

# Get BIEN data
bienData <- BIEN_trait_species(species=sp.vector)

# Subset to data that we want
continuous <- bienData %>% 
  filter(trait_name %in% c('leaf area', 'leaf area per dry mass', 'leaf dry mass', 'leaf dry mass per leaf fresh mass', 'seed mass',
                           'leaf nitrogen content per leaf dry mass'
                           # 'leaf life span', 'leaf nitrogen content per leaf area', 'leaf phosphorus content per leaf area',
                           # 'leaf photosynthetic rate per leaf area', 'stem wood density', 'leaf stomatal conductance for H2O per leaf area',
                           # 'leaf stomatal conductance per leaf area', 'leaf photosynthetic rate per leaf area', 'leaf carbon content per leaf dry mass', 
                           # 'leaf photosynthetic rate per leaf dry mass', 'leaf carbon content per leaf nitrogen content',
                           # 'leaf phosphorus content per leaf dry mass', 'leaf thickness', 'seed length'
                           )) %>% 
  mutate(trait_value=as.numeric(trait_value)) %>% 
  # Standardize units to fit TRY
  mutate(clean_trait_value=ifelse(trait_name=='leaf dry mass per leaf fresh mass', trait_value/1000, #LDMC (BIEN mg/g   TRY g/g)
                           ifelse(trait_name=='leaf area per leaf dry mass', trait_value*1000, #SLA (BIEN m2/kg   TRY mm2/g)
                           # ifelse(trait_name=='leaf carbon content per area', trait_value*1000, #leaf C per area (BIEN kg/m2  TRY g/m2)
                           # ifelse(trait_name=='leaf nitrogen content per area', trait_value*1000, #leaf N per area (BIEN kg/m2  TRY g/m2)
                           # ifelse(trait_name=='leaf phosphorous content per area', trait_value*1000, #leaf P per area (BIEN kg/m2  TRY g/m2)
                           ifelse(trait_name=='leaf dry mass', trait_value*1000, #leaf dry mass (BIEN g   TRY mg)
                                  trait_value)))) %>% 
  # Remove data that was not from a naturally growing plant
  filter(method!='laboratory/greenhouse/garden experiment',
         trait_value!=0) %>% 
  rename(species_matched=scrubbed_species_binomial) %>% 
  # Problem: A few datasets have lots of repeated data for some traits*species.
  # Solution: For each species, find if there is repeated data for all traits collected on an individual. 
  # Where this occurs, keep the lowest ObservationID.
  select(species_matched, trait_name, project_pi, id, clean_trait_value) %>% 
  pivot_wider(names_from=trait_name, values_from=clean_trait_value, names_prefix = "d__") %>% 
  group_by_at(vars(!id)) %>% 
  mutate(n=length(species_matched), obid2=min(id)) %>% 
  ungroup() %>% 
  select(-id) %>% 
  unique() %>% 
  pivot_longer(3:7, names_to = "CleanTraitName1", values_to = "StdValue") %>% 
  separate(CleanTraitName1, into = c("prefix", "CleanTraitName"), "__") %>% 
  select( -prefix, -n) %>% 
  na.omit() %>% 
  rename(ObservationID=obid2)

# Checking for duplicate data
test <- continuous %>% 
  group_by(species_matched, CleanTraitName, StdValue) %>% 
  summarize(n=length(StdValue)) %>% 
  ungroup() %>% 
  filter(n>2)

# Change BIEN trait names to fit TRY trait names
continuous$CleanTraitName <- recode(continuous$CleanTraitName, 
                                'leaf area'='leaf_area',
                                'leaf area per dry mass'='SLA',
                                # 'leaf carbon content per leaf dry mass'='leaf_C',
                                # 'leaf carbon content per leaf nitrogen content'='leaf_C:N',
                                'leaf dry mass'='leaf_dry_mass',
                                'leaf dry mass per leaf fresh mass'='LDMC',
                                # 'leaf life span'='leaf_longevity',
                                # 'leaf nitrogen content per leaf area'='50',
                                'leaf nitrogen content per leaf dry mass'='leaf_N',
                                # 'leaf phosphorus content per leaf area'='51',
                                # 'leaf phosphorus content per leaf dry mass'='leaf_P',
                                # 'leaf photosynthetic rate per leaf area'='photosynthesis_rate',
                                # 'leaf stomatal conductance for H2O per leaf area'='stomatal_conductance',
                                # 'leaf stomatal conductance per leaf area'='stomatal_conductance',
                                # 'leaf thickness'='leaf_thickness',
                                # 'seed length'='seed_length',
                                'seed mass'='seed_dry_mass'
                                # 'stem wood density'='stem_spec_density'
                                )


# Unify with other dataset columns
continuousClean <- continuous %>% 
  mutate(DatabaseID='BIEN') %>% 
  rename(DatasetID=project_pi) %>% 
  select(DatabaseID, DatasetID, ObservationID, species_matched, CleanTraitName, StdValue) %>% 
# Make a genus column
  separate(species_matched, into = c("genus","species"), sep=" ", remove=FALSE) %>% 
  select(-species) %>% 
# Filter outliers
  mutate(drop=ifelse(DatasetID %in% c('Abakumova M', 'Liu Y', 'Osborne CP') & CleanTraitName=='leaf_area', 1, #these studies did something other than leaf area (e.g., total leaf area for the whole plant)
              ifelse(DatasetID %in% c('Schmid B') & CleanTraitName=='leaf_dry_mass', 1, 0))) %>% #this study did something other than leaf dry mass (e.g., plant mass)
  filter(drop==0) %>% 
  select(-drop)



# write.csv(continuousClean, "OriginalData\\Traits\\BIEN\\BIEN_for_scorre_202310263.csv", row.names=F)



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
