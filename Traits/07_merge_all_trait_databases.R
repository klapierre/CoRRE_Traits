################################################################################
##  merge_all_trait_databases.R: Putting together data from TRY, BIEN, and AusTraits.
##
##  Authors: Kimberly Komatsu, Meghan Avolio
################################################################################

library(tidyverse)

setwd('C:\\Users\\mavolio2\\Dropbox\\CoRRE_database\\Data') #meghan's
setwd('C:\\Users\\kjkomatsu\\Dropbox (Smithsonian)\\working groups\\CoRRE\\CoRRE_database\\Data') #kim's


#### Read in data ####

# Cleaned species names
names <- read.csv("CompiledData\\Species_lists\\species_families_trees_2021.csv")

# AusTraits
AusTraits <- read.csv('OriginalData\\Traits\\AusTraits_2022\\AusTraits_CoRRE_March2023.csv') %>%
  left_join(names) %>%
  filter(tree.non.tree=="non-tree") %>% 
  mutate(species_matched2=species_matched)%>%
  separate(species_matched2, into=c('genus', 'species'))%>%
  mutate(DatabaseID='AusTraits')%>%
  select(DatabaseID, DatasetID, ObservationID, family, species_matched, genus, CleanTraitName, StdValue) %>% 
  filter(StdValue>0)


# TRY
TRY <- read.csv('OriginalData\\Traits\\TRY\\TRYCoRREMerge\\TRY_trait_data_continuous_long_March2023.csv') %>% 
  mutate(DatabaseID="TRY") %>% 
  filter(StdValue>0)

# BIEN
BIEN <- read.csv('OriginalData\\Traits\\BIEN\\BIEN_for_scorre_20230309.csv') %>% 
  left_join(names) %>%
  select(DatabaseID, DatasetID, ObservationID, family, species_matched, genus, CleanTraitName, StdValue) %>% 
  filter(StdValue>0)

# TiP leaf
TiP <- read.csv('OriginalData\\Traits\\TiP_leaf\\TiP_leaf_March2023.csv') %>% 
  filter(StdValue>0)
  

# Bind all together
allTraits <- rbind(TRY, AusTraits, BIEN, TiP) %>% 
  select(DatabaseID, DatasetID, ObservationID, family, genus, species_matched, CleanTraitName, StdValue)

# Are there any outlier datasets for each trait?
ggplot(data=subset(allTraits, CleanTraitName %in% c('dark_resp_rate', 'J_max', 'LDMC', 'leaf_area', 'leaf_C', 'leaf_C.N',
                                                    'leaf_density', 'leaf_dry_mass', 'leaf_K', 'leaf_longevity', 'leaf_N', 
                                                    'leaf_N.P', 'leaf_P', 'leaf_thickness', 'leaf_transp_rate', 'leaf_width',
                                                    'photosynthesis_rate', 'plant_height_vegetative', 'RGR', 'root.shoot', 
                                                    'root_C', 'root_density', 'root_diameter', 'root_dry_mass', 'root_N', 
                                                    'root_P', 'rooting_depth', 'seed_dry_mass', 'seed_length', 'seed_number',
                                                    'seed_terminal_velocity', 'SLA', 'SRL', 'stem_spec_density',
                                                    'stomatal_conductance', 'Vc_max')),
       aes(x=DatabaseID, y=StdValue, color=DatabaseID)) +
  geom_boxplot() +
  facet_wrap(~CleanTraitName, scales='free')


# How well correlated is BIEN SLA with the others? No overlap, so not relevant.
# test <- allTraits %>%
#   group_by(DatabaseID, species_matched, CleanTraitName) %>%
#   summarise(mean=mean(StdValue)) %>%
#   ungroup() %>%
#   pivot_wider(names_from=DatabaseID, values_from=mean)
# 
# ggplot(data=subset(test, !is.na(TRY) & !is.na(BIEN) & CleanTraitName=='SLA'), aes(x=TRY, y=BIEN)) +
#   geom_point() +
#   geom_abline(slope=1)
# 
# test2 <- subset(test, !is.na(TRY) & !is.na(BIEN) & CleanTraitName=='SLA')
# 
# cor.test(test2$TRY, test2$BIEN)
# cor.test(test2$TRY, test2$AusTraits)


# Transpose to wide format for gap filling.
talltraits <- allTraits %>% 
  group_by(DatabaseID, DatasetID, ObservationID, family, genus, species_matched) %>%
  pivot_wider(names_from=CleanTraitName, values_from=StdValue, values_fill=NA) %>% 
  ungroup()

# write.csv(allTraits, 'OriginalData\\Traits\\raw traits for gap filling\\TRYAusBIEN_continuous_March2023c_long.csv', row.names = F)

# write.csv(talltraits, 'OriginalData\\Traits\\raw traits for gap filling\\TRYAusBIEN_continuous_March2023c.csv', row.names = F)

##checking traits
test <- allTraits %>% 
  group_by(species_matched, CleanTraitName, StdValue, DatabaseID) %>% 
  summarize(n=length(StdValue)) %>% 
  ungroup() %>% 
  filter(n>10)

sum(test[,'n'])

## all databases have repeats - 28,042 across all data
## only 2298 are the same values repeated 10 or more times and were designated as keepers from cleaning code