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
AusTraits <- read.csv('OriginalData\\Traits\\AusTraits_2022\\AusTraits_CoRRE_March2023.csv')%>%
  left_join(names)%>%
  filter(tree.non.tree=="non-tree") %>% 
  mutate(species_matched2=species_matched)%>%
  separate(species_matched2, into=c('genus', 'species'))%>%
  mutate(DatabaseID='AusTraits')%>%
  select(DatabaseID, DatasetID, ObservationID, family, species_matched, genus, CleanTraitName, StdValue)


# TRY
TRY <- read.csv('OriginalData\\Traits\\TRY\\TRYCoRREMerge\\TRY_trait_data_continuous_long_March2023.csv') %>% 
  mutate(DatabaseID="TRY")

# BIEN
BIEN <- read.csv('OriginalData\\Traits\\BIEN\\BIEN_trait_data_continuous_Nov2022.csv')

# Bind all together
allTraits <- rbind(TRY, AusTraits, BIEN) %>% 
  select(DatabaseID, DatasetID, ObservationID, family, genus, species_matched, CleanTraitName, StdValue)

# How well correlated is BIEN SLA with the others? Pretty good, despite not knowing whether the data was collected with or without petiole or on leaves or leaflets when leaves are compound.
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

# write.csv(allTraits, 'OriginalData\\Traits\\raw traits for gap filling\\TRYAusBIEN_continuous_March2023_long.csv', row.names = F)

# write.csv(talltraits, 'OriginalData\\Traits\\raw traits for gap filling\\TRYAusBIEN_continuous_March2023.csv', row.names = F)

##checking traits
test <- allTraits %>% 
  group_by(species_matched, CleanTraitName, StdValue, DatabaseID) %>% 
  summarize(n=length(StdValue)) %>% 
  ungroup() %>% 
  filter(n>9)

## all databases have repeats - 72,936 across all data
## only 5597 are the same values repeated 10 or more times and were designated as keepers from cleaning code