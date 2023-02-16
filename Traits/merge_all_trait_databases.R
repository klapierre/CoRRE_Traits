library(tidyverse)

setwd('C:\\Users\\mavolio2\\Dropbox\\CoRRE_database\\Data') #meghan's
setwd('C:\\Users\\kjkomatsu\\Dropbox (Smithsonian)\\working groups\\CoRRE\\CoRRE_database\\Data') #kim's

#read in names
names <- read.csv("CompiledData\\Species_lists\\species_families_trees_2021.csv")


#read in AusTraits
AusTraits <- read.csv('OriginalData\\Traits\\AusTraits_2022\\AusTraits_CoRRE_Feb2023.csv')%>%
  left_join(names)%>%
  filter(tree.non.tree=="non-tree") %>% 
  mutate(species_matched2=species_matched)%>%
  separate(species_matched2, into=c('genus', 'species'))%>%
  mutate(DatabaseID='AusTraits')%>%
  select(DatabaseID, DatasetID, ObservationID, family, species_matched, genus, CleanTraitName, StdValue)
  



#read in TRY
TRY <- read.csv('OriginalData\\Traits\\TRY\\TRY_trait_data_continuous_long_Nov2022.csv') %>% 
  mutate(DatabaseID="TRY")

#read in BIEN
BIEN<-read.csv('OriginalData\\Traits\\BIEN\\BIEN_trait_data_continuous_Nov2022.csv')

#rbind all together
allTraits <- rbind(TRY, AusTraits, BIEN) %>% 
  select(DatabaseID, DatasetID, ObservationID, family, genus, species_matched, CleanTraitName, StdValue)

# How well correlated is BIEN SLA with the others?
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


#transpose data like Franzi wants
talltraits<-allTraits %>% 
  group_by(DatabaseID, DatasetID, ObservationID, family, genus, species_matched)%>%
  spread(CleanTraitName, StdValue, fill=NA)

# write.csv(allTraits, 'OriginalData\\Traits\\raw traits for gap filling\\TRYAusBIEN_continuous_Nov2022_long.csv', row.names = F)

# write.csv(talltraits, 'OriginalData\\Traits\\raw traits for gap filling\\TRYAusBIEN_continuous_Nov2022.csv', row.names = F)

##checking traits
test<-allTraits %>% 
  group_by(DatabaseID, species_matched, CleanTraitName, StdValue) %>% 
  summarize(n=length(StdValue)) %>% 
  filter(n>10)

##all databases have repeats - 53488
##only 1009 are the same values repeated 10 or more times when controlling for database

test2<-allTraits %>% 
  group_by(species_matched, CleanTraitName, StdValue) %>% 
  summarize(n=length(StdValue)) %>% 
  filter(n>10)

#1154 values are repeated 10 or more times across all data