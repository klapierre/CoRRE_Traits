library(tidyverse)

#read in names
names <- read.csv('C:\\Users\\kjkomatsu\\Dropbox (Smithsonian)\\working groups\\CoRRE\\CoRRE_database\\Data\\CompiledData\\Species_lists\\species_families_trees_2021.csv')



#read in AusTraits
AusTraits <- read.csv('C:\\Users\\kjkomatsu\\Dropbox (Smithsonian)\\working groups\\CoRRE\\CoRRE_database\\Data\\OriginalData\\Traits\\AusTraits_2022\\AusTraits_CoRRE_Nov2022.csv')%>%
  left_join(names)%>%
  mutate(species_matched2=species_matched)%>%
  separate(species_matched2, into=c('genus', 'species'))%>%
  mutate(DatabaseID='AusTraits')%>%
  select(DatabaseID, DatasetID, ObservationID, family, species_matched, genus, CleanTraitName, StdValue)
  



#read in TRY
TRY <- read.csv('C:\\Users\\kjkomatsu\\Dropbox (Smithsonian)\\working groups\\CoRRE\\sDiv\\sDiv_sCoRRE_shared\\CoRRE data\\trait data\\Raw TRY Data\\TRY Continuous data\\TRY_trait_data_continuous_long_Nov2021.csv')%>%
  mutate(DatabaseID='TRY')


#rbind
allTraits <- rbind(TRY, AusTraits)
