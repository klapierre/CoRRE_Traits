library(BIEN)
library(tidyverse)

#get list of species and families
splist<-read.csv("C:\\Users\\mavolio2\\Dropbox\\CoRRE_database\\Data\\CompiledData\\Species_lists\\FullList_Nov2021.csv") %>% 
  filter(remove==0) 

family<-read.csv("C:\\Users\\mavolio2\\Dropbox\\CoRRE_database\\Data\\CompiledData\\Species_lists\\species_families_trees_2021.csv")

splistfam<-splist %>% 
  left_join(family) %>% 
  select(family) %>% 
  unique()

famlist<-splistfam$family

#download list of BIEN traits famliy
traits<-BIEN_trait_family(famlist)

write.csv(traits, 'C:\\Users\\mavolio2\\Dropbox\\sDiv_sCoRRE_shared\\CoRRE data\\trait data\\BIEN\\BIEN_Nov22.csv', row.names=F)

biensp<-traits %>%
  mutate(species_matched=paste(scrubbed_species_binomial, sep=' ')) %>% 
  select(species_matched) %>% 
  unique() %>% 
  mutate(bien="y")

bien<-traits %>% 
  rename(species_matched=scrubbed_species_binomial)

splist2<-splist %>% 
  select(species_matched) %>% 
  mutate(corre="y")

merge<-biensp %>% 
  left_join(splist2) %>% 
  filter(corre=='y')

biensubsp<-bien %>% 
  right_join(merge) %>% 
  left_join(family) %>% 
  filter(tree.non.tree=="non-tree")

biensubsplist<-biensubsp %>% 
  select(species_matched) %>% 
  unique()
write.csv(biensubsp, 'C:\\Users\\mavolio2\\Dropbox\\sDiv_sCoRRE_shared\\CoRRE data\\trait data\\BIEN\\BIEN_SpeciesSubset_Nov22.csv', row.names=F)

biensubsptraits<-biensubsp %>% 
  filter(trait_name %in% c('maximum whole plant height', 'seed mass', 'leaf dry mass', 'leaf nitrogen content per leaf dry mass', 'leaf carbon content per leaf nitrogen content', 'leaf phosphorus content per leaf dry mass','leaf dry mass per leaf fresh mass','leaf carbon content per leaf dry mass','leaf area per leaf dry mass', 'whole plant height'))


#read in try traits
try<-read.csv('C:\\Users\\mavolio2\\Dropbox\\sDiv_sCoRRE_shared\\CoRRE data\\trait data\\Raw TRY Data\\TRY Continuous data\\TRY_trait_data_continuous_long_Nov2021.csv')
