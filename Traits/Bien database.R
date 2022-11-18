library(BIEN)
library(tidyverse)

#get list of species and families
splist<-read.csv("C:\\Users\\mavolio2\\Dropbox\\CoRRE_database\\Data\\CompiledData\\Species_lists\\FullList_Nov2021.csv") %>%
  filter(remove==0)

family<-read.csv("C:\\Users\\mavolio2\\Dropbox\\CoRRE_database\\Data\\CompiledData\\Species_lists\\species_families_trees_2021.csv")
# 
# splistfam<-splist %>% 
#   left_join(family) %>% 
#   select(family) %>% 
#   unique()
# 
# famlist<-splistfam$family
# 
# #download list of BIEN traits famliy
# traits<-BIEN_trait_family(famlist)
# 
# write.csv(traits, 'C:\\Users\\mavolio2\\Dropbox\\sDiv_sCoRRE_shared\\CoRRE data\\trait data\\BIEN\\BIEN_Nov22.csv', row.names=F)
# 
# biensp<-traits %>%
#   mutate(species_matched=paste(scrubbed_species_binomial, sep=' ')) %>% 
#   select(species_matched) %>% 
#   unique() %>% 
#   mutate(bien="y")
# 
# bien<-traits %>% 
#   rename(species_matched=scrubbed_species_binomial)
# 
# splist2<-splist %>% 
#   select(species_matched) %>% 
#   mutate(corre="y")
# 
# merge<-biensp %>% 
#   left_join(splist2) %>% 
#   filter(corre=='y')
# 
# biensubsp<-bien %>% 
#   right_join(merge) %>% 
#   left_join(family) %>% 
#   filter(tree.non.tree=="non-tree")
# 
# biensubsplist<-biensubsp %>% 
#   select(species_matched) %>% 
#   unique()
# write.csv(biensubsp, 'C:\\Users\\mavolio2\\Dropbox\\sDiv_sCoRRE_shared\\CoRRE data\\trait data\\BIEN\\BIEN_SpeciesSubset_Nov22.csv', row.names=F)

####start here
biensubsp<-read.csv('C:\\Users\\mavolio2\\Dropbox\\sDiv_sCoRRE_shared\\CoRRE data\\trait data\\BIEN\\BIEN_SpeciesSubset_Nov22.csv') %>% 
  unique()


test<-biensubsp %>% 
  filter(trait_name=="maximum whole plant height"|trait_name=='whole plant height')

biensubsptraits<-biensubsp %>% 
  select(-tree.non.tree, -family, -corre, -bien, -latitude, -longitude, -elevation_m, -project_pi_contact) %>% 
  filter(trait_name %in% c('seed mass', 'leaf dry mass', 'leaf nitrogen content per leaf dry mass', 'leaf carbon content per leaf nitrogen content', 'leaf phosphorus content per leaf dry mass','leaf dry mass per leaf fresh mass','leaf carbon content per leaf dry mass','leaf area per leaf dry mass')) %>% 
  filter(trait_value!='*') %>% 
  mutate(traitvalue=as.numeric(trait_value)) %>% 
  mutate(CleanTraitName=ifelse(trait_name=='seed mass', "seed_dry_mass", ifelse(trait_name=='leaf dry mass', 'leaf_dry_mass', ifelse(trait_name=='leaf nitrogen content per leaf dry mass', 'leaf_N', ifelse(trait_name=='leaf phosphorus content per leaf dry mass', 'leaf_P', ifelse(trait_name=='leaf carbon content per leaf dry mass', 'leaf_C', ifelse(trait_name=='leaf carbon content per leaf nitrogen content', 'leaf_C:N', ifelse(trait_name=='leaf area per leaf dry mass', 'SLA', ifelse(trait_name=='leaf dry mass per leaf fresh mass', 'LDMC', 'todo'))))))))) %>% 
  mutate(StdValue=ifelse(trait_name=='seed mass', traitvalue, ifelse(trait_name=='leaf dry mass', traitvalue*1000, ifelse(trait_name=='leaf nitrogen content per leaf dry mass', traitvalue, ifelse(trait_name=='leaf nitrogen content per leaf dry mass', traitvalue, ifelse(trait_name=='leaf phosphorus content per leaf dry mass', traitvalue, ifelse(trait_name=='leaf carbon content per leaf dry mass', traitvalue, ifelse(trait_name=='leaf carbon content per leaf nitrogen content', traitvalue, ifelse(trait_name=='leaf dry mass per leaf fresh mass', traitvalue*1000, ifelse(trait_name=='leaf area per leaf dry mass', traitvalue, 999))))))))))

splistinclude<-biensubsptraits %>% 
  select(species_matched) %>% 
  unique

contributors=biensubsptraits %>% 
  select(url_source, project_pi) %>% 
  unique() %>% 
  mutate(DatabaseID=seq(1:47))

Bienoutput<-biensubsptraits %>% 
  left_join(contributors) %>% 
  mutate(DatabaseID='BIEN') %>% 
  rename(ObservationID=id,
         family=scrubbed_family,
         genus=scrubbed_genus) %>% 
  select(DatabaseID, DatasetID, ObervationID, family, genus, species_matched, CleanTraitValue, StdValue)
  

#read in try traits
try<-read.csv('C:\\Users\\mavolio2\\Dropbox\\sDiv_sCoRRE_shared\\CoRRE data\\trait data\\Raw TRY Data\\TRY Continuous data\\TRY_trait_data_continuous_long_Nov2021.csv')
