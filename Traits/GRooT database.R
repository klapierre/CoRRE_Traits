library(tidyverse)

#read in groot
groot<-read.csv("C:\\Users\\mavolio2\\Dropbox\\sDiv_sCoRRE_shared\\CoRRE data\\trait data\\GROOT\\GRooTFullVersion\\GRooTFullVersion.csv")

#filter traits we are interested in
grootsub<-groot %>% 
  filter(traitName=="Specific_root_lenght"|traitName=="Rooting_depth") %>% 
  mutate(species_matched=paste(genusTNRS, speciesTNRS, sep=' '))

grootsp<-grootsub %>% 
  select(species_matched) %>% 
  unique() %>% 
  mutate(groot="y")


# read in our species list
splist<-read.csv("C:\\Users\\mavolio2\\Dropbox\\CoRRE_database\\Data\\CompiledData\\Species_lists\\FullList_Nov2021.csv") %>% 
  filter(remove==0) %>% 
  select(species_matched) %>% 
  unique() %>% 
  mutate(corre="y")

merge<-grootsp %>% 
  left_join(splist) %>% 
  filter(corre=='y')

grootsubsp<-grootsub %>% 
  right_join(merge) %>% 
  select(GRooTID, originalID, familyTNRS, genusTNRS, speciesTNRS, species_matched, measurementProvenance, measurementTreatments, traitName, traitValue, errorRisk)

#read in try traits
try<-read.csv('C:\\Users\\mavolio2\\Dropbox\\sDiv_sCoRRE_shared\\CoRRE data\\trait data\\Raw TRY Data\\TRY Continuous data\\TRY_trait_data_continuous_long_Nov2021.csv')

##notes - it looks like all of groot is already in try, probably twice
