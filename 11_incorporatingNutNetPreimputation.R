# rm(list=ls()) clean up workspace
library(readxl)
library(WorldFlora)
library(data.table)
library(PerformanceAnalytics)
library(scales)
library(ggpubr)
library(tidyverse)


speciesList <- read.csv('https://pasta.lternet.edu/package/data/eml/edi/1533/4/5ebbc389897a6a65dd0865094a8d0ffd') %>% 
  select(family, species) %>% 
  rename(species_matched=species,
         Family=family)

##### TRY data #####
trySp <- read.delim('C:\\Users\\kjkomatsu\\OneDrive - UNCG\\manuscripts\\1_first author\\codominance\\data\\NutNet\\data\\TRY 2025\\TryAccSpecies.txt') %>% 
  rename(species_matched=AccSpeciesName) %>% 
  left_join(speciesList)

dat_1 <- fread("C:\\Users\\kjkomatsu\\Smithsonian Dropbox\\Kimberly Komatsu\\working groups\\CoRRE\\CoRRE_database\\Data\\OriginalData\\Traits\\TRY\\TRY_Traits_Downloaded_April2023.txt",sep = "\t",data.table = FALSE,stringsAsFactors = FALSE,strip.white = TRUE)

dat_2 <- fread("C:\\Users\\kjkomatsu\\OneDrive - UNCG\\manuscripts\\1_first author\\codominance\\data\\NutNet\\data\\TRY 2025\\42609_02072025181806\\42609.txt",sep = "\t",data.table = FALSE,stringsAsFactors = FALSE,strip.white = TRUE)

dat_3 <- fread("C:\\Users\\kjkomatsu\\OneDrive - UNCG\\manuscripts\\1_first author\\codominance\\data\\NutNet\\data\\TRY 2025\\42608_02072025173546\\42608.txt",sep = "\t",data.table = FALSE,stringsAsFactors = FALSE,strip.white = TRUE)

dat <- rbind(dat_1, dat_2, dat_3)

# merge NutNet with TRY
trysp <- dat %>% 
  select(AccSpeciesID, AccSpeciesName) %>% 
  rename(species_matched=AccSpeciesName) %>% 
  unique()

nutnet_key <- speciesList %>% 
  right_join(trysp) %>% 
  na.omit() %>% 
  select(species_matched, AccSpeciesID, Family) %>% 
  unique()

dat2 <- dat %>%
  right_join(nutnet_key)

# selecting desired continuous traits
dat3 <- dat2 %>%
  filter(TraitID %in% c(3106,  #vegetative height
                        3109, 3110, 3114, #leaf area
                        55, #leaf dry mass
                        47, #LDMC
                        3115, 3116, 3117, #SLA
                        14, #leaf N
                        1080, 614, #SRL
                        26)) %>% #seed dry mass
  # give names to numbers for the core traits
  mutate(CleanTraitName=ifelse(TraitID==14, 'leaf_N',
                               ifelse(TraitID==26, 'seed_dry_mass', 
                                      ifelse(TraitID==47, 'LDMC', 
                                             ifelse(TraitID==55, 'leaf_dry_mass', 
                                                    ifelse(TraitID==1080, 'SRL',
                                                           ifelse(TraitID==3106, 'plant_height_vegetative', 
                                                                  ifelse(TraitID==3116, 'SLA', 
                                                                         ifelse(TraitID==3110, 'leaf_area',
                                                                                TraitID))))))))) %>%
  filter(!is.na(StdValue)) %>% # drop observations without a trait value
  filter(is.na(OrigObsDataID)) %>% # drop known repeats in TRY 
  filter(UncertaintyName!="Range" & UncertaintyName!="Class range") # drop 1190 observations that were range estimates for leaf area, plant vegetative height and seed mass, which led to repeats in the data

# removing dead plants
health <- dat %>%
  select(DatasetID, DataID, ObsDataID, AccSpeciesID, AccSpeciesName, 
         TraitID, OriglName, TraitName, OrigValueStr, OrigUnitStr, 
         StdValue, UnitName, ErrorRisk) %>%
  filter(DataID==1961) %>%
  mutate(drop=ifelse(OrigValueStr=="Dead", 1, 0)) %>%
  select(ObsDataID, drop) %>%
  unique() #list of dead plants

healthy <- dat3 %>% #merge to drop observations on dead plants
  left_join(health) %>% 
  mutate(drop=ifelse(is.na(drop), 0, drop)) %>% 
  filter(drop!=1) %>%  #no overlap in dataset
  select(-drop)

# removing trees that are not seedlings -- based on data identified specifically as either mature or seedling
corre_treesp <- read.csv("C:\\Users\\kjkomatsu\\Smithsonian Dropbox\\Kimberly Komatsu\\working groups\\CoRRE\\CoRRE_database\\Data\\CompiledData\\Species_lists\\species_families_trees_2021.csv") %>%
  mutate(AccSpeciesName=species_matched) #read in which species are trees

gex_treesp <- read.csv("C:\\Users\\kjkomatsu\\Smithsonian Dropbox\\Kimberly Komatsu\\working groups\\CoRRE\\CoRRE_database\\Data\\OriginalData\\Traits\\GEx_species_tree_complete.csv") %>% 
  filter(tree.non.tree %in% c("tree", "non-tree")) %>% 
  select(species_matched, family, tree.non.tree) %>% 
  separate(species_matched, into = c("genus", "species", "other"), sep=" ") %>% 
  filter(species!="NA") %>% 
  mutate(species_matched=paste(genus, species, sep=" ")) %>% 
  select(species_matched, family, tree.non.tree) %>% 
  unique() %>% 
  left_join(speciesList) %>% 
  unique()

treesp <- corre_treesp %>% 
  bind_rows(gex_treesp) %>% 
  unique() %>% 
  bind_rows(read.csv("C:\\Users\\kjkomatsu\\OneDrive - UNCG\\manuscripts\\1_first author\\codominance\\data\\NutNet\\nutnet_species_families_trees_2024.csv")) #read in which species are trees

tree <- dat %>% #get list of tree observations that were made on seedlings
  select(DatasetID, DataID, ObsDataID, AccSpeciesID, AccSpeciesName, TraitID, OriglName, TraitName,
         OrigValueStr, OrigUnitStr, StdValue, UnitName, ErrorRisk) %>%
  filter(DataID==413) %>%
  right_join(treesp) %>%
  filter(tree.non.tree=="tree") %>%
  mutate(drop=ifelse(OrigValueStr=="seedlings"|OrigUnitStr==0|OrigValueStr=="seedling"|OrigValueStr=="Seedling (0 - 1 y)"|OrigValueStr=="seedlings, 1st year",  0, 1)) %>%
  select(ObsDataID, drop) %>%
  unique()

nontree <- dat3 %>% #merge to drop tree observations that are not seedlings
  left_join(tree) %>%
  mutate(drop=ifelse(is.na(drop), 0, drop)) %>%
  filter(drop!=1) %>%  #no overlap in dataset
  select(-drop)

# Removing plants that were not measured in natural conditions
setting <- dat %>% #get list of observations that were not in natural settings
  select(DatasetID, DataID, ObsDataID, AccSpeciesID, AccSpeciesName, 
         TraitID, OriglName, TraitName, OrigValueStr, OrigUnitStr, 
         StdValue, UnitName, ErrorRisk) %>%
  filter(DataID==327) %>%
  mutate(drop=ifelse(OrigValueStr %in% c("Canadian High Arctic Research Station", "Control Plot", "field","Field", "Field (CG)", "Field (NE)", "field experiment", "Field Experiment", "Field plants", "forest stand", "Forest trees", "Forest understorey", "Fully open overstory 90 days, seedling", "Fully open overstory, seedling","Fully sunlit - Natural environment","High desert", "in situ", "In situ", "La Selva Biological Station", "meadows (M) and pastures (P) on south east to south west exposed slopes", "Montane meadow", "Mosses in forest", "nat env", "natural", "Natural", "natural-environment", "natural env", "natural envireonment", "natural enviroment", "Natural Enviroment", "natural environment", "Natural environment", "Natural Environment", "natural environment, high regional N and S deposition","natural environment, no warming, preccipitation ambient", "natural environment, sun exposed", "Natural Envrionment", "Natural Forest", "natural forest environment", "natural vegetation", "Natural Vegetation", "Natural Vegetation", "natural vegetation, but not top canopy", "natural wetland environment", "natural wetlands (field conditions)", "Natural/C", "natural_environment", "none", "None", "North facing slope", "Shade - Natural environment","South facing slope", "Trees in field"), 0, 1)) %>%
  select(ObsDataID, drop) %>%
  unique()

natural <- dat3 %>% # merge to drop observations in non-natural settings
  left_join(setting) %>% 
  mutate(drop=ifelse(is.na(drop), 0, drop)) %>% 
  filter(drop!=1) %>%  #no overlap in dataset
  select(-drop)

# Drop traits for trees -- based on whether or not the species is a tree (for all that were not designated as seedling, above)
splist <- speciesList %>%
  select(species_matched) %>%
  unique() %>%
  left_join(treesp) %>% 
  mutate(tree.non.tree=ifelse(species_matched=='Juniperus horizontalis', 'non-tree', 
                              ifelse(is.na(tree.non.tree), 'non-tree', tree.non.tree))) %>% 
  select(species_matched, tree.non.tree) %>%
  unique()

cont_traits <- dat3 %>%
  left_join(splist) %>%
  filter(tree.non.tree!='tree') %>%  # drops 91,212 observations
  select(-tree.non.tree)

# add taxonomic information for each species
cont_traits2 <- cont_traits %>%
  select(DatasetID, ObservationID, Family, species_matched, CleanTraitName, StdValue, ErrorRisk, Reference, UnitName, OriglName, TraitID) %>%
  separate(remove = F, species_matched, into = c("genus", "species"), sep=" ") %>%
  select(-species)

# removing trait outliers based on TRY's Error Risk designation
cont_traits3a <- cont_traits2 %>%
  select(DatasetID, ObservationID, Family, genus, species_matched, CleanTraitName, StdValue, ErrorRisk, Reference, UnitName, OriglName, TraitID) %>%
  mutate(ErrorRisk2=ifelse(is.na(ErrorRisk), 0, ErrorRisk)) %>%
  filter(ErrorRisk2<3) %>% #removes all observations that are greater than 3 sd from full database mean
  select(-ErrorRisk, -ErrorRisk2) %>% 
  filter(StdValue>0) %>% #removing negative and 0 values 
  rename(family=Family)


#### Removing problem data where there are replicates for ObservationID ####
cont_traits3 <- cont_traits3a %>% 
  mutate(remove=ifelse(TraitID==3116 & UnitName=='', 1, 
                ifelse(TraitID==3109 & OriglName=="Area (dry) cm2", 1, 
                ifelse(TraitID==3109 & OriglName=="Dry.area.cm2", 1,
                ifelse(TraitID==3117 & OriglName=="LMA", 1,
                ifelse(TraitID==614 & OriglName=="Min_Specific root length (SRL)", 1,
                ifelse(TraitID==614 & OriglName=="Max_Specific root length (SRL)", 1, 
                ifelse(TraitID==47 & OriglName=="LDMC_min", 1, 
                ifelse(TraitID==47 & OriglName=="LDMC_max", 1, 
                ifelse(TraitID==47 & OriglName=="WCf", 1,
                ifelse(TraitID==47 & OriglName=="Leaf dry matter concentration predicted from NIRS", 1,
                ifelse(TraitID==3110 & OriglName=="Leaf_area_min", 1, 
                ifelse(TraitID==3110 & OriglName=="Leaf_area_max", 1, 
                ifelse(TraitID==55 & OriglName=="WLfMass", 1, 
                ifelse(TraitID==55 & OriglName=="Mass_senesced_leaf", 1,
                ifelse(TraitID==14 & OriglName=="N amount%", 1, 
                ifelse(TraitID==14 & OriglName=="N_senesced_leaf", 1,
                ifelse(TraitID==3106 & OriglName=="Flowering plant height, heighest leaf elongated", 1, 
                ifelse(TraitID==3106 & OriglName=="Flowering plant height, heighest leaf not elongated", 1, 
                ifelse(TraitID==3106 & OriglName=="Height at 20 Years", 1, 
                ifelse(TraitID==3106 & OriglName=="MaximumHeightMinM", 1, 
                ifelse(TraitID==3106 & OriglName=="MaximumHeightExtremeM", 1, 
                ifelse(TraitID==3106 & OriglName=="Maximum Height", 1, 
                ifelse(TraitID==3106 & OriglName=="Plant_height_vegetative_min", 1, 
                ifelse(TraitID==3106 & OriglName=="Plant_height_vegetative_mean", 1,
                ifelse(TraitID==3106 & OriglName=="Length (aquatic)", 1, 
                ifelse(TraitID==3106 & OriglName=="Height (seedling)", 1,
                ifelse(TraitID==3106 & OriglName=="Height max (m)", 1,
                ifelse(TraitID==3106 & OriglName=="strechedPlantHight", 1,
                ifelse(TraitID==3106 & OriglName=="MaximumHeightM", 1,
                ifelse(TraitID==26 & OriglName=="original seed mass (mg)", 1,
                ifelse(TraitID==26 & OriglName=="OriginalSeedMassMean", 1,
                ifelse(TraitID==3116 & OriglName=="SLA_min", 1, 
                ifelse(TraitID==3116 & OriglName=="SLA_max", 1, 
                ifelse(TraitID==1080 & OriglName=="SRL roots >2mm diam (cm/g)", 1,
                ifelse(TraitID==26 & OriglName=="SeedMassMax" | OriglName=="SeedMassMin",1, 0)))))))))))))))))))))))))))))))))))) %>% 
  filter(remove==0) %>%
  select(-remove)


#### Problem Datasets -- duplicate entries ####

# Problem: We investigated many repeats of similar plant vegetative height, LDMC, and water content values. We think this data was probably methodologically linked in some way to give these similar values (e.g., poor resolution of measurements). 
# Solution: We think the values are real (despite the methodological linkage), not duplicated data. Therefore, we are keeping all of this data.

# Problem: Three observations per plant (same ObservationID). Probably three leaves per plant, but no way to link leaves. Solution: Average.
d453 <- cont_traits3 %>%
  filter(DatasetID==453) %>%
  group_by(DatasetID, ObservationID, species_matched, CleanTraitName, family, genus, Reference) %>%
  summarise(StdValue=mean(StdValue)) %>% 
  ungroup()

# Problem: Two height values per plant (maybe temporal observations).
# Solution: Taking largest value.
d428 <- cont_traits3 %>% 
  filter(DatasetID==428 & CleanTraitName=="plant_height_vegetative") %>%
  group_by(DatasetID, ObservationID, species_matched, CleanTraitName, family, genus, Reference) %>%
  summarise(StdValue=max(StdValue)) %>% 
  ungroup()

# Problem: Many observations with exactly repeated values for each trait*species. 
# Solution: Take the average and make them all unique observations (loses linking of data on same individuals, but this seems better than keeping so many repeats).
d415 <- cont_traits3 %>% 
  filter(DatasetID==415) %>% 
  group_by(DatasetID, species_matched, CleanTraitName, family, genus, Reference)%>%
  summarise(StdValue=mean(StdValue)) %>% 
  ungroup() %>% 
  mutate(ObservationID=row_number())

# Problem: Many doubles of values for trait*species with no clear pattern. 
# Solution: Take the average and make them all unique observations (loses linking of data on same individuals, but this seems better than keeping so many repeats).
d25 <- cont_traits3 %>% 
  filter(DatasetID==25) %>% 
  group_by(DatasetID, species_matched, CleanTraitName, family, genus, Reference) %>%
  summarise(StdValue=mean(StdValue)) %>% 
  ungroup() %>% 
  mutate(ObservationID=row_number())

# Problem: Dataset has lots of repeated data for some traits*species because the same observation ID is attributed to three papers,  Cornelissen et al. 1996, Cornelissen 2004, and Quested et al. 2003. 
# Repeats are of Cornelissen 2004 and Quested 2003
# Solution: Merge the datasets based on species name, trait, and trait value. For duplicates (~21), attribute to Quested 2004.
d1.1 <- cont_traits3 %>% 
  filter(DatasetID==1) 

unique(d1.1$Reference)
#this is one unique observation, don't need to worry about this.
d1_corn96<-d1.1 %>%
  filter(Reference=='Cornelissen, J. H. C., P. C. Diez, and R. Hunt. 1996. Seedling growth, allocation and leaf attributes in a wide range of woody plant species and types. Journal of Ecology 84:755-765.')
d1_corn04<-d1.1 %>% 
  filter(Reference=='Cornelissen, J. H. C., H. M. Quested, D. Gwynn-Jones, R. S. P. Van Logtestijn, M. A. H. De Beus, A. Kondratchuk, T. V. Callaghan, and R. Aerts. 2004. Leaf digestibility and litter decomposability are related in a wide range of subarctic plant species and types. Functional Ecology 18:779-786.')
d1_Ques<-d1.1 %>% 
  filter(Reference=='Quested, H. M., J. H. C. Cornelissen, M. C. Press, T. V. Callaghan, R. Aerts, F. Trosien, P. Riemann, D. Gwynn-Jones, A. Kondratchuk, and S. E. Jonasson. 2003. Decomposition of sub-arctic plants with differing nitrogen economies: A functional role for hemiparasites. Ecology 84:3209-3221.')

#Join these by all species and values to see the overlap
d1 <- d1_Ques %>% 
  full_join(d1_corn04, by=c('DatasetID', "family", "genus" , "species_matched","CleanTraitName" , "StdValue" )) %>% 
  mutate(ObservationID=ifelse(is.na(ObservationID.y), ObservationID.x, ifelse(is.na(ObservationID.x),ObservationID.y, ObservationID.x))) %>% 
  mutate(Reference=ifelse(is.na(Reference.y), Reference.x, ifelse(is.na(Reference.x),Reference.y, Reference.x))) %>% 
  select(DatasetID, ObservationID, family, genus, species_matched, CleanTraitName, StdValue, Reference) %>% 
  bind_rows(d1_corn96)

# Problem: has repeated individuals within the dataset. 
# Solution: Take the average for each individual for each trait. For each species, find if there is repeated data for all traits collected on an individual.
d412 <- cont_traits3 %>% 
  filter(DatasetID==412) %>% 
  group_by(DatasetID, ObservationID, family, genus, species_matched, CleanTraitName, Reference) %>% 
  summarize(StdValue=mean(StdValue)) %>% 
  ungroup()

# Problem: Trait 614 (measure of SRL) three values for a few observations. Solution: just average across traits
d339 <- cont_traits3 %>% 
  filter(DatasetID==339) %>% 
  group_by(DatasetID, ObservationID, family, genus, species_matched, CleanTraitName, Reference) %>% 
  summarize(StdValue=mean(StdValue)) %>% 
  ungroup()

# Problem: Plant vegetative height is measured 5x for for each plant (maybe temporal observations). 
# Solution: keep the maximum height. Also only has one value for roots
d201 <- cont_traits3 %>% 
  filter(DatasetID==201) %>% 
  group_by(DatasetID, ObservationID, family, genus, species_matched, CleanTraitName, Reference) %>% 
  summarize(StdValue=max(StdValue)) %>% 
  ungroup()

# Problem: for some individuals plant height is measured 2x (maybe over time). 
# Solution: Take the max of all traits as all other traits only in there 1x.
d45 <- cont_traits3 %>% 
  filter(DatasetID==45) %>% 
  group_by(DatasetID, ObservationID, family, genus, species_matched, CleanTraitName, Reference) %>% 
  summarize(StdValue=max(StdValue)) %>% 
  ungroup()

# Problem: for some individuals there are 2 measurements of plant height (maybe measured over time). 
# Solution: Take the max of all traits as all other traits only in there 1x.
d299 <- cont_traits3 %>% 
  filter(DatasetID==299) %>% 
  group_by(DatasetID, ObservationID, family, genus, species_matched, CleanTraitName, Reference) %>% 
  summarize(StdValue=max(StdValue)) %>% 
  ungroup()

# Problem: there are 2 measurements of leaf dry mass for all plants. 
# Solution: Take the mean of all traits as all other traits only in there 1x.
d477 <- cont_traits3 %>% 
  filter(DatasetID==477) %>% 
  group_by(DatasetID, ObservationID, family, genus, species_matched, CleanTraitName, Reference) %>% 
  summarize(StdValue=mean(StdValue)) %>% 
  ungroup()

# Problem: there are 2 measurements of plant height for some plants. 
# Solution: Take the max of all traits as all other traits only in there 1x.
d520 <- cont_traits3 %>% 
  filter(DatasetID==520) %>% 
  group_by(DatasetID, ObservationID, family, genus, species_matched, CleanTraitName, Reference) %>% 
  summarize(StdValue=max(StdValue)) %>% 
  ungroup()

# Problem: there are 2 measurements of plant height for some plants. 
# Solution: Take the max of all traits as all other traits only in there 1x.
d655 <- cont_traits3 %>% 
  filter(DatasetID==655) %>% 
  group_by(DatasetID, ObservationID, family, genus, species_matched, CleanTraitName, Reference) %>% 
  summarize(StdValue=max(StdValue)) %>% 
  ungroup()

# Problem: there are multiple measurements of 3114 (a measure of leaf area) for many individuals. 
# Solution: Take the mean of all traits as all other traits are in there 1x.
d486 <- cont_traits3 %>% 
  filter(DatasetID==486) %>% 
  group_by(DatasetID, ObservationID, family, genus, species_matched, CleanTraitName, Reference) %>% 
  summarize(StdValue=mean(StdValue)) %>% 
  ungroup()

#dataset 468 has many many repeats (50+) for plant height and not other measurements. This will add lots of missing data and not much information.
# Solution: Only keep 10 repeats, which is likely real data (measurement precision problems again), but not meaningful.
d468sub <- cont_traits3 %>% 
  filter(DatasetID==468) %>% 
  group_by(DatasetID, family, genus, species_matched, CleanTraitName, StdValue, Reference) %>% 
  summarize(n=length(StdValue)) %>% 
  filter(n>10) %>% 
  ungroup()

d468todrop <- d468sub %>% 
  filter(DatasetID==468) %>% 
  select(species_matched, CleanTraitName, StdValue, Reference) %>%
  unique() %>% 
  mutate(drop=1)

d468sub1 <- cont_traits3 %>% 
  filter(DatasetID==468) %>% 
  right_join(d468sub) %>% 
  group_by(DatasetID, family, genus, species_matched, CleanTraitName, StdValue, Reference) %>% 
  mutate(number=row_number()) %>% 
  filter(number< max(10, length(StdValue)*0.05)) %>% 
  select(-n, -number) %>% 
  ungroup()

d468 <- cont_traits3 %>% 
  filter(DatasetID==468) %>% 
  left_join(d468todrop) %>% 
  filter(is.na(drop)) %>% 
  select(-drop) %>% 
  bind_rows(d468sub1)

# Dropping problem datasets and appending clean versions.
TRYtraits <- cont_traits3 %>%
  filter(!(DatasetID %in% c(453, 415, 25, 1, 412, 339, 201, 45, 299, 477, 520, 655, 486, 468, 428))) %>%
  bind_rows(d339) %>% 
  bind_rows(d453) %>%
  bind_rows(d428) %>%
  bind_rows(d415) %>%
  bind_rows(d25) %>%
  bind_rows(d1) %>%
  bind_rows(d412) %>%
  bind_rows(d201) %>%
  bind_rows(d45) %>%
  bind_rows(d299) %>%
  bind_rows(d477) %>%
  bind_rows(d520) %>%
  bind_rows(d655) %>%
  bind_rows(d486) %>%
  bind_rows(d468)


# finds repeats; everything left is probably real, just measurement imprecision
repeats <- TRYtraits %>%
  group_by(species_matched, CleanTraitName, StdValue) %>%
  summarize(n=length(StdValue)) %>%
  ungroup() %>%
  filter(n>2)

# write.csv(TRYtraits, "C:\\Users\\kjkomatsu\\OneDrive - UNCG\\manuscripts\\1_first author\\codominance\\data\\allTraits\\all_TRY traits_20250701.csv", row.names=F)


##### BIEN traits #####

#gather data from library

library(BIEN)

sp.vector <- unique(speciesList$species_matched)

bienData <- BIEN_trait_species(species=sp.vector) %>% 
  rename(species_matched=scrubbed_species_binomial) %>%  
  right_join(nutnetSpp) %>% 
  # subset to data that we want
  filter(trait_name %in% c('leaf area', 'leaf area per dry mass', 'leaf dry mass', 
                           'leaf dry mass per leaf fresh mass', 'seed mass',
                           'leaf nitrogen content per leaf dry mass')) %>% 
  mutate(trait_value=as.numeric(trait_value)) %>% 
  # standardize units to fit TRY
  mutate(clean_trait_value=ifelse(trait_name=='leaf dry mass per leaf fresh mass', trait_value/1000, #LDMC (BIEN mg/g   TRY g/g)
                                  ifelse(trait_name=='leaf area per leaf dry mass', trait_value*1000, #SLA (BIEN m2/kg   TRY mm2/g)
                                         ifelse(trait_name=='leaf dry mass', trait_value*1000, #leaf dry mass (BIEN g   TRY mg)
                                                trait_value)))) %>% 
  # remove data that was not from a naturally growing plant
  filter(method!='laboratory/greenhouse/garden experiment',
         trait_value!=0) %>% 
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

# checking for duplicate data
test <- bienData %>% 
  group_by(species_matched, CleanTraitName, StdValue) %>% 
  summarize(n=length(StdValue)) %>% 
  ungroup() %>% 
  filter(n>2)

# change BIEN trait names to fit TRY trait names
bienData$CleanTraitName <- recode(bienData$CleanTraitName, 
                                  'leaf area'='leaf_area',
                                  'leaf area per dry mass'='SLA',
                                  'leaf dry mass'='leaf_dry_mass',
                                  'leaf dry mass per leaf fresh mass'='LDMC',
                                  'leaf nitrogen content per leaf dry mass'='leaf_N',
                                  'seed mass'='seed_dry_mass')


# unify with other dataset columns
BIENtraits <- bienData %>% 
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
  select(-drop) %>% 
  left_join(nutnetSpp) %>%
  mutate(Reference=DatasetID) %>% 
  select(DatabaseID, DatasetID, ObservationID, Family, species_matched, genus, CleanTraitName, StdValue, Reference) %>% 
  filter(StdValue>0)

# write.csv(BIENtraits, 'C:\\Users\\kjkomatsu\\OneDrive - UNCG\\manuscripts\\1_first author\\codominance\\data\\allTraits\\all_BIEN traits_20250701.csv', row.names=F)


##### AusTraits #####
library(austraits)

austraits <- load_austraits(version = "6.0.0", path = "austraits")

traits <- summarise_austraits(austraits, "trait_name")

#doesn't have seed number, stem specific density, rooting depth
data <- extract_trait(austraits, c('leaf_area',
                                   'leaf_dry_mass', 
                                   'leaf_dry_matter_content', 
                                   'leaf_mass_per_area', #need to inverse this
                                   'leaf_N_per_dry_mass', 
                                   'plant_height', 
                                   'root_specific_root_length', 
                                   'seed_dry_mass'))


traitData <- data$traits %>%
  mutate(DatabaseID='AusTraits') %>%
  rename(DatasetID=dataset_id,
         ObservationID=observation_id,
         species_matched=taxon_name,
         StdValue=value) %>% 
  mutate(StdValue=ifelse(trait_name=='leaf_mass_per_area', (1/StdValue)*1000, 
                         ifelse(trait_name=='root_specific_root_length', (StdValue*100), StdValue)),
         trait_name=ifelse(trait_name=='leaf_mass_per_area', 'specific_leaf_area', trait_name))

species <- nutnetSpp %>%  #species names are standardized
  left_join(treesp) %>% 
  filter(tree.non.tree != "tree") %>% #Remove trees
  separate(species_matched, into=c('genus', 'species', 'subspp'), sep=' ') %>% 
  filter(species!='sp.') %>% 
  unite(col='species_matched', genus:species, sep=' ', remove=T) %>% 
  select(species_matched) %>% 
  unique()

AusTraits <- traitData %>%
  right_join(species) %>%
  group_by(DatabaseID, DatasetID, ObservationID, species_matched, trait_name) %>% 
  summarize(StdValue=max(StdValue)) %>% 
  ungroup() %>% 
  mutate(drop=ifelse(trait_name=='plant_height' & StdValue>40, 1,
                     ifelse(trait_name=='seed_dry_mass' & StdValue>600, 1, 0))) %>% 
  filter(drop==0) %>% 
  select(-drop)

AusTraits$CleanTraitName <- recode(AusTraits$trait_name, 
                                   'leaf_dry_matter_content'='LDMC', 
                                   'specific_leaf_area'='SLA', 
                                   'leaf_N_per_dry_mass'='leaf_N', 
                                   'plant_height'='plant_height_vegetative', 
                                   'root_specific_root_length'='SRL') 
AusTraits <- AusTraits %>%
  left_join(nutnetSpp) %>%
  mutate(species_matched2=species_matched) %>%
  separate(species_matched2, into=c('genus', 'species')) %>%
  mutate(DatabaseID='AusTraits',
         Reference=DatasetID) %>%
  select(DatabaseID, DatasetID, ObservationID, Family, species_matched, genus, CleanTraitName, StdValue, Reference) %>% 
  filter(StdValue>0)

# write.csv(AusTraits, 'NutNet_AusTraits_20240711.csv', row.names=F)

##### TiP Leaf #####
tipTraits <- read_xlsx('C:\\Users\\kjkomatsu\\Smithsonian Dropbox\\Kimberly Komatsu\\working groups\\CoRRE\\CoRRE_database\\Data\\OriginalData\\Traits\\TiP_leaf\\The TiP-Leaf dataset.xlsx', sheet='plant traits') %>% 
  dplyr::rename(species_matched=Species) %>%
  filter(species_matched!='/') %>% 
  mutate(ObservationID=row_number(.)) %>% 
  mutate(DatasetID='1', DatabaseID='TIPleaf') %>% 
  left_join(nutnetSpp) %>% 
  filter(!is.na(Family)) %>% 
  mutate(LCC=as.numeric(ifelse(LCC=='/', NA, LCC)),
         LNC=as.numeric(ifelse(LNC=='/', NA, LNC)),
         LPC=as.numeric(ifelse(LPC=='/', NA, LPC)),
         SLA=SLA/10) %>% #unit conversion to TRY standards: cm2/g to mm2/mg 
  select(DatabaseID, DatasetID, ObservationID, species_matched, DW, LDMC, LA, SLA, LNC) %>% 
  pivot_longer(DW:LNC, names_to='trait_name', values_to='StdValue') %>% 
  unique()

tipTraits$CleanTraitName <- recode(tipTraits$trait_name, 
                                   'DW'='leaf_dry_mass',
                                   'LA'='leaf_area',
                                   'LNC'='leaf_N')

tipTraits <- tipTraits %>% 
  separate(col=species_matched, into=c('genus', 'species'), sep=' ', remove=F) %>% 
  left_join(nutnetSpp) %>% 
  mutate(Reference='TipLeaf Database') %>% 
  select(DatabaseID, DatasetID, ObservationID, Family, genus, species_matched, CleanTraitName, StdValue, Reference) %>% 
  filter(StdValue>0)

# write.csv(tipTraits, 'NutNet_TiP Leaf traits_20250701.csv', row.names=F)

##### China Plant Trait Database 2 #####
spList <- read.csv('C:\\Users\\kjkomatsu\\Smithsonian Dropbox\\Kimberly Komatsu\\working groups\\CoRRE\\CoRRE_database\\Data\\OriginalData\\Traits\\ChinaPlant2\\Species translations.csv') %>% 
  unite(col='species_matched', ACCEPTED.GENUS:ACCEPTED.SPECIES, sep=' ') %>% 
  select(species_matched, Site.ID, SAMPLE.ID) %>% 
  left_join(nutnetSpp)

chem <- read.csv("C:\\Users\\kjkomatsu\\Smithsonian Dropbox\\Kimberly Komatsu\\working groups\\CoRRE\\CoRRE_database\\Data\\OriginalData\\Traits\\ChinaPlant2\\Chemical traits.csv") %>% 
  filter(flagged=="") %>% 
  mutate(leaf_area=Average.LA*1000000) %>% #unit conversion to TRY standards: m2 to mm2
  mutate(LDMC=LDMC/1000) %>% #unit conversion to TRY standards: mg/g to g/g
  select(-LMA,-Narea, -Parea, -Karea, -d13C.12C, -d15N.14N, -flagged, -Average.LA) %>% 
  rename(leaf_C=Cmass, 
         leaf_N=Nmass,
         leaf_P=Pmass, 
         leaf_K=Kmass) %>% 
  pivot_longer(SLA:leaf_area, names_to="CleanTraitName", values_to="StdValue") %>% 
  right_join(spList) %>% 
  na.omit()

photo <- read.csv("C:\\Users\\kjkomatsu\\Smithsonian Dropbox\\Kimberly Komatsu\\working groups\\CoRRE\\CoRRE_database\\Data\\OriginalData\\Traits\\ChinaPlant2\\Photosynthetic traits.csv") %>% 
  filter(flagged=="") %>% 
  select(SAMPLE.ID, Vcmax, Jmax) %>% 
  rename(Vc_max=Vcmax,
         J_max=Jmax) %>% 
  pivot_longer(Vc_max:J_max, names_to='CleanTraitName', values_to='StdValue') %>% 
  right_join(spList) %>% 
  na.omit()

#bind together
CPTDtraits <- rbind(chem, photo) %>% 
  mutate(DatabaseID='CPTD2') %>% 
  rename(DatasetID=Site.ID,
         ObservationID=SAMPLE.ID) %>% 
  filter(CleanTraitName %in% c('LDMC', 'leaf_area', 'leaf_N', 'SLA')) %>% 
  separate(col=species_matched, into=c('genus', 'species'), sep=' ', remove=F) %>% 
  mutate(Reference='China Plant Trait Database 2') %>% 
  select(DatabaseID, DatasetID, ObservationID, Family, genus, species_matched, CleanTraitName, StdValue, Reference) %>% 
  filter(StdValue>0)


##### Bind all trait data #####
allTraits <- rbind(TRYtraits, AusTraits, BIENtraits, tipTraits, CPTDtraits) %>% 
  mutate(ReferenceID=paste(DatabaseID, DatasetID, sep='_')) %>% 
  select(DatabaseID, DatasetID, ObservationID, Family, genus, species_matched, 
         CleanTraitName, StdValue, Reference) %>% 
  group_by(DatabaseID, DatasetID, ObservationID, Family, genus, species_matched, 
           CleanTraitName, Reference) %>% 
  summarise(StdValue=mean(StdValue)) %>% 
  ungroup()

allTraits_wide <- allTraits %>% 
  pivot_wider(names_from = CleanTraitName, values_from = StdValue, values_fill=NA)

ntraits <- length(unique(allTraits$CleanTraitName))
miss <- sum(is.na(allTraits_wide))
total <- nrow(allTraits_wide)*ntraits
miss/total*100
#missing 89.05% of data

spnum <- length(unique(allTraits_wide$species_matched))
famnum <- length(unique(allTraits_wide$Family))

label <- allTraits %>%
  group_by(CleanTraitName, DatabaseID) %>%
  summarise(length=length(StdValue)) %>%
  ungroup() %>%
  group_by(CleanTraitName) %>%
  mutate(length2=sum(length)) %>%
  ungroup() %>%
  pivot_longer(cols=length:length2, names_to='name', values_to='length') %>%
  mutate(DatabaseID=ifelse(name=='length2', 'total', DatabaseID)) %>%
  unique() %>%
  mutate(percent=round((length/253224)*100, 1)) %>% 
  mutate(CleanTraitName2=ifelse(CleanTraitName==3109, 'Leaf Area (leaflet, -petiole)',
                                ifelse(CleanTraitName==3114, 'Leaf Area (undefined, undefined)',
                                       ifelse(CleanTraitName=='leaf_area', 'Leaf Area (leaf, +petiole)',
                                              ifelse(CleanTraitName==3115, 'Specific Leaf Area (-petiole)',
                                                     ifelse(CleanTraitName==3117, 'Specific Leaf Area (undefined)',
                                                            ifelse(CleanTraitName=='SLA', 'Specific Leaf Area (+petiole)', 
                                                                   ifelse(CleanTraitName=='SRL', 'Specific Root Length (all root)',
                                                                          ifelse(CleanTraitName==614, 'Specific Root Length (fine root)', 
                                                                                 ifelse(CleanTraitName=='leaf_N', 'Leaf N Content',
                                                                                        ifelse(CleanTraitName=='plant_height_vegetative', 'Plant Vegetative Height',
                                                                                               ifelse(CleanTraitName=='seed_dry_mass', 'Seed Dry Mass',
                                                                                                      ifelse(CleanTraitName=='leaf_dry_mass', 'Leaf Dry Mass',
                                                                                                             ifelse(CleanTraitName=='LDMC', 'Leaf Dry Matter Content',
                                                                                                                    CleanTraitName))))))))))))))

label$CleanTraitName2 = factor(label$CleanTraitName2, levels=c('Leaf Area (leaf, +petiole)', 'Leaf Area (leaflet, -petiole)', 'Leaf Area (undefined, undefined)', 'Leaf Dry Mass', 'Leaf Dry Matter Content', 'Specific Leaf Area (+petiole)', 'Specific Leaf Area (-petiole)', 'Specific Leaf Area (undefined)', 'Leaf N Content', 'Plant Vegetative Height', 'Specific Root Length (all root)', 'Specific Root Length (fine root)', 'Seed Dry Mass'))

# How many observations do we have for each trait across our database?
ggplot(data=label, aes(x=DatabaseID, y=length, label=round(percent,1), fill=DatabaseID)) +
  geom_bar(stat='identity', position=position_dodge()) +
  geom_hline(yintercept=253224*.2) + # 20% of observations missing any given trait
  geom_hline(yintercept=253224*.1, color='red') + # 10% of observations missing any given trait
  geom_text(vjust = -0.25, size=6) +
  facet_wrap(~CleanTraitName2, ncol=5, labeller=label_wrap_gen(width=25)) +
  scale_y_continuous() +
  scale_x_discrete(breaks=c("AusTraits", "BIEN", "CPTD2", "TIPleaf", "TRY", "total"),
                   limits=c("AusTraits", "BIEN", "CPTD2", "TIPleaf", "TRY", "total"),
                   labels=c("Au", "BN", "C2", "TP", "TY", 'all')) +
  scale_fill_manual(values=c('#4E3686', '#5DA4D9', '#80D87F', '#FED23F','darkgrey', '#EE724C'))+
  theme(strip.text.x = element_text(size = 18),
        axis.title.x=element_text(size=24, vjust=-0.35, margin=margin(t=15)), axis.text.x=element_text(size=22),
        axis.title.y=element_text(size=24, angle=90, vjust=0.5, margin=margin(r=15)), axis.text.y=element_text(size=22),
        legend.position='none') +
  ylab('Number of Observations') + xlab('Database ID')
# ggsave('x.png', width=17, height=19, units='in', dpi=300, bg='white')

# Are there any outlier datasets for each trait?
ggplot(data=allTraits, aes(x=DatabaseID, y=StdValue)) +
  scale_y_log10() + # note log axis!
  geom_jitter(aes(color=DatabaseID)) +
  geom_boxplot(color='black', alpha=0) +
  facet_wrap(~CleanTraitName, scales='free_y', ncol=4) +
  scale_x_discrete(breaks=c("AusTraits", "BIEN", "CPTD2", "TIPleaf", "TRY"),
                   labels=c("A", "B", "C", "TIP", "TY")) +
  scale_color_manual(values=c('#4E3686', '#5DA4D9', '#80D87F', '#FED23F', '#EE724C')) +
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        legend.position='top') 
# ggsave('x.png', width=7.5, height=10, units='in', dpi=300, bg='white')


# Transpose to wide format for gap filling.
talltraits <- allTraits %>% 
  group_by(DatabaseID, DatasetID, ObservationID, Family, genus, species_matched) %>%
  pivot_wider(names_from=CleanTraitName, values_from=StdValue, values_fill=NA) %>% 
  ungroup()

# write.csv(allTraits, 'nutnet_trait database_combo_continuous_20250701_long.csv', row.names = F)

# write.csv(talltraits, 'nutnet_trait database_combo_continuous_20250701.csv', row.names = F)


##### Impute Traits #####

library(devtools)
# install_github("fisw10/BHPMF")
library(BHPMF)
library(plyr)
library(abind)
library(mice)

traits <- read.table('nutnet_trait database_combo_continuous_20240710.csv', row.names=NULL, sep=",", header=T) %>% 
  mutate(family=Family) %>% 
  select(-Family)

# traits <- traits[1:100000,]

#remove trait values with > 4 SD:
spp <- unique(traits$species_matched) #get vector with species names

out<-NULL
for(i in 1:length(spp)) { #loop for each species
  print(i/length(spp))
  sub <- traits[traits$species_matched %in% spp[i],]
  for(j in 7:ncol(traits)) { #loop for each trait (column)
    mean.sub <- mean(sub[,j], na.rm=T)
    sd.sub <- sd(sub[,j], na.rm=T)
    
    lim_up <- mean.sub + 4*sd.sub
    lim_dn <- mean.sub - 4*sd.sub
    
    sub[,j][sub[,j] > lim_up] <- NA
    sub[,j][sub[,j] < lim_dn] <- NA
  }
  out <- rbind(out, sub)
}

#create hierarchy file:
hierarchy.info <- subset(traits, select = c(ObservationID, species_matched, genus, family))
names(hierarchy.info) <- c("plant_id","species", "genus", "family")
hierarchy.info$plant_id <- 1:nrow(hierarchy.info)

#some genera are assigned to different families. Need to be unified:
hierarchy.info$family[hierarchy.info$genus=="Lancea"] <- "Mazaceae"
hierarchy.info$family[hierarchy.info$genus=="Toxicoscordion"] <- "Melanthiaceae"
hierarchy.info$family[hierarchy.info$genus=="Heliotropium"] <- "Boraginaceae"
hierarchy.info$family[hierarchy.info$genus=="Phacelia"] <- "Boraginaceae"
hierarchy.info$family[hierarchy.info$genus=="Pholistoma"] <- "Boraginaceae"

# test <- hierarchy.info %>%
#   select(family, genus) %>% 
#   unique(.) %>% 
#   group_by(genus) %>%
#   summarize(length(family)) %>%
#   ungroup()

#create trait info file:
trait.info <- as.data.frame(subset(traits, select = -c(family, genus, species_matched, ObservationID,
                                                       DatabaseID, DatasetID)))

#check if both datasets are equal
nrow(hierarchy.info) == nrow(trait.info)



##### z % log transform #####
back_trans_pars <- list()
rm_col <- c()
for(i in 1:ncol(trait.info)){
  x <- trait.info[,i] # goes through the columns
  min_x <- min(x,na.rm = T) # takes the min of each column
  if(min_x < 0.00000000001){
    x <- x - min_x + 1 # make this optional if min x is neg
  }
  logx <- log10(x)
  mlogx <- mean(logx, na.rm = T)
  slogx <- sd(logx, na.rm = T)
  x <- (logx - mlogx)/slogx # Z transformation
  back_trans_pars[[i]] <- list(min_x = min_x,
                               mlogx = mlogx,
                               slogx = slogx)
  trait.info[,i] <- x
}

# write.table(back_trans_pars, "imputation_20240711\\back_trans_pars.csv")


##### gap-filling #####
#set-directory
tmp.dir <- dirname("imputation_20240711\\tmp")

#set parameters
smpl <- 900:1000
fold <- c(rep(10:20, 8), 10, 11)

#set number of iterations:
repe <- 90 #should be 90

for(i in 1:repe) { #loop for each trait (column)
  set.seed(123)
  GapFilling(as.matrix(trait.info), hierarchy.info,
             num.samples = smpl[i], num.folds.tuning=fold[i], burn=187,
             mean.gap.filled.output.path = paste0(tmp.dir,"/mean_gap_filled_",i,".txt"),
             std.gap.filled.output.path = paste0(tmp.dir,"/std_gap_filled_",i,".txt"),
             tmp.dir = tmp.dir, verbose=F)
}


##### load imputed traits and clean-up table #####
mean.trait <- list()
std.trait <- list()
for(i in 1:repe) { #loop for each trait (column)
  print(i)
  trt <- read.table(paste0("imputation_20240711\\mean_gap_filled_",i,".txt"), row.names=NULL, header=T)
  std <- read.table(paste0("imputation_20240711\\std_gap_filled_",i,".txt"), row.names=NULL, header=T)
  
  #Return to NA those values with SD > 1:
  for(j in 1:ncol(trt)) {
    trt[,j][std[,j]>1] <- NA
  }
  
  #Return to NA values > 1.5*max observed trait:
  for(j in 1:ncol(trt)) {
    maxt <- max(trait.info[,j], na.rm=T)
    trt[,j][trt[,j] > (maxt*1.5)] <- NA
  }
  
  mean.trait[[i]] <- trt
  std.trait[[i]] <- std
}


#### get mean across all means ####
mean.trait <- abind(mean.trait, along=3)
mean.trait <- apply(mean.trait, c(1,2), mean, na.rm=T)
mean.trait[is.nan(mean.trait)] <- NA

#data for back transforming output
back <- read.table("imputation_20240711\\back_trans_pars.csv")

#don't replace original values:
trait.info.noreplacement <- as.data.frame(mean.trait)

o <- 1 #to select the appropriate columns:
for(i in 1:ncol(trait.info.noreplacement)){
  
  #recover values:
  min_x <- back[1,o]
  mlogx <- back[1,o+1]
  slogx <- back[1,o+2]
  
  #back transform:
  x <- trait.info.noreplacement[,i] # goes through the columns
  logx <- (x*slogx) + mlogx
  b <- 10^logx
  
  #for negative values
  if(min_x < 0.00000000001){
    b <- b + min_x - 1 # make this optional if min x is neg
  }
  
  trait.info.noreplacement[,i] <- b
  o <- o+3
}

#save output
# write.csv(trait.info.noreplacement, "imputation_20240711\\imputed_traits.csv", row.names=F)


#### get mean across all std ####
std.trait <- abind(std.trait, along=3)
std.trait <- apply(std.trait, c(1,2), mean, na.rm=T)
std.trait[is.nan(std.trait)] <- NA

#don't replace original values:
trait.std.noreplacement <- as.data.frame(std.trait)

o <- 1 #to select the appropriate columns:
for(i in 1:ncol(trait.std.noreplacement)){
  
  #recover values:
  min_x <- back[1,o]
  mlogx <- back[1,o+1]
  slogx <- back[1,o+2]
  
  #back transform:
  x <- trait.std.noreplacement[,i] # goes through the columns
  logx <- (x*slogx) + mlogx
  b <- 10^logx
  
  #for negative values
  if(min_x < 0.00000000001){
    b <- b + min_x - 1 # make this optional if min x is neg
  }
  
  trait.std.noreplacement[,i] <- b
  o <- o+3
}

#save output
# write.csv(trait.std.noreplacement, "imputation_20240711\\imputed_traits_std.csv", row.names=F)



##### Impute missing values with "mice" #####
trait.info.mice <- complete(mice(trait.info.noreplacement, method="cart"), action = "long")
trait.info.mice.mean <- aggregate(. ~ .id, data = trait.info.mice[, -1], FUN = mean) #mean values: the final output
trait.info.mice.sd <- aggregate(. ~ .id, data = trait.info.mice[, -1], FUN = sd) #SDs per observation

# write.csv(trait.info.mice.mean, "imputation_20240711\\imputed_traits_mice.csv", row.names=F)
# write.csv(trait.info.mice.sd, "imputation_20240711\\imputed_traits_mice_std.csv", row.names=F)

#clean-up:
# rm(list = ls())




##### NutNet Traits - imputed/gathered for this project #####
nutnetTraitsContinuous <- read.csv('C:\\Users\\kjkomatsu\\OneDrive - UNCG\\manuscripts\\1_first author\\codominance\\data\\NutNet/NutNet_continuousTraitData_imputed_20240711.csv') %>% 
  filter(!species %in% correGExTraitsContinuous$species) %>% 
  rename(family=Family)

## NutNet N-fixers ##
nutnetNfixer <- read.csv('C:\\Users\\kjkomatsu\\OneDrive - UNCG\\manuscripts\\1_first author\\codominance\\data\\NutNet/NutNet_species_list_N-fixers.csv') %>% 
  select(species_matched, n_fixer) %>% 
  rename(species=species_matched,
         n_fixation_type=n_fixer) %>% 
  mutate(n_fixation_type_source='Werner')

nutnetTraitsCategorical <- read_xlsx('C:\\Users\\kjkomatsu\\OneDrive - UNCG\\manuscripts\\1_first author\\codominance\\data\\NutNet/NutNet_categorical_traits_2024b.xlsx') %>% 
  select(family, species_matched, leaf_type, leaf_type_source, leaf_compoundness, leaf_compoundness_source, growth_form, growth_form_source,
         photosynthetic_pathway, photosynthetic_pathway_source, lifespan, lifespan_source, stem_support, stem_support_source, clonal, clonal_source) %>% 
  rename(species=species_matched) %>% 
  full_join(nutnetNfixer) %>% 
  pivot_longer(leaf_type:n_fixation_type_source, names_to='trait2', values_to='trait_value') %>% 
  unique() %>% 
  filter(!species %in% correGExTraitsCategorical$species) %>% 
  mutate(type=ifelse(str_detect(trait2, "_source"), 'source', 'trait_value'),
         trait=str_remove(trait2, "_source$")) %>% 
  select(-trait2) %>% 
  pivot_wider(names_from=type, values_from=trait_value) %>% 
  mutate(error_risk_overall=case_when(
    trait=='leaf_type' ~ 0.002,
    trait=='leaf_compoundness' ~ 0.002,
    trait=='stem_support' ~ 0.033,
    trait=='growth_form' ~ 0.009,
    trait=='photosynthetic_pathway' ~ 0.017,
    trait=='lifespan' ~ 0.033,
    trait=='clonal' ~ 0.050,
    trait=='n_fixation_type' ~ NA
  ))



##### rbind #####
continuousTraits <- rbind(correGExTraitsContinuous, nutnetTraitsContinuous)
# write.csv(continuousTraits, 'C:\\Users\\kjkomatsu\\OneDrive - UNCG\\manuscripts\\1_first author\\2024_corre traits_Nature Scientific Data\\with BIEN and TIPleaf\\trait data for EDI\\v3\\CoRRE_continuousTraitData_Apr2026.csv', row.names=F)

length(unique(continuousTraits$species))
length(unique(continuousTraits$family))

categoricalTraits <- rbind(correGExTraitsCategorical, nutnetTraitsCategorical) %>% 
  filter(!is.na(family)) %>% 
  mutate(trait_value=ifelse(trait_value=='awl', 'scale', trait_value))
# write.csv(categoricalTraits, 'C:\\Users\\kjkomatsu\\OneDrive - UNCG\\manuscripts\\1_first author\\2024_corre traits_Nature Scientific Data\\with BIEN and TIPleaf\\trait data for EDI\\v3\\CoRRE_categoricalTraitData_Apr2026.csv', row.names=F)

length(unique(categoricalTraits$species))
length(unique(categoricalTraits$family))




##### source data for imputation #####
nutnetSource <- read.csv('C:\\Users\\kjkomatsu\\OneDrive - UNCG\\manuscripts\\1_first author\\codominance\\data\\nutnet\\nutnet_trait database_combo_continuous_20250701_long.csv') %>% 
  rename(family=Family,
         species=species_matched,
         trait=CleanTraitName,
         trait_value=StdValue) %>% 
  mutate(ReferenceID=paste(DatabaseID, DatasetID, sep='_'))

correGExSource <- read.csv('https://pasta.lternet.edu/package/data/eml/edi/1533/3/f21fe032152862d12f85d7d4b0eda94a') 

source <- rbind(nutnetSource, correGExSource)
# write.csv(nutnetSource, 'C:\\Users\\kjkomatsu\\OneDrive - UNCG\\manuscripts\\1_first author\\2024_corre traits_Nature Scientific Data\\with BIEN and TIPleaf\\trait data for EDI\\v3\\TRYAusBIEN_continuous_Apr2026_long.csv', row.names=F)

