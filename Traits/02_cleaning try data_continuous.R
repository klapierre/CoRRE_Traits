################################################################################
##  cleaning try data_continuous.R: Removing outliers and repeats from TRY Plant Database continous data
##  for CoRRE database plant species. Updated to include GEx and fewer species May2023
##
##  Authors: Meghan Avolio, Kimberly Komatsu
################################################################################

## NOTES: Update Feb 2021. We are adding new species with the CoRRE database 2.0 update and adding the species
## that were missing from the 2019 data pull. Redoing traits for all species.

#### Setting up script ####
library(tidyverse)
library(data.table)
# library(Taxonstand)
# library(WorldFlora)
# library(taxize)

theme_set(theme_bw(12))

#meghan's working directory
setwd("C:/Users/mavolio2/Dropbox/CoRRE_database/Data/")
setwd("E:/Dropbox/CoRRE_database/Data/")

#kim's working directory
setwd('C:\\Users\\kjkomatsu\\Dropbox (Smithsonian)\\working groups\\CoRRE\\CoRRE_database\\Data\\')


#### Reading in data ####
# TRY data
dat <- fread("OriginalData\\Traits\\TRY\\TRY_Traits_Downloaded_April2023.txt",sep = "\t",data.table = FALSE,stringsAsFactors = FALSE,strip.white = TRUE)

# trylist<-read.csv("OriginalData\\Traits\\TRY\\TryAccSpecies_2023.csv")  %>% 
#   extract("AccSpeciesName", c("genus", "species"), "([[:alpha:] ]+) ([[:alpha:] ]+)") %>% 
#   filter(species!='sp') %>% 
#   mutate(AccSpeciesName=paste(genus, species, sep=' '))
#   
# #get list of species names
# TRYsplist<-trylist
# 
# TRYsplist$family <- tax_name(trylist$AccSpeciesName, get = 'family', db='itis')$family

# #link to family
# WFO.file<-read.delim("CompiledData/Species_lists/WFO_Backbone/classification.txt")
# 
# familyinfo<-WFO.family(taxon=TRYsplist$genus, WFO.data = WFO.file)

# generate list of units for ALL TRY traits
units <- dat %>%
  select(OriglName, OrigUnitStr, TraitName, UnitName)%>%
  unique()
# write.csv(units, 'TRY_all traits_units.csv')


#### Merging CoRRE with TRY ####
# Read in cleaned CoRRE species names that link with TRY
corre_key <- read.csv("OriginalData\\Traits\\TRY\\corre2trykey_2021.csv") %>%
  select(species_matched, AccSpeciesID, AccSpeciesName) %>%
  unique()

corresp<-corre_key %>% 
  select(species_matched) %>% 
  unique() %>% 
  mutate(corre=1)

#get list to merge gex with try
gex <- read.csv("OriginalData\\Traits\\GEx_species_family_May2023.csv")

gexlist<-gex %>% 
  select(species_matched) %>% 
  mutate(AccSpeciesName=species_matched)
  
trysp<-dat %>% 
  select(AccSpeciesID, AccSpeciesName) %>% 
  unique()

gex_key<-gexlist %>% 
  right_join(trysp) %>% 
  na.omit()

key<-corre_key %>% 
  bind_rows(gex_key) %>% 
  unique()

# Merge CoRRE species names with TRY data
dat2 <- dat %>%
  right_join(key)


#### Selecting desired continuous traits ####

# tests <- dat3 %>%
#   filter(TraitID %in% c(3109)) %>%
#   #select(TraitID, OrigUnitStr, UnitName) %>%
#   #unique() %>% 
#   select(TraitID, UnitName, OrigUnitStr, OriglName, TraitName) %>%
#   #filter(UnitName!="g/m2/d") %>%
#   group_by(TraitID, UnitName, OrigUnitStr, OriglName, TraitName) %>%
#   summarize(n=length(TraitID))

# Subsetting out traits we want and replacing the numbers with names
#if origlname or unit is blank but there are no duplicates we are keeping it.
dat3 <- dat2 %>%
  filter(TraitID %in% c(4, 6, 9, 12, 13, 14, 15, 26, 27, 40, 41, 44, 45, 46, 47, 48, 50, 51, 52, 53, 55, 56, 57, 58, 66, 77, 80, 82, 83, 84, 106, 111, 138, 145, 146, 185, 186, 269, 270, 363, 475, 570, 614, 683, 1080, 1104, 1781, 3106, 3107, 3108, 3109, 3110, 3111, 3112, 3113, 3114, 3115, 3116, 3117, 3121, 3122)) %>%
  mutate(remove=ifelse(TraitID==48 & UnitName=='', 1, 
                ifelse(TraitID==3107 & UnitName=='cm', 1, 
                ifelse(TraitID==53 & UnitName=='g/m2/d', 1, 
                ifelse(TraitID==4 & UnitName=='', 1, 
                ifelse(TraitID==3116 & UnitName=='', 1, 
                ifelse(TraitID==3122 & OriglName=='WCt', 1, 
                ifelse(TraitID==3121 & OriglName=='WCs', 1,
                ifelse(TraitID==77 & OriglName=="Ra", 1, 
                ifelse(TraitID==77 & OriglName=="RGRh relative growth rate in height)", 1, 
                ifelse(TraitID==106 & OriglName=="", 1,	
                ifelse(TraitID==1781 & OriglName=="Min_Root tissue density (RTD)", 1,
                ifelse(TraitID==1781 & OriglName=="Max_Root tissue density (RTD)", 1,
                ifelse(TraitID==1781 & OriglName=="Upper quartile_Root tissue density (RTD)", 1,
                ifelse(TraitID==1781 & OriglName=="Lower quartile_Root tissue density (RTD)", 1,
                ifelse(TraitID==185 & OriglName=="", 1, 
                ifelse(TraitID==3109 & OriglName=="Area (dry) cm2", 1, 
                ifelse(TraitID==3109 & OriglName=="Dry.area.cm2", 1,
                ifelse(TraitID==3117 & OriglName=="LMA", 1,
                ifelse(TraitID==40 & OriglName=="A500 mass", 1,
                ifelse(TraitID==614 & OriglName=="Min_Specific root length (SRL)", 1,
                ifelse(TraitID==614 & OriglName=="Max_Specific root length (SRL)", 1, 
                ifelse(TraitID==41 & OriglName=="Rdarkm_25C_FixedQ10", 1,
                ifelse(TraitID==41 & OriglName=="Rdarkm_25C_varQ10", 1,
                ifelse(TraitID==41 & OriglName=="Rdarkm_MeasMonth.T_varQ10", 1,
                ifelse(TraitID==41 & OriglName=="Rdarkm_TWQ.T_varQ10", 1, 
                ifelse(TraitID==41 & OriglName=="Leaf Rdark mass 18C", 1,
                ifelse(TraitID==41 & OriglName=="Rm_adj_gst", 1, 
                ifelse(TraitID==41 & OriglName=="Rm25", 1, 
                ifelse(TraitID==41 & OriglName=="Rm_amb", 1, 
                ifelse(TraitID==41 & OriglName=="Leaf Rdark mass 28C", 1, 
                ifelse(TraitID==269 & OriglName=="Jmax25_Bern", 1, 
                ifelse(TraitID==269 & OriglName=="Jmax_25_Rog", 1,
                ifelse(TraitID==269 & OriglName=="Jmax_reported", 1, 
                ifelse(TraitID==47 & OriglName=="LDMC_min", 1, 
                ifelse(TraitID==47 & OriglName=="LDMC_max", 1, 
                ifelse(TraitID==47 & OriglName=="WCf", 1,
                ifelse(TraitID==47 & OriglName=="Leaf dry matter concentration predicted from NIRS", 1,
                ifelse(TraitID==3110 & OriglName=="Leaf_area_min", 1, 
                ifelse(TraitID==3110 & OriglName=="Leaf_area_max", 1, 
                ifelse(TraitID==13 & OriglName=="C amount%", 1, 
                ifelse(TraitID==13 & OriglName=="C_senesced_leaf", 1, 
                ifelse(TraitID==55 & OriglName=="WLfMass", 1, 
                ifelse(TraitID==55 & OriglName=="Mass_senesced_leaf", 1,
                ifelse(TraitID==44 & OriglName=="K_senesced_leaf", 1, 
                ifelse(TraitID==14 & OriglName=="N amount%", 1, 
                ifelse(TraitID==14 & OriglName=="N_senesced_leaf", 1,
                ifelse(TraitID==15 & OriglName=="P_senesced_leaf", 1, 
                ifelse(TraitID==111 & OriglName=="Asat_E  (mmol/m2/s)", 1, 
                ifelse(TraitID==145 & OriglName=="LaminaWidthMaxExtremeCm", 1, 0)))))))))))))))))))))))))))))))))))))))))))))))))) %>%
  mutate(remove2=ifelse(TraitID==145 & OriglName=="LaminaWidthMinCm", 1,
                 ifelse(TraitID==145 & OriglName=="LaminaWidthMinExtremeCm", 1,
                 ifelse(TraitID==145 & OriglName=="LeafWidth_max", 1,
                 ifelse(TraitID==53 & OriglName=="A500 area", 1, 
                 ifelse(TraitID==53 & OriglName=="Net photosynthesis at turgor loss point", 1, 
                 ifelse(TraitID==53 & OriglName=="Asat_Photo (umol/m2/s)", 1, 
                 ifelse(TraitID==3107 & OriglName=="Plant_height_generative_max", 1,
                 ifelse(TraitID==3108 & OriglName=="Flowering plant height, heighest leaf elongated", 1, 
                 ifelse(TraitID==3108 & OriglName=="Flowering plant height, heighest leaf not elongated", 1, 
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
                 ifelse(TraitID==9 & OriglName=="Root / shoot ratio seedlings", 1, 
                 ifelse(TraitID==363 & OriglName=="Root dry mass below 15 cm", 1,
                 ifelse(TraitID==363 & OriglName=="Root dry mass 0-15 cm", 1,
                 ifelse(TraitID==80 & OriglName=="Upper quartile_Root N content", 1,
                 ifelse(TraitID==80 & OriglName=="Lower quartile_Root N content", 1,
                 ifelse(TraitID==80 & OriglName=="Min_Root N content", 1,
                 ifelse(TraitID==80 & OriglName=="Max_Root N content", 1,
                 ifelse(TraitID==66 & OriglName=="single value [m/s^2]", 1,
                 ifelse(TraitID==66 & OriglName=="maximum TV [m/s^2]", 1,
                 ifelse(TraitID==66 & OriglName=="minimum TV [m/s^2]", 1,
                 ifelse(TraitID==66 & OriglName=="median TV [m/s^2]", 1,
                 ifelse(TraitID==26 & OriglName=="original seed mass (mg)", 1,
                 ifelse(TraitID==26 & OriglName=="OriginalSeedMassMean", 1,
                 ifelse(TraitID==6 & OriglName=="Rooting depth_min", 1,
                 ifelse(TraitID==27 & OriglName=="SeedsCurvedLength", 1, 
                 ifelse(TraitID==3116 & OriglName=="SLA_min", 1, 
                 ifelse(TraitID==3116 & OriglName=="SLA_max", 1, 
                 ifelse(TraitID==1080 & OriglName=="SRL roots >2mm diam (cm/g)", 1,
                 ifelse(TraitID==45 & OriglName=="LeafConductivityDorsal", 1, 
                 ifelse(TraitID==45 & OriglName=="LeadConductivityVentral", 1,
                 ifelse(TraitID==45 & OriglName=="Minimal stomatal conductance", 1, 
                 ifelse(TraitID==45 & OriglName=="Asat_Gs (mol/m2/s)", 1, 
                 ifelse(TraitID==186 & OriglName=="Vcmax25_Bern", 1,
                 ifelse(TraitID==186 & OriglName=="Vcmax_25_Rog", 1,
                 ifelse(TraitID==186 & OriglName=="Vcmax_reported", 1,
                 ifelse(TraitID==186 & OriglName=="", 1,
                 ifelse(TraitID==186 & OriglName=="Vcmax_a_25", 1, 0)))))))))))))))))))))))))))))))))))))))))))))))))) %>%
  mutate(remove3=ifelse(TraitID==186 & OriglName=="Vcmax25Rog", 1,
                 ifelse(TraitID==3120 & OrigUnitStr=="mmol/g", 1, 
                 ifelse(TraitID==26 & OriglName=="SeedMassMax" | OriglName=="SeedMassMin",1, 0)))) %>% # remove problem data where there are replicates for ObservationID
  filter(remove==0, remove2==0, remove3==0) %>%
  select(-remove, -remove2, -remove3) %>%
  mutate(CleanTraitName=ifelse(TraitID==4, 'stem_spec_density', 
                        ifelse(TraitID==6, 'rooting_depth', 
                        ifelse(TraitID==9, 'root:shoot', 
                        ifelse(TraitID==12, 'leaf_longevity', 
                        ifelse(TraitID==13, 'leaf_C',
                        ifelse(TraitID==14, 'leaf_N',
                        ifelse(TraitID==15, 'leaf_P',
                        ifelse(TraitID==26, 'seed_dry_mass', 
                        ifelse(TraitID==27, 'seed_length', 
                        ifelse(TraitID==41, 'dark_resp_rate',
                        ifelse(TraitID==44, 'leaf_K',
                        ifelse(TraitID==45, 'stomatal_conductance',
                        ifelse(TraitID==46, 'leaf_thickness', 
                        ifelse(TraitID==47, 'LDMC', 
                        ifelse(TraitID==48, 'leaf_density', 
                        ifelse(TraitID==53, 'photosynthesis_rate',
                        ifelse(TraitID==55, 'leaf_dry_mass', 
                        ifelse(TraitID==56, 'leaf_N:P', 
                        ifelse(TraitID==66, 'seed_terminal_velocity',
                        ifelse(TraitID==77, 'RGR', 
                        ifelse(TraitID==80, 'root_N', 
                        ifelse(TraitID==82, 'root_density', 
                        ifelse(TraitID==83, 'root_diameter', 
                        ifelse(TraitID==84, 'root_C', 
                        ifelse(TraitID==111, 'leaf_transp_rate',
                        ifelse(TraitID==138, 'seed_number',
                        ifelse(TraitID==145, 'leaf_width', 
                        ifelse(TraitID==146, 'leaf_C:N', 
                        ifelse(TraitID==186, 'Vc_max',
                        ifelse(TraitID==269, 'J_max',
                        ifelse(TraitID==363, 'root_dry_mass',
                        ifelse(TraitID==683, 'root_P', 
                        ifelse(TraitID==1080, 'SRL',
                        ifelse(TraitID==3106, 'plant_height_vegetative', 
                        ifelse(TraitID==3107, 'plant_height_generative',
                        ifelse(TraitID==3116, 'SLA', 
                        ifelse(TraitID==3110, 'leaf_area',
                        TraitID)))))))))))))))))))))))))))))))))))))) %>%
  filter(!is.na(StdValue)) %>% 
  filter(CleanTraitName!='plant_height_generative') #removing this trait because it doesn't make sense when compared to plant height vegetative

#testing consistent units for each trait and ranking traits by priority
# priority <- read.csv("OriginalData\\Traits\\TRY\\trait_priority.csv") %>%
#   rename(TraitID=TRY.trait.ID)

# Traits_Units <- dat3 %>%
#   select(TraitID, TraitName, CleanTraitName, UnitName) %>%
#   unique() %>%
#   mutate(Units=ifelse(CleanTraitName==3121, "g(W)/g(DM)", ifelse(CleanTraitName==3122, "g(W)/g(DM)", UnitName))) %>%
#   select(-UnitName) %>%
#   left_join(priority)

# write.csv(Traits_Units, "OriginalData\\Traits\\TRY\\TRYCoRREMerge\\ContTraitUnits.csv", row.names = F)


#### Removing observations that do not meet our criteria ####

# Removing dead plants
health <- dat %>%
  select(DatasetID, DataID, ObsDataID, AccSpeciesID, AccSpeciesName, TraitID, OriglName, TraitName, OrigValueStr, OrigUnitStr, StdValue, UnitName, ErrorRisk) %>%
  filter(DataID==1961) %>%
  mutate(drop=ifelse(OrigValueStr=="Dead", 1, 0)) %>%
  select(ObsDataID, drop) %>%
  unique() #list of dead plants

healthy <- dat3 %>% #merge to drop observations on dead plants
  left_join(health) %>% 
  mutate(drop=ifelse(is.na(drop), 0, drop)) %>% 
  filter(drop!=1) %>%  #no overlap in dataset
  select(-drop)

# Removing trees that are not seedlings -- based on data identified specifically as either mature or seedling
corre_treesp <- read.csv("CompiledData/Species_lists/species_families_trees_2021.csv") %>%
  mutate(AccSpeciesName=species_matched) #read in which species are trees

gex_treesp<-read.csv("C:/Users/mavolio2/Dropbox/CoRRE_database/Data/OriginalData/Traits/GEx_species_tree_complete.csv") %>% 
  filter(tree.non.tree %in% c("tree", "non-tree")) %>% 
  select(species_matched, family, tree.non.tree) %>% 
  separate(species_matched, into = c("genus", "species", "other"), sep=" ") %>% 
  filter(species!="NA") %>% 
  mutate(species_matched=paste(genus, species, sep=" ")) %>% 
  select(species_matched, family, tree.non.tree) %>% 
  left_join(gexlist) %>% 
  unique()

treesp<-corre_treesp %>% 
  bind_rows(gex_treesp) %>% 
  unique()

tree <- dat %>% #get list of tree observations that were made on seedlings
  select(DatasetID, DataID, ObsDataID, AccSpeciesID, AccSpeciesName, TraitID, OriglName, TraitName, OrigValueStr, OrigUnitStr, StdValue, UnitName, ErrorRisk) %>%
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
  select(DatasetID, DataID, ObsDataID, AccSpeciesID, AccSpeciesName, TraitID, OriglName, TraitName, OrigValueStr, OrigUnitStr, StdValue, UnitName, ErrorRisk) %>%
  filter(DataID==327) %>%
  mutate(drop=ifelse(OrigValueStr %in% c("Canadian High Arctic Research Station", "Control Plot", "field","Field", "Field (CG)", "Field (NE)", "field experiment", "Field Experiment", "Field plants", "forest stand", "Forest trees", "Forest understorey", "Fully open overstory 90 days, seedling", "Fully open overstory, seedling","Fully sunlit - Natural environment","High desert", "in situ", "In situ", "La Selva Biological Station", "meadows (M) and pastures (P) on south east to south west exposed slopes", "Montane meadow", "Mosses in forest", "nat env", "natural", "Natural", "natural-environment", "natural env", "natural envireonment", "natural enviroment", "Natural Enviroment", "natural environment", "Natural environment", "Natural Environment", "natural environment, high regional N and S deposition","natural environment, no warming, preccipitation ambient", "natural environment, sun exposed", "Natural Envrionment", "Natural Forest", "natural forest environment", "natural vegetation", "Natural Vegetation", "Natural Vegetation", "natural vegetation, but not top canopy", "natural wetland environment", "natural wetlands (field conditions)", "Natural/C", "natural_environment", "none", "None", "North facing slope", "Shade - Natural environment","South facing slope", "Trees in field"), 0, 1)) %>%
  select(ObsDataID, drop) %>%
  unique()
natural <- dat3 %>% #merge to drop observations in non-natural settings
  left_join(setting) %>% 
  mutate(drop=ifelse(is.na(drop), 0, drop)) %>% 
  filter(drop!=1) %>%  #no overlap in dataset
  select(-drop)

# Drop traits for trees -- based on whether or not the species is a tree (for all that were not designated as seedling, above)
splist <- key %>% 
  select(species_matched) %>%
  unique() %>%
  left_join(treesp) %>%
  na.omit() %>% #drop non fully id'd to species
  mutate(family2=ifelse(species_matched=="Comandra umbellata", "Santalaceae", 
                 ifelse(species_matched=="Dianella longifolia", "Asphodelaceae",
                 ifelse(species_matched=="Lancea tibetica", "Mazaceae", family)))) %>% 
  select(-family) %>% 
  rename(family=family2) %>% 
  unique

# splist_famconflict<-splist %>% 
#   group_by(species_matched) %>% 
#   summarize(n=length(family))

cont_traits <- dat3 %>%
  left_join(splist, by="species_matched") %>%
  mutate(remove=ifelse(tree.non.tree=="non-tree", 0, 1)) %>%
  filter(remove==0) %>%
  select(-tree.non.tree, -AccSpeciesName.y, -remove) %>%
  rename(AccSpeciesName=AccSpeciesName.x) #drops X observations


#### Add taxonomic information for each species ####
cont_traits2 <- cont_traits %>%
  select(DatasetID, ObservationID, family, species_matched, CleanTraitName, StdValue, ErrorRisk) %>%
  separate(remove = F, species_matched, into = c("genus", "species"), sep=" ") %>%
  select(-species)


#### Removing trait outliers based on TRY's Error Risk designation ####
cont_traits3 <- cont_traits2 %>%
  select(DatasetID, ObservationID, family, genus, species_matched, CleanTraitName, StdValue, ErrorRisk) %>%
  mutate(ErrorRisk2=ifelse(is.na(ErrorRisk), 0, ErrorRisk)) %>%
  filter(ErrorRisk2<3) %>% #removes all observations that are greater than 3 sd from full database mean: drops X observations
  select(-ErrorRisk, -ErrorRisk2) %>% 
  mutate(drop=ifelse(CleanTraitName=="seed_number" & StdValue==0, 1,
              ifelse(CleanTraitName==1104 & StdValue==0, 1, 0))) %>%  #removing seed number where 0 (X observations)
  filter(drop==0) %>% 
  select(-drop) %>% 
  filter(StdValue>0) #removing negative and 0 values (X)


#### Problem Datasets -- duplicate entries ####

# Problem: We investigated many repeats of similar plant vegetative height, LDMC, and water content values. We think this data was probably methodologically linked in some way to give these similar values. 
# Solution: We think the values are real (despite the methodological linkage), not duplicated data. Therefore, we are keeping all of this data.


# Problem: Three observations per plant (same ObservationID). Probably three leaves per plant, but no way to link leaves. Solution: Average.
d453 <- cont_traits3 %>%
  filter(DatasetID==453) %>%
  group_by(DatasetID, ObservationID, species_matched, CleanTraitName, family, genus) %>%
  summarise(StdValue=mean(StdValue)) %>% 
  ungroup()

# Problem: Two height values per plant (maybe temporal observations). Additionally, some plants have two identical values for root_P. Solution: Taking largest value.
d428 <- cont_traits3 %>% 
  filter(DatasetID==428 & CleanTraitName=="plant_height_vegetative" | DatasetID==428 & CleanTraitName=="root_P") %>%
  group_by(DatasetID, ObservationID, species_matched, CleanTraitName, family, genus) %>%
  summarise(StdValue=max(StdValue)) %>% 
  ungroup()

# Problem: Many observations with exactly repeated values for each trait*species. Solution: Take the average and make them all unique observations (loses linking of data on same individuals, but this seems better than keeping so many repeats).
d415 <- cont_traits3 %>% 
  filter(DatasetID==415) %>% 
  group_by(DatasetID, species_matched, CleanTraitName, family, genus)%>%
  summarise(StdValue=mean(StdValue)) %>% 
  ungroup() %>% 
  mutate(ObservationID=row_number())

# Problem: Many doubles of values for trait*speices with no clear pattern. Solution: Take the average and make them all unique observations (loses linking of data on same individuals, but this seems better than keeping so many repeats).
d25 <- cont_traits3 %>% 
  filter(DatasetID==25) %>% 
  group_by(DatasetID, species_matched, CleanTraitName, family, genus) %>%
  summarise(StdValue=mean(StdValue)) %>% 
  ungroup() %>% 
  mutate(ObservationID=row_number())

# Problem: Repeated Arabidopsis data. Dataset 102 has repeats with dataset 226. Datasets 359, 102, and 226 are all from the same data provider. Solution: Join all together and keep the data identifiers from 226 if there are repeats (otherwise use identifiers from original dataset).
d359 <- cont_traits3 %>% #all arabidopsis
  filter(DatasetID==359) 

d102 <- cont_traits3 %>% 
  filter(DatasetID==102) %>% 
  rename(did=DatasetID, 
         ob=ObservationID) 

d226 <- cont_traits3 %>% 
  filter(DatasetID==226) %>% 
  filter(species_matched!="Arabidopsis thaliana") %>% 
  bind_rows(d359) %>% 
  full_join(d102, multiple='all') %>% 
  mutate(DatasetID=ifelse(is.na(DatasetID), did, DatasetID),
         ObservationID=ifelse(is.na(ObservationID), ob, ObservationID)) %>% 
  select(-ob, -did) %>% 
  unique()

# Problem: Two datasets with lot of repeated data, but they are the same dataset. Solution: Join all data together and keep the data identifiers from dataset 327 if there are repeats (otherwise use identifiers from original dataset).
d327 <- cont_traits3 %>% 
  filter(DatasetID==327) %>% 
  rename(did=DatasetID, 
         ob=ObservationID) 

d353 <- cont_traits3 %>%
  filter(DatasetID==353) %>% 
  full_join(d327, multiple='all') %>% 
  mutate(DatasetID=ifelse(is.na(DatasetID), did, DatasetID),
         ObservationID=ifelse(is.na(ObservationID), ob, ObservationID)) %>% 
  select(-ob, -did) %>% 
  unique() 

# Problem: Dataset has lots of repeated data for some traits*species. Solution: For each species, find if there is repeated data for all traits collected on an individual. Where this occurs, keep the lowest ObservationID.
d1 <- cont_traits3 %>% 
  filter(DatasetID==1) %>% 
  pivot_wider(names_from=CleanTraitName, values_from = StdValue, names_prefix = "d__") %>% 
  group_by(species_matched, d__3115, d__leaf_N, d__leaf_C, d__plant_height_vegetative, d__leaf_longevity, d__3109  , d__leaf_thickness, d__40, d__50 , d__photosynthesis_rate, d__570) %>% 
  mutate(n=length(species_matched), obid2=min(ObservationID)) %>% 
  ungroup() %>% 
  select(-ObservationID) %>% 
  unique() %>% 
  pivot_longer(d__3115:d__570, names_to = "CleanTraitName1", values_to = "StdValue") %>% 
  separate(CleanTraitName1, into = c("prefix", "CleanTraitName"), "__") %>% 
  select( -prefix, -n) %>% 
  na.omit() %>% 
  rename(ObservationID=obid2)

# Problem: has repeated individuals within the dataset. Solution: Take the average for each individual for each trait. For each species, find if there is repeated data for all traits collected on an individual.
d412 <- cont_traits3 %>% 
  filter(DatasetID==412) %>% 
  group_by(DatasetID, ObservationID, family, genus, species_matched, CleanTraitName) %>% 
  summarize(StdValue=mean(StdValue)) %>% 
  ungroup()



# Problem: Many repeated values for trait 3117 (SLA undefined petiole), but they are attached to unique observations for other traits. Perhaps this is another methodological issue with how SLA was measured. Solution: Just keep what is there despite the repeats.
# d400 <- cont_traits3 %>% 
#   filter(DatasetID==400) %>% 
#   pivot_wider(names_from=CleanTraitName, values_from=StdValue)

# Problem: Rooting depth was recorded as the exact same value across many species. Solution: Drop rooting depth data from this dataset. Also trait 614 (measure of SRL) three values for a few observations. Solution 2: just average across traits
d339 <- cont_traits3 %>% 
  filter(DatasetID==339) %>% 
  group_by(DatasetID, ObservationID, family, genus, species_matched, CleanTraitName) %>% 
  summarize(StdValue=mean(StdValue))
  
# Problem: Plant vegetative height is measured 5x for for each plant (maybe temporal observations). Solution: keep the maximum height. Also only has one value for roots
d201<-cont_traits3 %>% 
  filter(DatasetID==201) %>% 
  group_by(DatasetID, ObservationID, family, genus, species_matched, CleanTraitName) %>% 
  summarize(StdValue=max(StdValue))

# Problem: Dark respiration rate is measured 3-4x for for each plant (maybe temporal observations). Solution: keep the max Has only one value for all other traits
d96<-cont_traits3 %>% 
  filter(DatasetID==96) %>% 
  group_by(DatasetID, ObservationID, family, genus, species_matched, CleanTraitName) %>% 
  summarize(StdValue=max(StdValue))

# Problem: trait 1104 (seed number / inflor) is measured 3x for for each plant. Solution: keep the mean
d355<-cont_traits3 %>% 
  filter(DatasetID==355) %>% 
  group_by(DatasetID, ObservationID, family, genus, species_matched, CleanTraitName) %>% 
  summarize(StdValue=mean(StdValue))

# Problem: for some individuals plant height is measured 2x (maybe over time). Solution. Take the max of all traits as all other traits only in there 1x.
d45<-cont_traits3 %>% 
  filter(DatasetID==45) %>% 
  group_by(DatasetID, ObservationID, family, genus, species_matched, CleanTraitName) %>% 
  summarize(StdValue=max(StdValue))

# Problem: for some individuals there are 2 measurements of 185 (related to VCmax). Solution. Take the mean of all traits as all other traits only in there 1x.
d87<-cont_traits3 %>% 
  filter(DatasetID==87) %>% 
  group_by(DatasetID, ObservationID, family, genus, species_matched, CleanTraitName) %>% 
  summarize(StdValue=mean(StdValue))

# Problem: for some individuals there are 2 measurements of plant height (maybe measured over time). Solution. Take the max of all traits as all other traits only in there 1x.
d299<-cont_traits3 %>% 
  filter(DatasetID==299) %>% 
  group_by(DatasetID, ObservationID, family, genus, species_matched, CleanTraitName) %>% 
  summarize(StdValue=max(StdValue))

# Problem: there are 2 measurements of leaf dry mass for all plants. Solution. Take the mean of all traits as all other traits only in there 1x.
d477<-cont_traits3 %>% 
  filter(DatasetID==477) %>% 
  group_by(DatasetID, ObservationID, family, genus, species_matched, CleanTraitName) %>% 
  summarize(StdValue=mean(StdValue))

# Problem: there are 2 measurements of plant height for some plants. Solution. Take the max of all traits as all other traits only in there 1x.
d520<-cont_traits3 %>% 
  filter(DatasetID==520) %>% 
  group_by(DatasetID, ObservationID, family, genus, species_matched, CleanTraitName) %>% 
  summarize(StdValue=max(StdValue))

# Problem: there are 2 measurements of plant height for some plants. Solution. Take the max of all traits as all other traits only in there 1x.
d655<-cont_traits3 %>% 
  filter(DatasetID==655) %>% 
  group_by(DatasetID, ObservationID, family, genus, species_matched, CleanTraitName) %>% 
  summarize(StdValue=max(StdValue))

# Problem: there are multiple measurements of 3114 (a measure of leaf area) for many individuals. Solution. Take the mean of all traits as all other traits are in there 1x.
d486<-cont_traits3 %>% 
  filter(DatasetID==486) %>% 
  group_by(DatasetID, ObservationID, family, genus, species_matched, CleanTraitName) %>% 
  summarize(StdValue=mean(StdValue))

#dataset 468 has many many repeats (50+) for plant height and not other measurements. This will add lots of missing data and not much information. We are going to replace all these repeats with 10 repeats
d468sub<-cont_traits3 %>% 
  filter(DatasetID==468) %>% 
  group_by(DatasetID, family, genus, species_matched, CleanTraitName, StdValue) %>% 
  summarize(n=length(StdValue)) %>% 
  filter(n>10)

d468todrop<-d468sub %>% 
  filter(DatasetID==468) %>% 
  ungroup() %>% 
  select(species_matched, CleanTraitName, StdValue) %>%
  unique() %>% 
  mutate(drop=1)
 
d468sub1<-cont_traits3 %>% 
  filter(DatasetID==468) %>% 
  right_join(d468sub) %>% 
  group_by(DatasetID, family, genus, species_matched, CleanTraitName, StdValue) %>% 
  mutate(number=row_number()) %>% 
  filter(number< max(10, length(StdValue)*0.05)) %>% 
  select(-n, -number)

d468<-cont_traits3 %>% 
  filter(DatasetID==468) %>% 
  left_join(d468todrop) %>% 
  filter(is.na(drop)) %>% 
  select(-drop) %>% 
  bind_rows(d468sub1)

# Problem: there are multiple measurements of many traits for many individuals. Solution. Take the mean of all traits.
d231<-cont_traits3 %>% 
  filter(DatasetID==231) %>% 
  group_by(DatasetID, ObservationID, family, genus, species_matched, CleanTraitName) %>% 
  summarize(StdValue=mean(StdValue))


# Dropping problem datasets and appending clean versions.
cont_traits4 <- cont_traits3 %>%
  filter(DatasetID!=453 & DatasetID!=415 & DatasetID!=25 & DatasetID!=226 & DatasetID!=359 & DatasetID!=102 & DatasetID!=353 & DatasetID!=327 & DatasetID!=1 & DatasetID!=412 & DatasetID!=201 & DatasetID!=96 & DatasetID!=339 & DatasetID!=355 & DatasetID!=45 & DatasetID!=87 & DatasetID!=299 & DatasetID!=477 & DatasetID!=520 & DatasetID!=655 & DatasetID!=486 & DatasetID!=231 & DatasetID!=468) %>%
  bind_rows(d339) %>% 
  mutate(remove=ifelse(DatasetID==428 & CleanTraitName=="plant_height_vegetative", 1, 
                ifelse(DatasetID==428 & CleanTraitName=="root_P", 1, 
                ifelse(DatasetID==339 & CleanTraitName=="rooting_depth", 1, 0)))) %>%
  filter(remove==0) %>%
  select(-remove) %>%
  bind_rows(d453) %>%
  bind_rows(d428) %>% 
  bind_rows(d415) %>% 
  bind_rows(d25) %>% 
  bind_rows(d226) %>% 
  bind_rows(d353) %>% 
  bind_rows(d1) %>% 
  bind_rows(d201) %>% 
  bind_rows(d96) %>% 
  bind_rows(d355) %>% 
  bind_rows(d45) %>% 
  bind_rows(d87) %>% 
  bind_rows(d299) %>%
  bind_rows(d477) %>% 
  bind_rows(d520) %>% 
  bind_rows(d655) %>% 
  bind_rows(d412) %>%
  bind_rows(d486) %>% 
  bind_rows(d231) %>% 
  bind_rows(d468) %>% 
  ungroup()

# Making sure there is just one measurement per variable.  
cont_traits5 <- cont_traits4 %>%
  mutate(present=1) %>%
  group_by(DatasetID, ObservationID, species_matched, CleanTraitName) %>%
  summarise(n=sum(present)) %>% 
  ungroup()

# Finding problem data (repeats).
# Remaining data with repeats were determined to either be real data (see above) or below the threshold to investigate (<3 repeats).
repeats <- cont_traits4 %>% 
  group_by(species_matched, CleanTraitName, StdValue) %>%
  summarize(n=length(StdValue)) %>%
  ungroup() %>% 
  filter(n>1)

ttraits <- cont_traits4 %>%
  group_by(DatasetID, ObservationID, family, genus, species_matched) %>%
  spread(CleanTraitName, StdValue, fill=NA)
  

# write.csv(ttraits, "OriginalData\\Traits\\TRY\\TRY_trait_data_continuous_May2023.csv", row.names = F)
# write.csv(cont_traits4, "OriginalData\\Traits\\TRY\\TRY_trait_data_continuous_long_May2023.csv", row.names = F)
