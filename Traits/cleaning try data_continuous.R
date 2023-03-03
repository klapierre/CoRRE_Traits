library(tidyverse)
library(data.table)
library(Taxonstand)

theme_set(theme_bw(12))

##update Feb 2021
##we are adding new species with the CoRRE database 2.0 update
## we also found we were missing species in the Nov 2019 datapull b/c of naming inconsistencies so we now redoing traits for all species.


#meghan's
setwd("C:/Users/mavolio2/Dropbox/CoRRE_database/Data/")
setwd("E:/Dropbox/CoRRE_database/Data/")

#kim's desktop
setwd('C:\\Users\\komatsuk\\Dropbox (Smithsonian)\\working groups\\CoRRE\\converge_diverge\\datasets\\Traits\\Try Data Nov 2019')

#kim's laptop
setwd('C:\\Users\\lapie\\Dropbox (Smithsonian)\\working groups\\CoRRE\\converge_diverge\\datasets\\Traits\\Try Data Nov 2019')

#read in both datasets
# dat1<-fread("TRYCoRREMerge/TRY_Traits_Download_Nov2019.txt",sep = "\t",data.table = FALSE,stringsAsFactors = FALSE,strip.white = TRUE)
# 
# dat2<-fread("TRYCoRREMerge/TRY_Traits_Download_Feb2021.txt",sep = "\t",data.table = FALSE,stringsAsFactors = FALSE,strip.white = TRUE)

dat<-fread("TRYCoRREMerge/TRY_Traits_Download_Feb15_2021.txt",sep = "\t",data.table = FALSE,stringsAsFactors = FALSE,strip.white = TRUE)

#generate list of units for ALL TRY traits
units <- dat%>%
  select(OriglName, OrigUnitStr, TraitName, UnitName)%>%
  unique()
# write.csv(units, 'TRY_all traits_units.csv')


#removing trait outlines
dat2<-dat%>%
  select(DatasetID,DataID, ObsDataID, ObservationID, AccSpeciesID, AccSpeciesName, TraitID, OriglName, TraitName, OrigValueStr, OrigUnitStr, StdValue, UnitName, ErrorRisk)%>%
  mutate(ErrorRisk2=ifelse(is.na(ErrorRisk), 0, ErrorRisk))%>%
  filter(ErrorRisk2<3)%>%
  filter(!is.na(TraitID))

#merging corre with try

#read in species links to merge try with core
key<-read.csv("TRYCoRREMerge/corre2trykey_2021.csv")%>%
  select(species_matched, AccSpeciesID, AccSpeciesName)%>%
  unique()


#merge list of species in Corre with TRY
dat3<-dat2%>%
  right_join(key)%>%
  select(-ErrorRisk, -ErrorRisk2)


####selecting desired continuous traits
#figuring out how clean traits are
###FEB 15 
# 
# tests<-dat3%>%
#   filter(TraitID %in% c(3109))%>%
#   #select(TraitID, OrigUnitStr, UnitName)%>%
#   #unique()
#   select(TraitID, UnitName, OrigUnitStr, OriglName, TraitName)%>%
#   #filter(UnitName!="g/m2/d")%>%
#   group_by(TraitID, UnitName, OrigUnitStr, OriglName, TraitName)%>%
#   summarize(n=length(TraitID))

#subsetting out traits and naming them
#if origlname or unit is blank but there are no duplicates we are keepping it.
cont_traits<-dat3%>%
  filter(TraitID %in% c(4, 6, 9, 12, 13, 14, 15, 26, 27, 40, 41, 44, 45, 46, 47, 48, 50, 51, 52, 53, 55, 56, 57, 58, 66, 77, 80, 82, 83, 84, 106, 111, 138, 145, 146, 185, 186, 269, 270, 363, 475, 570, 614, 683, 1080, 1104, 1781, 2809, 3106, 3107, 3108, 3109, 3110, 3111, 3112, 3113, 3114, 3115, 3116, 3117,3120, 3121, 3122))%>%
  mutate(remove=ifelse(TraitID==48&UnitName=='', 1, 
                ifelse(TraitID==3107&UnitName=='cm', 1, 
                ifelse(TraitID==53&UnitName=='g/m2/d', 1, 
                ifelse(TraitID==4&UnitName=='', 1, 
                ifelse(TraitID==3116&UnitName=='', 1, 
                ifelse(TraitID==3122&OriglName=='WCt', 1, 
                ifelse(TraitID==3121&OriglName=='WCs', 1,
                ifelse(TraitID==77&OriglName=="Ra", 1, 
                ifelse(TraitID==77&OriglName=="RGRh relative growth rate in height)", 1, 
                ifelse(TraitID==106&OriglName=="", 1,	
                ifelse(TraitID==1781&OriglName=="Min_Root tissue density (RTD)", 1,
                ifelse(TraitID==1781&OriglName=="Max_Root tissue density (RTD)", 1,
                ifelse(TraitID==1781&OriglName=="Upper quartile_Root tissue density (RTD)", 1,
                ifelse(TraitID==1781&OriglName=="Lower quartile_Root tissue density (RTD)", 1,
                ifelse(TraitID==185&OriglName=="", 1, 
                ifelse(TraitID==3109&OriglName=="Area (dry) cm2", 1, 
                ifelse(TraitID==3109&OriglName=="Dry.area.cm2", 1,
                ifelse(TraitID==3117&OriglName=="LMA", 1,
                ifelse(TraitID==40&OriglName=="A500 mass", 1,
                ifelse(TraitID==614&OriglName=="Min_Specific root length (SRL)", 1,
                ifelse(TraitID==614&OriglName=="Max_Specific root length (SRL)", 1, 
                ifelse(TraitID==41&OriglName=="Rdarkm_25C_FixedQ10", 1,
                ifelse(TraitID==41&OriglName=="Rdarkm_25C_varQ10", 1,
                ifelse(TraitID==41&OriglName=="Rdarkm_MeasMonth.T_varQ10", 1,
                ifelse(TraitID==41&OriglName=="Rdarkm_TWQ.T_varQ10", 1, 
                ifelse(TraitID==41&OriglName=="Leaf Rdark mass 18C", 1,
                ifelse(TraitID==41&OriglName=="Rm_adj_gst", 1, 
                ifelse(TraitID==41&OriglName=="Rm25", 1, 
                ifelse(TraitID==41&OriglName=="Rm_amb", 1, 
                ifelse(TraitID==41&OriglName=="Leaf Rdark mass 28C", 1, 
                ifelse(TraitID==269&OriglName=="Jmax25_Bern", 1, 
                ifelse(TraitID==269&OriglName=="Jmax_25_Rog", 1,
                ifelse(TraitID==269&OriglName=="Jmax_reported", 1, 
                ifelse(TraitID==47&OriglName=="LDMC_min", 1, 
                ifelse(TraitID==47&OriglName=="LDMC_max", 1, 
                ifelse(TraitID==47&OriglName=="WCf", 1,
                ifelse(TraitID==47&OriglName=="Leaf dry matter concentration predicted from NIRS", 1,
                ifelse(TraitID==3110&OriglName=="Leaf_area_min", 1, 
                ifelse(TraitID==3110&OriglName=="Leaf_area_max", 1, 
                ifelse(TraitID==13&OriglName=="C amount%", 1, 
                ifelse(TraitID==13&OriglName=="C_senesced_leaf", 1, 
                ifelse(TraitID==55&OriglName=="WLfMass", 1, 
                ifelse(TraitID==55&OriglName=="Mass_senesced_leaf", 1,
                ifelse(TraitID==44&OriglName=="K_senesced_leaf", 1, 
                ifelse(TraitID==14&OriglName=="N amount%", 1, 
                ifelse(TraitID==14&OriglName=="N_senesced_leaf", 1,
                ifelse(TraitID==15&OriglName=="P_senesced_leaf", 1, 
                ifelse(TraitID==111&OriglName=="Asat_E  (mmol/m2/s)", 1, 
                ifelse(TraitID==145&OriglName=="LaminaWidthMaxExtremeCm", 1, 0))))))))))))))))))))))))))))))))))))))))))))))))))%>%
  mutate(remove2=ifelse(TraitID==145&OriglName=="LaminaWidthMinCm", 1,
                 ifelse(TraitID==145&OriglName=="LaminaWidthMinExtremeCm", 1,
                 ifelse(TraitID==145&OriglName=="LeafWidth_max", 1,
                 ifelse(TraitID==53&OriglName=="A500 area", 1, 
                 ifelse(TraitID==53&OriglName=="Net photosynthesis at turgor loss point", 1, 
                 ifelse(TraitID==53&OriglName=="Asat_Photo (umol/m2/s)", 1, 
                 ifelse(TraitID==3107&OriglName=="Plant_height_generative_max", 1,
                 ifelse(TraitID==3108&OriglName=="Flowering plant height, heighest leaf elongated", 1, 
                 ifelse(TraitID==3108&OriglName=="Flowering plant height, heighest leaf not elongated", 1, 
                 ifelse(TraitID==3106&OriglName=="Flowering plant height, heighest leaf elongated", 1, 
                 ifelse(TraitID==3106&OriglName=="Flowering plant height, heighest leaf not elongated", 1, 
                 ifelse(TraitID==3106&OriglName=="Height at 20 Years", 1, 
                 ifelse(TraitID==3106&OriglName=="MaximumHeightMinM", 1, 
                 ifelse(TraitID==3106&OriglName=="MaximumHeightExtremeM", 1, 
                 ifelse(TraitID==3106&OriglName=="Maximum Height", 1, 
                 ifelse(TraitID==3106&OriglName=="Plant_height_vegetative_min", 1, 
                 ifelse(TraitID==3106&OriglName=="Plant_height_vegetative_mean", 1,
                 ifelse(TraitID==3106&OriglName=="Length (aquatic)", 1, 
                 ifelse(TraitID==3106&OriglName=="Height (seedling)", 1,
                 ifelse(TraitID==3106&OriglName=="Height max (m)", 1,
                 ifelse(TraitID==3106&OriglName=="strechedPlantHight", 1,
                 ifelse(TraitID==3106&OriglName=="MaximumHeightM", 1,
                 ifelse(TraitID==9&OriglName=="Root / shoot ratio seedlings", 1, 
                 ifelse(TraitID==363&OriglName=="Root dry mass below 15 cm", 1,
                 ifelse(TraitID==363&OriglName=="Root dry mass 0-15 cm", 1,
                 ifelse(TraitID==80&OriglName=="Upper quartile_Root N content", 1,
                 ifelse(TraitID==80&OriglName=="Lower quartile_Root N content", 1,
                 ifelse(TraitID==80&OriglName=="Min_Root N content", 1,
                 ifelse(TraitID==80&OriglName=="Max_Root N content", 1,
                 ifelse(TraitID==66&OriglName=="single value [m/s^2]", 1,
                 ifelse(TraitID==66&OriglName=="maximum TV [m/s^2]", 1,
                 ifelse(TraitID==66&OriglName=="minimum TV [m/s^2]", 1,
                 ifelse(TraitID==66&OriglName=="median TV [m/s^2]", 1,
                 ifelse(TraitID==26&OriglName=="original seed mass (mg)", 1,
                 ifelse(TraitID==26&OriglName=="OriginalSeedMassMean", 1,
                 ifelse(TraitID==6&OriglName=="Rooting depth_min", 1,
                 ifelse(TraitID==27&OriglName=="SeedsCurvedLength", 1, 
                 ifelse(TraitID==3116&OriglName=="SLA_min", 1, 
                 ifelse(TraitID==3116&OriglName=="SLA_max", 1, 
                 ifelse(TraitID==1080&OriglName=="SRL roots >2mm diam (cm/g)", 1,
                 ifelse(TraitID==45&OriglName=="LeafConductivityDorsal", 1, 
                 ifelse(TraitID==45&OriglName=="LeadConductivityVentral", 1,
                 ifelse(TraitID==45&OriglName=="Minimal stomatal conductance", 1, 
                 ifelse(TraitID==45&OriglName=="Asat_Gs (mol/m2/s)", 1, 
                 ifelse(TraitID==186&OriglName=="Vcmax25_Bern", 1,
                 ifelse(TraitID==186&OriglName=="Vcmax_25_Rog", 1,
                 ifelse(TraitID==186&OriglName=="Vcmax_reported", 1,
                 ifelse(TraitID==186&OriglName=="", 1,
                 ifelse(TraitID==186&OriglName=="Vcmax_a_25", 1, 0))))))))))))))))))))))))))))))))))))))))))))))))))%>%
  mutate(remove3=ifelse(TraitID==186&OriglName=="Vcmax25Rog", 1,
                 ifelse(TraitID==3120&OrigUnitStr=="mmol/g", 1, 
                 ifelse(TraitID==26&OriglName=="SeedMassMax"|OriglName=="SeedMassMin",1, 0))))%>%#remove problem data where there are replicates for ObservationID
  filter(remove==0)%>%
  filter(remove2==0)%>%
  filter(remove3==0)%>%
  select(-remove, -remove2, -remove3)%>%
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
                        ifelse(TraitID==2809, 'seedbank_duration',
                        ifelse(TraitID==3106, 'plant_height_vegetative', 
                        ifelse(TraitID==3107, 'plant_height_generative', 
                        ifelse(TraitID==3116, 'SLA', 
                        ifelse(TraitID==3110, 'leaf_area',
                        ifelse(TraitID==3120, 'water_content', 
                        TraitID))))))))))))))))))))))))))))))))))))))))%>%
  filter(!is.na(StdValue))

#testing consistent units for each trait and ranking traits by proirity
priority<-read.csv("TRYCoRREMerge/trait_priority.csv")%>%
  rename(TraitID=TRY.trait.ID)

Traits_Units<-cont_traits%>%
  select(TraitID, TraitName, CleanTraitName, UnitName)%>%
  unique%>%
  mutate(Units=ifelse(CleanTraitName==3121, "g(W)/g(DM)", ifelse(CleanTraitName==3122, "g(W)/g(DM)", UnitName)))%>%
  select(-UnitName)%>%
  left_join(priority)

#write.csv(Traits_Units, "For Franzi/ContTraitUnits.csv", row.names = F)

#subset out dead plants
#get list of dead plants
health<-dat%>%
  select(DatasetID,DataID, ObsDataID, AccSpeciesID, AccSpeciesName, TraitID, OriglName, TraitName, OrigValueStr, OrigUnitStr, StdValue, UnitName, ErrorRisk)%>%
  mutate(ErrorRisk2=ifelse(is.na(ErrorRisk), 0, ErrorRisk))%>%
  filter(ErrorRisk2<8)%>%
  filter(DataID==1961)%>%
  mutate(drop=ifelse(OrigValueStr=="Dead", 1, 0))%>%
  select(ObsDataID, drop)%>%
  unique()

table(health$OrigValueStr)

#merge with our list, there is no overalp
healthy<-cont_traits%>%
  full_join(health)

##drop out trees that not seedlings
#read in which species are trees
treesp<-read.csv("C:/Users/mavolio2/Dropbox/CoRRE_database/Data/CompiledData/Species_lists/species_families_trees_2021.csv")%>%
  mutate(AccSpeciesName=species_matched)

#get list of tree observations that were made on seedlings
develop<-dat%>%
  select(DatasetID,DataID, ObsDataID, AccSpeciesID, AccSpeciesName, TraitID, OriglName, TraitName, OrigValueStr, OrigUnitStr, StdValue, UnitName, ErrorRisk)%>%
  mutate(ErrorRisk2=ifelse(is.na(ErrorRisk), 0, ErrorRisk))%>%
  filter(ErrorRisk2<8)%>%
  filter(DataID==413)%>%
  right_join(treesp)%>%
  filter(tree.non.tree=="tree")%>%
  mutate(drop=ifelse(OrigValueStr=="seedlings"|OrigUnitStr==0|OrigValueStr=="seedling"|OrigValueStr=="Seedling (0 - 1 y)",  0, 1))%>%
  select(ObsDataID, drop)%>%
  unique()

table(unique(develop$drop))

#merge to drop tree observations that are not seedlings - none were.
developed<-cont_traits%>%
  full_join(develop)

##drop out plant that were not measured in natural conditions - there is no overlap

#get list of observations that were not in natural settings
setting<-dat%>%
  select(DatasetID,DataID, ObsDataID, AccSpeciesID, AccSpeciesName, TraitID, OriglName, TraitName, OrigValueStr, OrigUnitStr, StdValue, UnitName, ErrorRisk)%>%
  mutate(ErrorRisk2=ifelse(is.na(ErrorRisk), 0, ErrorRisk))%>%
  filter(ErrorRisk2<8)%>%
  filter(DataID==327)%>%
  mutate(drop=ifelse(OrigValueStr=="Botanical garden"|OrigValueStr=="botanical garden (Bergius Botanical Garden, Stockholm, Sweden)"|OrigValueStr=="Botanical gardens, greenhouses and other atypical habitats"|OrigValueStr=="Chamber"|OrigValueStr=="climate chamber"|OrigValueStr=="Climate chamber"|OrigValueStr=="Climate Chamber"|OrigValueStr=="Climate chamber, non-limiting conditions, (cf. dataset reference)"|OrigValueStr=="climate chambers"|OrigValueStr=="Common Garden"|OrigValueStr=="Controlled climate chamber"|OrigValueStr=="controlled environment room"|OrigValueStr=="drought treatment"|OrigValueStr=="FACE"|OrigValueStr=="FE"|OrigValueStr=="C"|OrigValueStr=="Field Experiment"|OrigValueStr=="FW"|OrigValueStr=="G"|OrigValueStr=="GH"|OrigValueStr=="Glasshouse"|OrigValueStr=="Greehouse"|OrigValueStr=="Green house"|OrigValueStr=="greenhouse"|OrigValueStr=="Greenhouse"|OrigValueStr=="Greenhouse, grrowth container"|OrigValueStr=="groth chamber"|OrigValueStr=="growth-chamber"|OrigValueStr=="growth chamber"|OrigValueStr=="Growth chamber"|OrigValueStr=="Growth Chamber"|OrigValueStr=="growth chambers"|OrigValueStr=="Growth chambers"|OrigValueStr=="Growth exp"|OrigValueStr=="hydroponic"|OrigValueStr=="Irrigation"|OrigValueStr=="Irrigation and N fertilisation (100 kg/ha)"|OrigValueStr=="LAU_Ploughed/mown"|OrigValueStr=="LAU_Ploughed/mown and fertilized"|OrigValueStr=="mesocosm"|OrigValueStr=="mini-ecosystem"|OrigValueStr=="N"|OrigValueStr=="natural environment, high warming +4C, preccipitation ambient"|OrigValueStr=="natural environment, high warming +4C, preccipitation ambient -50%"|OrigValueStr=="natural environment, high warming +4C, preccipitation ambient +50%"|OrigValueStr=="natural environment, low warming +1.5C, preccipitation ambient"|OrigValueStr=="natural environment, low warming +1.5C, preccipitation ambient -50%"|OrigValueStr=="natural environment, low warming +1.5C, preccipitation ambient +50%"|OrigValueStr=="natural environment, medium warming +2.5C, preccipitation ambient"|OrigValueStr=="natural environment, medium warming +2.5C, preccipitation ambient -50%"|OrigValueStr=="natural environment, medium warming +2.5C, preccipitation ambient +50%"|OrigValueStr=="natural environment, no warming, preccipitation ambient -50%"|OrigValueStr=="natural environment, no warming, preccipitation ambient +50%"|OrigValueStr=="natural grassland, experimental nutrient NP addition"|OrigValueStr=="nutrient addition experiment"|OrigValueStr=="Open Top"|OrigValueStr=="open-top chamber"|OrigValueStr=="Open top chambers"|OrigValueStr=="OTC"|OrigValueStr=="plantation"|OrigValueStr=="PM"|OrigValueStr=="pot"|OrigValueStr=="Pot-grown"|OrigValueStr=="Pots outside"|OrigValueStr=="pots, outside in natural environment"|OrigValueStr=="shade houses"|OrigValueStr=="university campus"|OrigValueStr=="Uzbekistan: Irrigated desert land"|OrigValueStr=="VER_permanent extensively mown meadow"|OrigValueStr=="VER_permanent meadow mown and fertilized"|OrigValueStr=="VER_permanent meadows mown and fertilized"|OrigValueStr=="water stress experiment"|OrigValueStr=="water treatment", 1, 0))%>%
  select(ObsDataID, drop)%>%
  unique()

table(setting$drop)


#merge with out traits - there is no overlap
settings<-cont_traits%>%
  full_join(setting)


#add info on genus family

#drop non fully id'd to species
splist<-key%>%
  select(species_matched)%>%
  unique%>%
  left_join(treesp)%>%
  na.omit()


##drop trees - check are keeping tree seedlings if the data exists.
cont_traits2<-cont_traits%>%
  left_join(splist, by="species_matched")%>%
  mutate(remove=ifelse(tree.non.tree=="non-tree", 0, 1))%>%
  filter(remove==0)%>%
  select(-tree.non.tree, -AccSpeciesName.y, -remove)%>%
  rename(AccSpeciesName=AccSpeciesName.x)

#get genus info and select desired columns
cont_traits3<-cont_traits2%>%
  select(DatasetID, ObservationID, family, species_matched, CleanTraitName, StdValue)%>%
  separate(remove = F, species_matched, into = c("genus", "species"), sep=" ")%>%
  select(-species)


###getting dataset to give to Frazni and Padu

#investigating problem traits
d453<-cont_traits3%>%#this dataset has 3 obs per plant, but no way to link leaves so we are averaging
  filter(DatasetID==453)%>%
  group_by(DatasetID, ObservationID, species_matched, CleanTraitName, family, genus)%>%
  summarise(StdValue=mean(StdValue))

d428<-cont_traits3%>% #this dataset has two height values per plant, we are taking the largest
  filter(DatasetID==428&CleanTraitName=="plant_height_vegetative"|DatasetID==428&CleanTraitName=="root_P")%>%
  group_by(DatasetID, ObservationID, species_matched, CleanTraitName, family, genus)%>%
  summarise(StdValue=max(StdValue))

d415<-cont_traits3 %>% #this dataset has 415 repeats of all data
  filter(DatasetID==415) %>% 
  group_by(DatasetID, species_matched, CleanTraitName, family, genus)%>%
  summarise(StdValue=mean(StdValue)) %>% 
  ungroup() %>% 
  mutate(ObservationID=row_number())

d25<-cont_traits3 %>% #this dataset has repeats of many repeats with no pattern
  filter(DatasetID==25) %>% 
  group_by(DatasetID, species_matched, CleanTraitName, family, genus)%>%
  summarise(StdValue=mean(StdValue)) %>% 
  ungroup() %>% 
  mutate(ObservationID=row_number())

#two data sets with repeated Arabidopsis data and 102 has repeats with 226 all same data provider
d359<-cont_traits3 %>% #all arabidopsis
  filter(DatasetID==359) 
d102<-cont_traits3 %>% 
  filter(DatasetID==102)%>% 
  rename(did=DatasetID, 
         ob=ObservationID) 
d226<-cont_traits3 %>% 
  filter(DatasetID==226) %>% 
  filter(species_matched!="Arabidopsis thaliana") %>% 
  bind_rows(d359) %>% 
  full_join(d102, join_by=c("family", "species_matches", "genus", "CleanTraitName", "StdValue")) %>% 
  mutate(DatasetID=ifelse(is.na(DatasetID), did, DatasetID),
         ObservationID=ifelse(is.na(ObservationID), ob, ObservationID)) %>% 
  select(-ob, -did) %>% 
  unique()

#two datasets with lot of repeated data - same basic database
d327<-cont_traits3 %>% #has one more species than 353
  filter(DatasetID==327) %>% 
  rename(did=DatasetID, 
         ob=ObservationID) 
d353<-cont_traits3 %>% #getting rid of repeated data by full joining by unique values
  filter(DatasetID==353) %>% 
  full_join(d327, join_by=c("family", "species_matches", "genus", "CleanTraitName", "StdValue")) %>% 
  mutate(DatasetID=ifelse(is.na(DatasetID), did, DatasetID),
         ObservationID=ifelse(is.na(ObservationID), ob, ObservationID)) %>% 
  select(-ob, -did) %>% 
  unique() 

# dataset has lot of repeated data randomly
d1<-cont_traits3 %>% 
  filter(DatasetID==1) %>% 
  pivot_wider(names_from=CleanTraitName, values_from = StdValue, names_prefix = "d__") %>% 
  group_by(species_matched, d__3115, d__leaf_N, d__leaf_C ,d__plant_height_vegetative, d__leaf_longevity, d__3109  , d__leaf_thickness, d__40, d__50 , d__photosynthesis_rate, d__570  ) %>% 
  mutate(n=length(species_matched), obid2=min(ObservationID)) %>% 
  select(-ObservationID) %>% 
  unique() %>% 
  pivot_longer(d__3115:d__570, names_to = "CleanTraitName1", values_to = "StdValue") %>% 
  separate(CleanTraitName1, into = c("prefix", "CleanTraitName"), "__") %>% 
  select( -prefix, -n) %>% 
  na.omit %>% 
  rename(ObservationID=obid2)


##we investigated many repeats of similar LDMC and water content values. We think this is probably not the best way to have collected this data, but we think the values are real and not duplicated data and are keeping it. This applies to plant vegeative height, LDMC and water content.

#dataset 412 I suspect has lots of duplicated datasets in TRY. I am doing an inner join to try to correct this with the full trait database
d412<-cont_traits3 %>% 
  filter(DatasetID==412) %>% 
  mutate(CleanTraitName=ifelse(CleanTraitName=="leaf_C:N", "LeafCN", CleanTraitName)) %>% 
  pivot_wider(names_from=CleanTraitName, values_from = StdValue, names_prefix = "d__") %>% 
  group_by(species_matched, d__40,d__3117,d__leaf_N, d__leaf_P,d__dark_resp_rate,d__leaf_density,   d__leaf_thickness,d__LDMC,d__water_content,d__leaf_C,
           d__LeafCN, d__leaf_longevity,d__3122,d__50,d__51,d__photosynthesis_rate,d__570) %>% 
  mutate(n=length(species_matched), obid2=min(ObservationID)) %>% 
  select(-ObservationID) %>% 
  unique() %>% 
  pivot_longer(d__40:d__570, names_to = "CleanTraitName1", values_to = "StdValue") %>% 
  separate(CleanTraitName1, into = c("prefix", "CleanTraitName"), "__") %>% 
  select( -prefix, -n) %>% 
  na.omit %>% 
  mutate(CleanTraitName=ifelse(CleanTraitName=="LeafCN", "leaf_C:N", CleanTraitName)) %>% 
  rename(did=DatasetID) 

#dataset 20 I suspect has lots of duplicated datasets in TRY. I am doing an inner join to try to correct this with the full trait database
d20<-cont_traits3 %>% 
  filter(DatasetID==20) %>% 
  rename(did=DatasetID, 
         ob=ObservationID) 

#this dataset has a lot of repeated values for 3117 (sla), but they are attached to unique observations for other traits and I think we just need to keep what is there.
d400<-cont_traits3 %>% 
  filter(DatasetID==400) %>% 
  pivot_wider(names_from=CleanTraitName, values_from=StdValue)

#dropping problem datasets and re appending clean ones and removing repeated datasets - Triple check correct things are dropped and added.
cont_traits4<-cont_traits3%>%
  filter(DatasetID!=453&DatasetID!=415&DatasetID!=25&DatasetID!=226&DatasetID!=359&DatasetID!=327&DatasetID!=353&DatasetID!=412&DatasetID!=102&DatasetID!=1&DatasetID!=20)%>%
  mutate(remove=ifelse(DatasetID==428&CleanTraitName=="plant_height_vegetative", 1, 
                ifelse(DatasetID==428&CleanTraitName=="root_P", 1, 
                ifelse(DatasetID==339&CleanTraitName=="rooting_depth", 1,
                ifelse(CleanTraitName=="seed_number"&StdValue==0, 1, 
                ifelse(CleanTraitName==1104&StdValue==0, 1, 0))))))%>%
  filter(remove==0)%>%
  select(-remove)%>%
  bind_rows(d453)%>%
  bind_rows(d428) %>% 
  filter(CleanTraitName!="seedbank_duration") %>% 
  bind_rows(d415) %>% 
  bind_rows(d25) %>% 
  bind_rows(d226) %>% 
  bind_rows(d353) %>% 
  bind_rows(d1) %>% 
  full_join(d412, join_by=c("family", "species_matches", "genus", "CleanTraitName", "StdValue")) %>% 
  mutate(DatasetID=ifelse(is.na(DatasetID), did, DatasetID),
         ObservationID=ifelse(is.na(ObservationID), obid2, ObservationID)) %>% 
  select(-obid2, -did) %>% 
  unique() %>% 
  full_join(d20, join_by=c("family", "species_matches", "genus", "CleanTraitName", "StdValue")) %>% 
  mutate(DatasetID=ifelse(is.na(DatasetID), did, DatasetID),
         ObservationID=ifelse(is.na(ObservationID), ob, ObservationID)) %>% 
  select(-ob, -did) %>% 
  unique()


#making sure there is just on measurement per variable.  
cont_traits5<-cont_traits4%>%
  mutate(present=1)%>%
  group_by(DatasetID, ObservationID, species_matched, CleanTraitName)%>%
  summarise(n=sum(present))

#prob obs
repeats<-cont_traits4 %>% 
  group_by(species_matched, CleanTraitName, StdValue) %>% 
  summarize(n=length(StdValue)) %>% 
  filter(n>1)


#troubleshooting the problems
probtraits<-subset(cont_traits5, n>1)%>%
  ungroup()%>%
  select(CleanTraitName, n)%>%
  unique()
  #spread(CleanTraitName, StdValue)

ttraits<-cont_traits4%>%
  ungroup()%>%
  group_by(DatasetID, ObservationID, family, genus, species_matched)%>%
  spread(CleanTraitName, StdValue, fill=NA)
  

write.csv(ttraits, "C:\\Users\\mavolio2\\Dropbox\\sDiv_sCoRRE_shared\\Trait Data\\TRY Data\\TRY Continuous data/TRY_trait_data_continuous_Nov2021.csv", row.names = F)
write.csv(cont_traits4, "C:\\Users\\mavolio2\\Dropbox\\sDiv_sCoRRE_shared\\Trait Data\\TRY Data\\TRY Continuous data/TRY_trait_data_continuous_long_Nov2021.csv", row.names = F)
