################################################################################
##  merge_all_trait_databases.R: Putting together data from TRY, BIEN, and AusTraits.
##
##  Authors: Kimberly Komatsu, Meghan Avolio
################################################################################

library(tidyverse)
library(ggbreak) 

setwd('C:\\Users\\mavolio2\\Dropbox\\CoRRE_database\\Data') #meghan's
setwd('C:\\Users\\kjkomatsu\\Dropbox (Smithsonian)\\working groups\\CoRRE\\CoRRE_database\\Data') #kim's


#### Read in data ####

# Cleaned species names
# Import CoRRE species names
correSpecies <- read.csv("CompiledData\\Species_lists\\FullList_Nov2021.csv") %>%  #species names are standardized
  left_join(read.csv("CompiledData\\Species_lists\\species_families_trees_2021.csv")) %>% 
  filter(tree.non.tree != "tree") %>% #Remove trees
  separate(species_matched, into=c('genus', 'species', 'subspp'), sep=' ') %>% 
  filter(species!='sp.') %>% 
  unite(col='species_matched', genus:species, sep=' ', remove=T) %>% 
  select(family, species_matched) %>% 
  unique()

# Import GEx species names
GExSpecies <- read.csv('OriginalData\\Traits\\GEx_species_family_May2023.csv') %>% 
  select(family, species_matched) %>% 
  unique()

names <- rbind(correSpecies, GExSpecies) %>% 
  unique()

# AusTraits
AusTraits <- read.csv('OriginalData\\Traits\\AusTraits_2022\\AusTraits_CoRRE_May2023.csv') %>%
  left_join(names) %>%
  mutate(species_matched2=species_matched)%>%
  separate(species_matched2, into=c('genus', 'species'))%>%
  mutate(DatabaseID='AusTraits')%>%
  select(DatabaseID, DatasetID, ObservationID, family, species_matched, genus, CleanTraitName, StdValue) %>% 
  filter(StdValue>0)

# TRY
TRY <- read.csv('OriginalData\\Traits\\TRY\\old files\\TRY_trait_data_continuous_long_March2023.csv') %>% 
  mutate(DatabaseID="TRY") %>% 
  filter(StdValue>0)

# BIEN - NOTE: photosynthetic rate, stomatal conductence, stem specific density were dropped in the BIEN cleaning file because they were out of line with the TRY trait values
BIEN <- read.csv('OriginalData\\Traits\\BIEN\\BIEN_for_scorre_20230511.csv') %>% 
  left_join(names) %>%
  select(DatabaseID, DatasetID, ObservationID, family, species_matched, genus, CleanTraitName, StdValue) %>% 
  filter(StdValue>0)

# TiP leaf
TiP <- read.csv('OriginalData\\Traits\\TiP_leaf\\TiP_leaf_May2023.csv') %>% 
  separate(col=species_matched, into=c('genus', 'species'), sep=' ', remove=F) %>% 
  left_join(names) %>% 
  select(DatabaseID, DatasetID, ObservationID, family, genus, species_matched, CleanTraitName, StdValue) %>% 
  filter(StdValue>0)

# China Plant Trait Database 2
CPTD2 <- read.csv('OriginalData\\Traits\\ChinaPlant2\\CPTD2_May2023.csv') %>% 
  separate(col=species_matched, into=c('genus', 'species'), sep=' ', remove=F) %>% 
  select(DatabaseID, DatasetID, ObservationID, family, genus, species_matched, CleanTraitName, StdValue) %>% 
  filter(StdValue>0)

#find ferns and lycophytes
growthForm <- read.csv("CleanedData\\Traits\\complete categorical traits\\sCoRRE categorical trait data_12142022.csv") %>% 
  select(species_matched, growth_form)

# Bind all together
allTraits <- rbind(TRY, AusTraits, BIEN, TiP, CPTD2) %>% 
  left_join(growthForm) %>% 
  filter(!(growth_form %in% c('fern', 'lycophyte'))) %>% 
  select(DatabaseID, DatasetID, ObservationID, family, genus, species_matched, CleanTraitName, StdValue)

allTraits_wide<-allTraits %>% 
  pivot_wider(names_from = CleanTraitName, values_from = StdValue, values_fill =NA)

ntraits<-length(unique(allTraits$CleanTraitName))
miss<-sum(is.na(allTraits_wide))
total<-nrow(allTraits_wide)*ntraits
miss/total*100

spnum<-length(unique(allTraits_wide$species_matched))

#originally were missing 96% of data for 1876 species

#drop traits that are related to physiology, water content, and different ways of measuring SLA and Leaf area and root nutrients
#Final option is SLA (3115, 3116), LDMC, LA (3108:3114), leaf mass, seed dry mass and plant veg, SRL (614), and leaf N ()
allTraits_sub<-allTraits %>% 
  filter(!(CleanTraitName %in% c(106, "Vc_max", "J_max", "dark_resp_rate", 3120, 3122, 3121, "leaf_transp_rate", 185, "photosynthesis_rate", "stomatal_conductance", 185, 270, 40)),#phy water content
           !(CleanTraitName %in% c(3108, 3109, 3111, 3112, 3113)),#other ways of measuring SLA, LA
           !(CleanTraitName %in% c(475, "root_C", "root_N", "root_P", 1781)), #root traits
 !(CleanTraitName %in% c("leaf_K", 52, "seed_terminal_velocity", "leaf_longevity", "stem_spec_density", 1104, 57, 58, 51, "leaf_N:P", "leaf_P", 51, "leaf_density", "leaf_thickness", "RGR", "leaf_width", "seed_length", "leaf_C:N", "seed_number", "leaf_C", 570)),#traits with low coverage
 !(CleanTraitName %in% c("root_density", "root_diameter", 'rooting_depth', "root:shoot", "root_dry_mass")))#rest of root traits


#make wide to sum NA
allTraits_sub_wide<-allTraits_sub %>% 
  pivot_wider(names_from = CleanTraitName, values_from = StdValue, values_fill =NA)

ntraits<-length(unique(allTraits_sub$CleanTraitName))
miss<-sum(is.na(allTraits_sub_wide))
total<-nrow(allTraits_sub_wide)*ntraits
miss/total*100

spnum<-length(unique(allTraits_sub_wide$species_matched))
####with out subset we are now missing 87% of data for 1852 species

label <- allTraits %>% 
  group_by(CleanTraitName, DatabaseID) %>% 
  summarise(length=length(StdValue)) %>% 
  ungroup() %>% 
  group_by(CleanTraitName) %>% 
  mutate(length2=sum(length)) %>% 
  ungroup() %>% 
  # filter(CleanTraitName %in% c('dark_resp_rate', 'J_max', 'LDMC', 'leaf_area', 'leaf_C', 'leaf_C:N',
                               #                                                     'leaf_density', 'leaf_dry_mass', 'leaf_K', 'leaf_longevity', 'leaf_N',
                               #                                                     'leaf_N:P', 'leaf_P', 'leaf_thickness', 'leaf_transp_rate', 'leaf_width',
                               #                                                     'photosynthesis_rate', 'plant_height_vegetative', 'RGR', 'root:shoot',
                               #                                                     'root_C', 'root_density', 'root_diameter', 'root_dry_mass', 'root_N',
                               #                                                     'root_P', 'rooting_depth', 'seed_dry_mass', 'seed_length', 'seed_number',
                               #                                                     'seed_terminal_velocity', 'SLA', 'SRL', 'stem_spec_density',
                               # 'stomatal_conductance', 'Vc_max')) %>% 
  pivot_longer(cols=length:length2, names_to='name', values_to='length') %>% 
  mutate(DatabaseID=ifelse(name=='length2', 'total', DatabaseID)) %>% 
  unique()

ggplot(data=label, aes(x=DatabaseID, y=length, label=length, fill=DatabaseID)) +
  geom_bar(stat='identity', position=position_dodge()) +
  geom_text() +
  geom_hline(yintercept=23068) +
  geom_hline(yintercept=11534, color='red') +
  facet_wrap(~CleanTraitName, ncol=10) +
  scale_x_discrete(breaks=c("AusTraits", "BIEN", "CPTD2", "TIPleaf", "TRY", "total"),
                   limits=c("AusTraits", "BIEN", "CPTD2", "TIPleaf", "TRY", "total"),
                   labels=c("A", "B", "C", "TIP", "TRY", 'tot')) +
  scale_fill_manual(values=c('#4E3686', '#5DA4D9', '#80D87F', '#FED23F', '#EE724C', 'darkgrey')) +
  theme(legend.position='none')


# Are there any outlier datasets for each trait?
ggplot(data=subset(allTraits, CleanTraitName %in% c('dark_resp_rate', 'J_max', 'LDMC', 'leaf_area', 'leaf_C', 'leaf_C:N',
#                                                     'leaf_density', 'leaf_dry_mass', 'leaf_K', 'leaf_longevity', 'leaf_N', 
#                                                     'leaf_N:P', 'leaf_P', 'leaf_thickness', 'leaf_transp_rate', 'leaf_width',
#                                                     'photosynthesis_rate', 'plant_height_vegetative', 'RGR', 'root:shoot', 
#                                                     'root_C', 'root_density', 'root_diameter', 'root_dry_mass', 'root_N', 
#                                                     'root_P', 'rooting_depth', 'seed_dry_mass', 'seed_length', 'seed_number',
#                                                     'seed_terminal_velocity', 'SLA', 'SRL', 'stem_spec_density',
                                                    'stomatal_conductance', 'Vc_max')),
       aes(x=DatabaseID, y=StdValue)) +
  geom_jitter(aes(color=DatabaseID)) +
  geom_boxplot(color='black', alpha=0) +
  facet_wrap(~CleanTraitName, scales='free', ncol=4) +
  scale_x_discrete(breaks=c("AusTraits", "BIEN", "CPTD2", "TIPleaf", "TRY"),
                   labels=c("A", "B", "C", "TIP", "TRY")) +
  scale_color_manual(values=c('#4E3686', '#5DA4D9', '#80D87F', '#FED23F', '#EE724C')) +
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        legend.position='top') 

# ggsave('C:\\Users\\kjkomatsu\\Dropbox (Smithsonian)\\working groups\\CoRRE\\sDiv\\sDiv_sCoRRE_shared\\DataPaper\\2023_sCoRRE_traits\\figures\\Fig 1_input traits histograms.png', width=7.5, height=10, units='in', dpi=300, bg='white')


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
talltraits <- allTraits_sub %>% 
  group_by(DatabaseID, DatasetID, ObservationID, family, genus, species_matched) %>%
  pivot_wider(names_from=CleanTraitName, values_from=StdValue, values_fill=NA) %>% 
  ungroup()

# write.csv(allTraits_sub, 'OriginalData\\Traits\\raw traits for gap filling\\TRYAusBIEN_continuous_May2023_long.csv', row.names = F)

# write.csv(talltraits, 'OriginalData\\Traits\\raw traits for gap filling\\TRYAusBIEN_continuous_May2023.csv', row.names = F)

##checking traits
test <- allTraits %>% 
  group_by(species_matched, CleanTraitName, StdValue, DatabaseID) %>% 
  summarize(n=length(StdValue)) %>% 
  ungroup() %>% 
  filter(n>10)

sum(test[,'n'])

## all databases have repeats - 26,909 across all data
## only 2298 are the same values repeated 10 or more times and were designated as keepers from cleaning code

sppLength <- talltraits %>% 
  select(species_matched) %>% 
  unique()

multiTraitInd <- allTraits %>% 
  group_by(DatabaseID, DatasetID, ObservationID, species_matched) %>% 
  summarise(num_traits=length(CleanTraitName)) %>% 
  ungroup() # %>%
  # filter(num_traits>1)

ggplot(data=multiTraitInd, aes(x=num_traits)) +
  geom_histogram(binwidth = 1) +
  xlab('Number of Traits per Individual') + ylab('Number of Individuals') +
  scale_y_break(c(20000, 59000), ticklabels=c(60000))

# ggsave('C:\\Users\\kjkomatsu\\Dropbox (Smithsonian)\\working groups\\CoRRE\\sDiv\\sDiv_sCoRRE_shared\\DataPaper\\2023_sCoRRE_traits\\figures\\Fig 2_traits per individual histogram.png', width=8, height=8, units='in', dpi=300, bg='white')

##Trying to figure out which families have very little observed data going into the gap filling methods

traitmeasured <- allTraits_sub %>% 
  mutate(present=1) %>% 
  group_by(DatabaseID, DatasetID, ObservationID, family, genus, species_matched) %>%
  pivot_wider(names_from=CleanTraitName, names_prefix="X", values_from=present, values_fill=0) %>% 
  ungroup()

familycomplete<-traitmeasured %>% 
  group_by(family) %>% 
  summarize(across(Xseed_dry_mass:X3114, mean)) %>% 
  pivot_longer(Xseed_dry_mass:X3114, names_to="trait", values_to = "value") %>% 
  mutate(traitpresent=ifelse(value>0, 1, 0)) %>% 
  group_by(family) %>% 
  summarise(ntraits=sum(traitpresent)) %>% 
  mutate(percenttraits=(ntraits/12))

write.csv(familycomplete, "CompiledData\\TraitCompletnessbyFamily.csv", row.names = F)  
