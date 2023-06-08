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
GExSpecies <- read.csv('OriginalData\\Traits\\GEx_species_tree_complete.csv') %>% 
  select(family, species_matched) %>% 
  unique()

names <- rbind(correSpecies, GExSpecies) %>% 
  unique() %>% 
  mutate(drop=ifelse(species_matched=='Dianella longifolia'&family=='Xanthorrhoeaceae', 1, 
              ifelse(species_matched=='Lancea tibetica'&family=='Phrymaceae', 1, 0))) %>% 
  filter(drop==0) %>% 
  select(-drop)

# AusTraits
AusTraits <- read.csv('OriginalData\\Traits\\AusTraits_2022\\AusTraits_CoRRE_May2023.csv') %>%
  left_join(names) %>%
  mutate(species_matched2=species_matched)%>%
  separate(species_matched2, into=c('genus', 'species'))%>%
  mutate(DatabaseID='AusTraits')%>%
  select(DatabaseID, DatasetID, ObservationID, family, species_matched, genus, CleanTraitName, StdValue) %>% 
  filter(StdValue>0)

# TRY
TRY <- read.csv('OriginalData\\Traits\\TRY\\TRY_trait_data_continuous_long_May2023.csv') %>% 
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

allTraits_wide <- allTraits %>% 
  pivot_wider(names_from = CleanTraitName, values_from = StdValue, values_fill =NA)

ntraits <- length(unique(allTraits$CleanTraitName))
miss <- sum(is.na(allTraits_wide))
total <- nrow(allTraits_wide)*ntraits
miss/total*100

spnum <- length(unique(allTraits_wide$species_matched))
#originally were missing 96.6% of data for all species

# Drop traits that are related to physiology, water content, different ways of measuring SLA and leaf area, and root nutrients
#Final option is SLA (3115, 3117), LDMC, LA (3108:3114), leaf mass, seed dry mass and plant veg, SRL (614), and leaf N [14 traits total to impute 7 traits]
allTraits_sub <- allTraits %>% 
  filter(CleanTraitName %in% c('SLA', 3115, 3117, 'LDMC', 'leaf_area', 3109, 3114, 'leaf_dry_mass', 'seed_dry_mass', 'plant_height_vegetative', 'SRL', 614, 'leaf_N'))

#make wide to sum NA
allTraits_sub_wide <- allTraits_sub %>% 
  pivot_wider(names_from = CleanTraitName, values_from = StdValue, values_fill =NA)

ntraits <- length(unique(allTraits_sub$CleanTraitName))
miss <- sum(is.na(allTraits_sub_wide))
total <- nrow(allTraits_sub_wide)*ntraits
miss/total*100

spnum <- length(unique(allTraits_sub_wide$species_matched)) 
# after selecting a subset of the traits, we are now missing 88.2% of data for 3193 species

# label <- allTraits_sub %>% 
#   group_by(CleanTraitName, DatabaseID) %>% 
#   summarise(length=length(StdValue)) %>% 
#   ungroup() %>% 
#   group_by(CleanTraitName) %>% 
#   mutate(length2=sum(length)) %>% 
#   ungroup() %>% 
#   pivot_longer(cols=length:length2, names_to='name', values_to='length') %>% 
#   mutate(DatabaseID=ifelse(name=='length2', 'total', DatabaseID)) %>% 
#   unique()
# 
# # How many observations do we have for each trait across our database?
# ggplot(data=label, aes(x=DatabaseID, y=length, label=length, fill=DatabaseID)) +
#   geom_bar(stat='identity', position=position_dodge()) +
#   geom_text() +
#   geom_hline(yintercept=254440*.2) + # 20% of observations missing any given trait
#   geom_hline(yintercept=254440*.1, color='red') + # 10% of observations missing any given trait
#   facet_wrap(~CleanTraitName, ncol=10) +
#   scale_x_discrete(breaks=c("AusTraits", "BIEN", "CPTD2", "TIPleaf", "TRY", "total"),
#                    limits=c("AusTraits", "BIEN", "CPTD2", "TIPleaf", "TRY", "total"),
#                    labels=c("A", "B", "C", "TIP", "TRY", 'tot')) +
#   scale_fill_manual(values=c('#4E3686', '#5DA4D9', '#80D87F', '#FED23F', '#EE724C', 'darkgrey')) +
#   theme(legend.position='none')

# Are there any outlier datasets for each trait?
ggplot(data=allTraits_sub, aes(x=DatabaseID, y=StdValue)) +
  scale_y_log10() + # note log axis!
  geom_jitter(aes(color=DatabaseID)) +
  geom_boxplot(color='black', alpha=0) +
  facet_wrap(~CleanTraitName, scales='free_y', ncol=4) +
  scale_x_discrete(breaks=c("AusTraits", "BIEN", "CPTD2", "TIPleaf", "TRY"),
                   labels=c("A", "B", "C", "TIP", "TRY")) +
  scale_color_manual(values=c('#4E3686', '#5DA4D9', '#80D87F', '#FED23F', '#EE724C')) +
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        legend.position='top') 

# ggsave('C:\\Users\\kjkomatsu\\Dropbox (Smithsonian)\\working groups\\CoRRE\\sDiv\\sDiv_sCoRRE_shared\\DataPaper\\2023_sCoRRE_traits\\figures\\Fig 1_input traits histograms.png', width=7.5, height=10, units='in', dpi=300, bg='white')


# Transpose to wide format for gap filling.
talltraits <- allTraits_sub %>% 
  group_by(DatabaseID, DatasetID, ObservationID, family, genus, species_matched) %>%
  pivot_wider(names_from=CleanTraitName, values_from=StdValue, values_fill=NA) %>% 
  ungroup()

# write.csv(allTraits_sub, 'OriginalData\\Traits\\raw traits for gap filling\\TRYAusBIEN_continuous_June2023_long.csv', row.names = F)

# write.csv(talltraits, 'OriginalData\\Traits\\raw traits for gap filling\\TRYAusBIEN_continuous_June2023.csv', row.names = F)

##checking traits
test <- allTraits_sub %>% 
  group_by(species_matched, CleanTraitName, StdValue, DatabaseID) %>% 
  summarize(n=length(StdValue)) %>% 
  ungroup() %>% 
  filter(n>1)

sum(test[,'n'])

## all databases have repeats - 118,536 across all data
## 12,758 are the same values repeated 10 or more times and were designated as keepers from cleaning code

sppLength <- talltraits %>% 
  select(species_matched) %>% 
  unique()
# 3193 species

multiTraitInd <- allTraits_sub %>% 
  group_by(DatabaseID, DatasetID, ObservationID, species_matched) %>% 
  summarise(num_traits=length(CleanTraitName)) %>% 
  ungroup() # %>%
  # filter(num_traits>1)
# 254,440 individuals measured (some have more than 1 trait measured on the same individual)
# 69,863 individuals have more than 1 trait measured on the same individual

ggplot(data=multiTraitInd, aes(x=num_traits)) +
  geom_histogram(binwidth = 1) +
  xlab('Number of Traits per Individual') + ylab('Number of Individuals') +
  scale_y_break(c(40000, 160000), ticklabels=c(170000, 180000))

# ggsave('C:\\Users\\kjkomatsu\\Dropbox (Smithsonian)\\working groups\\CoRRE\\sDiv\\sDiv_sCoRRE_shared\\DataPaper\\2023_sCoRRE_traits\\figures\\Fig 2_traits per individual histogram.png', width=8, height=8, units='in', dpi=300, bg='white')

# Which families have very little observed data going into the gap filling methods?
traitMeasured <- allTraits_sub %>% 
  mutate(present=1) %>% 
  group_by(DatabaseID, DatasetID, ObservationID, family, genus, species_matched) %>%
  pivot_wider(names_from=CleanTraitName, names_prefix="X", values_from=present, values_fill=0) %>% 
  ungroup()

familyComplete <- traitMeasured %>% 
  group_by(family) %>% 
  summarize(across(Xseed_dry_mass:X3114, mean)) %>% 
  pivot_longer(Xseed_dry_mass:X3114, names_to="trait", values_to = "value") %>% 
  mutate(traitpresent=ifelse(value>0, 1, 0)) %>% 
  group_by(family) %>% 
  summarise(ntraits=sum(traitpresent)) %>% 
  mutate(percenttraits=(ntraits/12))

# write.csv(familyComplete, "CompiledData\\TraitCompletnessbyFamily.csv", row.names = F)  