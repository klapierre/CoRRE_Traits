################################################################################
##  merge_all_trait_databases.R: Putting together data from TRY, BIEN, and AusTraits.
##
##  Authors: Kimberly Komatsu, Meghan Avolio
################################################################################


library(scales)
library(tidyverse)
library(ggbreak) 

setwd('C:\\Users\\mavolio2\\Dropbox\\CoRRE_database\\Data') #meghan's
setwd('C:\\Users\\kjkomatsu\\Dropbox (Smithsonian)\\working groups\\CoRRE\\CoRRE_database\\Data') #kim's

theme_set(theme_bw())
theme_update(axis.title.x=element_text(size=20, vjust=-0.35, margin=margin(t=15)), axis.text.x=element_text(size=16),
             axis.title.y=element_text(size=20, angle=90, vjust=0.5, margin=margin(r=15)), axis.text.y=element_text(size=16),
             plot.title = element_text(size=24, vjust=2),
             panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
             legend.title=element_blank(), legend.text=element_text(size=20))


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
  unique() %>% 
  mutate(drop=ifelse(species_matched=='Dianella longifolia'&family=='Xanthorrhoeaceae', 1, 
              ifelse(species_matched=='Lancea tibetica'&family=='Phrymaceae', 1, 0))) %>% 
  filter(drop==0) %>% 
  select(-drop)

# AusTraits
AusTraits <- read.csv('OriginalData\\Traits\\AusTraits_2022\\AusTraits_CoRRE_Oct2023.csv') %>%
  left_join(names) %>%
  mutate(species_matched2=species_matched)%>%
  separate(species_matched2, into=c('genus', 'species'))%>%
  mutate(DatabaseID='AusTraits')%>%
  select(DatabaseID, DatasetID, ObservationID, family, species_matched, genus, CleanTraitName, StdValue) %>% 
  filter(StdValue>0)

# TRY
TRY <- read.csv('OriginalData\\Traits\\TRY\\TRY_trait_data_continuous_long_Oct2023.csv') %>% 
  mutate(DatabaseID="TRY") %>% 
  select(DatabaseID, DatasetID, ObservationID, family, species_matched, genus, CleanTraitName, StdValue) %>% 
  filter(StdValue>0)

# BIEN - NOTE: photosynthetic rate, stomatal conductance, stem specific density were dropped in the BIEN cleaning file because they were out of line with the TRY trait values
BIEN <- read.csv('OriginalData\\Traits\\BIEN\\BIEN_for_scorre_202310263.csv') %>% 
  left_join(names) %>%
  select(DatabaseID, DatasetID, ObservationID, family, species_matched, genus, CleanTraitName, StdValue) %>% 
  filter(StdValue>0)

# TiP leaf
TiP <- read.csv('OriginalData\\Traits\\TiP_leaf\\TiP_leaf_Oct2023.csv') %>% 
  separate(col=species_matched, into=c('genus', 'species'), sep=' ', remove=F) %>% 
  left_join(names) %>% 
  select(DatabaseID, DatasetID, ObservationID, family, genus, species_matched, CleanTraitName, StdValue) %>% 
  filter(StdValue>0)

# China Plant Trait Database 2
CPTD2 <- read.csv('OriginalData\\Traits\\ChinaPlant2\\CPTD2_June2023.csv') %>% 
  separate(col=species_matched, into=c('genus', 'species'), sep=' ', remove=F) %>% 
  select(DatabaseID, DatasetID, ObservationID, family, genus, species_matched, CleanTraitName, StdValue) %>% 
  filter(StdValue>0)

# find ferns and lycophytes
growthForm <- read.csv("CleanedData\\Traits\\complete categorical traits\\sCoRRE categorical trait data_12142022.csv") %>% 
  select(species_matched, growth_form)

# Bind all together
allTraits <- rbind(TRY, AusTraits, BIEN, TiP, CPTD2) %>% 
  left_join(growthForm) %>% 
  select(DatabaseID, DatasetID, ObservationID, family, genus, species_matched, CleanTraitName, StdValue)

allTraits_wide <- allTraits %>% 
  pivot_wider(names_from = CleanTraitName, values_from = StdValue, values_fill =NA)

ntraits <- length(unique(allTraits$CleanTraitName))
miss <- sum(is.na(allTraits_wide))
total <- nrow(allTraits_wide)*ntraits
miss/total*100

spnum <- length(unique(allTraits_wide$species_matched))
famnum <- length(unique(allTraits_wide$family))
# originally were missing 96.6% of data for all species, now with only traits of interest we are missing 88.54% of data

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
  mutate(CleanTraitName2=
                         # ifelse(CleanTraitName==3108, 'LA (leaf, -petiole)',
                         ifelse(CleanTraitName==3109, 'Leaf Area (leaflet, -petiole)',
                         # ifelse(CleanTraitName==3111, 'LA (leaflet, +petiole)',
                         # ifelse(CleanTraitName==3112, 'LA (leaf, undefined)',
                         # ifelse(CleanTraitName==3113, 'LA (leaflet, undefined)',
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
  scale_y_continuous(labels = label_comma()) +
  scale_x_discrete(breaks=c("AusTraits", "BIEN", "CPTD2", "TIPleaf", "TRY", "total"),
                   limits=c("AusTraits", "BIEN", "CPTD2", "TIPleaf", "TRY", "total"),
                   labels=c("Au", "BN", "C2", "TP", "TY", 'all')) +
  scale_fill_manual(values=c('#4E3686', '#5DA4D9', '#80D87F', '#FED23F','darkgrey', '#EE724C'))+
  theme(strip.text.x = element_text(size = 18),
        axis.title.x=element_text(size=24, vjust=-0.35, margin=margin(t=15)), axis.text.x=element_text(size=22),
        axis.title.y=element_text(size=24, angle=90, vjust=0.5, margin=margin(r=15)), axis.text.y=element_text(size=22),
        legend.position='none') +
  ylab('Number of Observations') + xlab('Database ID')
# ggsave('C:\\Users\\kjkomatsu\\Dropbox (Smithsonian)\\working groups\\CoRRE\\sDiv\\sDiv_sCoRRE_shared\\DataPaper\\2023_sCoRRE_traits\\figures\\Fig 3_input percent complete_20231006.png', width=17, height=19, units='in', dpi=300, bg='white')

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
# ggsave('C:\\Users\\kjkomatsu\\Dropbox (Smithsonian)\\working groups\\CoRRE\\sDiv\\sDiv_sCoRRE_shared\\DataPaper\\2023_sCoRRE_traits\\figures\\Fig x_input traits histograms.png', width=7.5, height=10, units='in', dpi=300, bg='white')


# Transpose to wide format for gap filling.
talltraits <- allTraits %>% 
  group_by(DatabaseID, DatasetID, ObservationID, family, genus, species_matched) %>%
  pivot_wider(names_from=CleanTraitName, values_from=StdValue, values_fill=NA) %>% 
  ungroup()

# write.csv(allTraits, 'OriginalData\\Traits\\raw traits for gap filling\\TRYAusBIEN_continuous_Oct2023_long.csv', row.names = F)

# write.csv(talltraits, 'OriginalData\\Traits\\raw traits for gap filling\\TRYAusBIEN_continuous_Oct2023.csv', row.names = F)

##checking traits
test <- allTraits %>% 
  group_by(species_matched, CleanTraitName, StdValue, DatabaseID) %>% 
  summarize(n=length(StdValue)) %>% 
  ungroup() %>% 
  filter(n>1)

sum(test[,'n'])

multiTraitInd <- allTraits %>% 
  group_by(DatabaseID, DatasetID, ObservationID, species_matched) %>% 
  summarise(num_traits=length(CleanTraitName)) %>% 
  ungroup() # %>%
  # filter(num_traits>1)
# 205,923 individuals measured (some have more than 1 trait measured on the same individual)
# 51,172 individuals have more than 1 trait measured on the same individual

ggplot(data=multiTraitInd, aes(x=num_traits)) +
  geom_histogram(binwidth = 1) +
  xlab('Number of Traits per Individual') + ylab('Number of Individuals') +
  scale_y_break(c(40000, 130000), ticklabels=c(140000, 150000))

# ggsave('C:\\Users\\kjkomatsu\\Dropbox (Smithsonian)\\working groups\\CoRRE\\sDiv\\sDiv_sCoRRE_shared\\DataPaper\\2023_sCoRRE_traits\\figures\\Fig 2_traits per individual histogram_20231006.png', width=8, height=8, units='in', dpi=300, bg='white')

# Which families have very little observed data going into the gap filling methods?
traitMeasured <- allTraits %>% 
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