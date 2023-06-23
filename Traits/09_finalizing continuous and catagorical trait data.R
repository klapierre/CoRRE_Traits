################################################################################
##  finalizing continuous and categorical traits.R: Checking imputed continuous data and gathered categorical data.
##
##  Authors: Kimberly Komatsu, Meghan Avolio, Kevin Wilcox
################################################################################

#### Set up working space ####

# rm(list=ls()) clean up workspace
#library(FD)
library(PerformanceAnalytics)
# library(ggforce)
library(tidyverse)

setwd('C:\\Users\\kjkomatsu\\Dropbox (Smithsonian)\\working groups\\CoRRE\\CoRRE_database\\Data') #Kim's
# setwd("C:\\Users\\wilco\\Dropbox\\shared working groups\\sDiv_sCoRRE_shared\\CoRRE data\\") # Kevin's laptop wd

theme_set(theme_bw())
theme_update(axis.title.x=element_text(size=20, vjust=-0.35, margin=margin(t=15)), axis.text.x=element_text(size=16),
             axis.title.y=element_text(size=20, angle=90, vjust=0.5, margin=margin(r=15)), axis.text.y=element_text(size=16),
             plot.title = element_text(size=24, vjust=2),
             panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
             legend.title=element_blank(), legend.text=element_text(size=20))


#### Categorical trait data ####
categoricalTraits <- read.csv("CleanedData\\Traits\\complete categorical traits\\sCoRRE categorical trait data_12142022.csv") %>%
  dplyr::select(family, species_matched, leaf_type, leaf_compoundness, stem_support, growth_form, photosynthetic_pathway, lifespan,  clonal, mycorrhizal_type, n_fixation, rhizobial, actinorhizal) %>%
  mutate(photosynthetic_pathway = replace(photosynthetic_pathway, grep("possible", photosynthetic_pathway), NA)) %>%
  mutate(clonal = replace(clonal, clonal=="uncertain", NA)) %>%
  mutate(mycorrhizal_type = replace(mycorrhizal_type, mycorrhizal_type=="uncertain", NA)) %>%
  mutate(lifespan = replace(lifespan, lifespan=="uncertain", NA)) %>%
  mutate(n_fixation_type=ifelse(rhizobial=='yes', 'rhizobial',
                         ifelse(actinorhizal=='yes', 'actinorhizal', 'none'))) %>% 
  filter(lifespan != "moss") %>% 
  select(-family, -n_fixation, -rhizobial, -actinorhizal)


# #### Testing out stream graphs ####
# categoricalTraitsGather <- categoricalTraits %>%
#   # filter(clonal!='NA') %>%
#   group_by(growth_form, leaf_type, leaf_compoundness) %>%
#   summarise(value=length(species_matched)) %>%
#   ungroup() %>%
#   gather_set_data(c(1:3))
# 
# ggplot(categoricalTraitsGather, aes(x, id = id, split = y, value = value)) +
#   geom_parallel_sets(aes(fill = growth_form), alpha = 0.3, axis.width = 0.1) +
#   geom_parallel_sets_axes(axis.width = 0.1) +
#   geom_parallel_sets_labels(colour = 'white')


#### Pie Charts for each categorical trait ####
# leaf type
leafType <- categoricalTraits %>% 
  group_by(leaf_type) %>%
  count() %>% 
  ungroup() %>% 
  mutate(proportion = round((n/sum(n)), digits=3)) %>% 
  arrange(proportion) %>%
  mutate(labels=scales::percent(proportion))

ggplot(leafType, aes(x="", y=proportion, fill=leaf_type)) +
  geom_col() +
  coord_polar(theta="y") +
  scale_fill_manual(values=c('#7DCBBB', '#FFFFA4', '#B0AAD1', '#F7695F', '#6EA1C9', '#FBA550', '#A5DA56', '#AD68AF'))

# ggsave('C:\\Users\\kjkomatsu\\Dropbox (Smithsonian)\\working groups\\CoRRE\\sDiv\\sDiv_sCoRRE_shared\\DataPaper\\2023_sCoRRE_traits\\figures\\pie chart\\4_leaf_type.png', width=8, height=8, units='in', dpi=300, bg='white')

# leaf compoundness
leafCompoundness <- categoricalTraits %>% 
  group_by(leaf_compoundness) %>%
  count() %>% 
  ungroup() %>% 
  mutate(proportion = round((n/sum(n)), digits=3)) %>% 
  arrange(proportion) %>%
  mutate(labels=scales::percent(proportion))

ggplot(leafCompoundness, aes(x="", y=proportion, fill=leaf_compoundness)) +
  geom_col() +
  coord_polar(theta="y") +
  scale_fill_manual(values=c('#7DCBBB', '#FFFFA4', '#B0AAD1', '#F7695F', '#6EA1C9', '#FBA550', '#A5DA56', '#AD68AF'))

# ggsave('C:\\Users\\kjkomatsu\\Dropbox (Smithsonian)\\working groups\\CoRRE\\sDiv\\sDiv_sCoRRE_shared\\DataPaper\\2023_sCoRRE_traits\\figures\\pie chart\\5_leaf_compoundness.png', width=8, height=8, units='in', dpi=300, bg='white')

# stem support
stemSupport <- categoricalTraits %>% 
  group_by(stem_support) %>%
  count() %>% 
  ungroup() %>% 
  mutate(proportion = round((n/sum(n)), digits=3)) %>% 
  arrange(proportion) %>%
  mutate(labels=scales::percent(proportion))

ggplot(stemSupport, aes(x="", y=proportion, fill=stem_support)) +
  geom_col() +
  coord_polar(theta="y") +
  scale_fill_manual(values=c('#7DCBBB', '#FFFFA4', '#B0AAD1', '#F7695F', '#6EA1C9', '#FBA550', '#A5DA56', '#AD68AF'))

# ggsave('C:\\Users\\kjkomatsu\\Dropbox (Smithsonian)\\working groups\\CoRRE\\sDiv\\sDiv_sCoRRE_shared\\DataPaper\\2023_sCoRRE_traits\\figures\\pie chart\\6_stem_support.png', width=8, height=8, units='in', dpi=300, bg='white')

# growth form
growthForm <- categoricalTraits %>% 
  group_by(growth_form) %>%
  count() %>% 
  ungroup() %>% 
  mutate(proportion = round((n/sum(n)), digits=3)) %>% 
  arrange(proportion) %>%
  mutate(labels=scales::percent(proportion))

ggplot(growthForm, aes(x="", y=proportion, fill=growth_form)) +
  geom_col() +
  coord_polar(theta="y") +
  scale_fill_manual(values=c('#7DCBBB', '#FFFFA4', '#B0AAD1', '#F7695F', '#6EA1C9', '#FBA550', '#A5DA56', '#AD68AF'))

# ggsave('C:\\Users\\kjkomatsu\\Dropbox (Smithsonian)\\working groups\\CoRRE\\sDiv\\sDiv_sCoRRE_shared\\DataPaper\\2023_sCoRRE_traits\\figures\\pie chart\\1_growth_form.png', width=8, height=8, units='in', dpi=300, bg='white')

# photosynthetic pathway
photosyntheticPathway <- categoricalTraits %>% 
  group_by(photosynthetic_pathway) %>%
  count() %>% 
  ungroup() %>% 
  mutate(proportion = round((n/sum(n)), digits=3)) %>% 
  arrange(proportion) %>%
  mutate(labels=scales::percent(proportion))

ggplot(photosyntheticPathway, aes(x="", y=proportion, fill=photosynthetic_pathway)) +
  geom_col() +
  coord_polar(theta="y") +
  scale_fill_manual(values=c('#7DCBBB', '#FFFFA4', '#B0AAD1', '#F7695F', '#6EA1C9', '#FBA550', '#A5DA56', '#AD68AF'))

# ggsave('C:\\Users\\kjkomatsu\\Dropbox (Smithsonian)\\working groups\\CoRRE\\sDiv\\sDiv_sCoRRE_shared\\DataPaper\\2023_sCoRRE_traits\\figures\\pie chart\\7_photosynthetic_pathway.png', width=8, height=8, units='in', dpi=300, bg='white')

# lifespan
lifespan <- categoricalTraits %>% 
  group_by(lifespan) %>%
  count() %>% 
  ungroup() %>% 
  mutate(proportion = round((n/sum(n)), digits=3)) %>% 
  arrange(proportion) %>%
  mutate(labels=scales::percent(proportion))

ggplot(lifespan, aes(x="", y=proportion, fill=lifespan)) +
  geom_col() +
  coord_polar(theta="y") +
  scale_fill_manual(values=c('#7DCBBB', '#FFFFA4', '#B0AAD1', '#F7695F', '#6EA1C9', '#FBA550', '#A5DA56', '#AD68AF'))

# ggsave('C:\\Users\\kjkomatsu\\Dropbox (Smithsonian)\\working groups\\CoRRE\\sDiv\\sDiv_sCoRRE_shared\\DataPaper\\2023_sCoRRE_traits\\figures\\pie chart\\2_lifespan.png', width=8, height=8, units='in', dpi=300, bg='white')

# clonal
clonal <- categoricalTraits %>% 
  group_by(clonal) %>%
  count() %>% 
  ungroup() %>% 
  mutate(proportion = round((n/sum(n)), digits=3)) %>% 
  arrange(proportion) %>%
  mutate(labels=scales::percent(proportion))

ggplot(clonal, aes(x="", y=proportion, fill=clonal)) +
  geom_col() +
  coord_polar(theta="y") +
  scale_fill_manual(values=c('#7DCBBB', '#FFFFA4', '#B0AAD1', '#F7695F', '#6EA1C9', '#FBA550', '#A5DA56', '#AD68AF'))

# ggsave('C:\\Users\\kjkomatsu\\Dropbox (Smithsonian)\\working groups\\CoRRE\\sDiv\\sDiv_sCoRRE_shared\\DataPaper\\2023_sCoRRE_traits\\figures\\pie chart\\3_clonal.png', width=8, height=8, units='in', dpi=300, bg='white')

# mycorrhizal type
mycorrhizalType <- categoricalTraits %>% 
  group_by(mycorrhizal_type) %>%
  count() %>% 
  ungroup() %>% 
  mutate(proportion = round((n/sum(n)), digits=3)) %>% 
  arrange(proportion) %>%
  mutate(labels=scales::percent(proportion))

ggplot(mycorrhizalType, aes(x="", y=proportion, fill=mycorrhizal_type)) +
  geom_col() +
  coord_polar(theta="y") +
  scale_fill_manual(values=c('#7DCBBB', '#FFFFA4', '#B0AAD1', '#F7695F', '#6EA1C9', '#FBA550', '#A5DA56', '#AD68AF'))

# ggsave('C:\\Users\\kjkomatsu\\Dropbox (Smithsonian)\\working groups\\CoRRE\\sDiv\\sDiv_sCoRRE_shared\\DataPaper\\2023_sCoRRE_traits\\figures\\pie chart\\8_mycorrhizal_type.png', width=8, height=8, units='in', dpi=300, bg='white')

# n fixation type
nFixationType <- categoricalTraits %>% 
  group_by(n_fixation_type) %>%
  count() %>% 
  ungroup() %>% 
  mutate(proportion = round((n/sum(n)), digits=3)) %>% 
  arrange(proportion) %>%
  mutate(labels=scales::percent(proportion))

ggplot(nFixationType, aes(x="", y=proportion, fill=n_fixation_type)) +
  geom_col() +
  coord_polar(theta="y") +
  scale_fill_manual(values=c('#7DCBBB', '#FFFFA4', '#B0AAD1', '#F7695F', '#6EA1C9', '#FBA550', '#A5DA56', '#AD68AF'))

# ggsave('C:\\Users\\kjkomatsu\\Dropbox (Smithsonian)\\working groups\\CoRRE\\sDiv\\sDiv_sCoRRE_shared\\DataPaper\\2023_sCoRRE_traits\\figures\\pie chart\\9_n_fixation_type.png', width=8, height=8, units='in', dpi=300, bg='white')




#### Continuous traits ####

# Read species data to remove mosses
mossKey <- read.csv("CleanedData\\Traits\\complete categorical traits\\sCoRRE categorical trait data_12142022.csv") %>%
  dplyr::select(species_matched, leaf_type) %>%
  mutate(moss = ifelse(leaf_type=="moss", "moss","non-moss")) %>%
  dplyr::select(-leaf_type)


# Read in imputed trait data and bind on species information
## this is trait data without replacement (all imputed)
imputedRaw <- read.csv("CleanedData\\Traits\\gap filled continuous traits\\20230623_final\\imputed_traits_mice.csv") %>%
  bind_cols(read.csv('OriginalData\\Traits\\raw traits for gap filling\\TRYAusBIEN_continuous_June2023.csv')[,c('DatabaseID', 'DatasetID', 'ObservationID', 'family', 'genus', 'species_matched')]) %>%   
  left_join(mossKey) %>% 
  filter(moss!="moss") %>%
  dplyr::select(-moss) #removes 6 species observations

imputedLong <- imputedRaw %>% 
  pivot_longer(names_to='trait', values_to='imputed_value', seed_dry_mass:SRL)

# Read original trait data and join with imputed data
originalRaw <- read.csv('OriginalData\\Traits\\raw traits for gap filling\\TRYAusBIEN_continuous_June2023.csv') %>%
  pivot_longer(names_to='trait', values_to='original_value', seed_dry_mass:SRL) %>%
  na.omit()


# Join original trait data with imputed data. Only keep traits of interest.
allContinuous <- imputedLong %>% 
  left_join(originalRaw) %>% 
  filter(trait %in% c('LDMC', 'leaf_area', 'leaf_dry_mass', 'leaf_N', 'plant_height_vegetative', 'seed_dry_mass', 'SLA', 'SRL'))


allContinuousWide <- allContinuous %>% 
  select(-original_value) %>% 
  pivot_wider(names_from=trait, values_from=imputed_value)


# Calculate averages for each species
meanContinuous <- allContinuous %>% 
  group_by(family, species_matched, trait) %>% 
  summarize_at(.vars=c('imputed_value', 'original_value'),
               .funs=list(mean=mean, sd=sd),
               na.rm=T) %>% 
  ungroup()

speciesCount <- meanContinuous %>% 
  select(family, species_matched) %>% 
  unique() %>% 
  group_by(family) %>% 
  summarize(num_species=length(family)) %>% 
  ungroup() #116 families

sum(speciesCount$num_species)


# Compare imputed to original continuous trait data
ggplot(data=na.omit(meanContinuous), aes(x=original_value_mean, y=imputed_value_mean)) +
  geom_point() +
  geom_abline(slope=1) +
  facet_wrap(~trait, scales='free') +
  xlab('Mean Original Value') + ylab('Mean Imputed Value') +
  theme(strip.text.x = element_text(size = 12)) 
# ggsave('C:\\Users\\kjkomatsu\\Dropbox (Smithsonian)\\working groups\\CoRRE\\sDiv\\sDiv_sCoRRE_shared\\DataPaper\\2023_sCoRRE_traits\\figures\\Fig x_mean original v imputed_20230620.png', width=12, height=12, units='in', dpi=300, bg='white')

# Only grasses -- 11620 species
ggplot(data=na.omit(subset(allContinuous, family=='Poaceae')), aes(x=original_value, y=imputed_value)) +
  geom_point() +
  geom_abline(slope=1) +
  facet_wrap(~trait, scales='free')

# Only asters -- 11480
ggplot(data=na.omit(subset(allContinuous, family=='Asteraceae')), aes(x=original_value, y=imputed_value)) +
  geom_point() +
  geom_abline(slope=1) +
  facet_wrap(~trait, scales='free')

# Only legumes -- 4620
ggplot(data=na.omit(subset(allContinuous, family=='Fabaceae')), aes(x=original_value, y=imputed_value)) +
  geom_point() +
  geom_abline(slope=1) +
  facet_wrap(~trait, scales='free')

# Compare raw imputed and original data
ggplot(data=na.omit(allContinuous), aes(x=original_value, y=imputed_value)) +
  geom_point() +
  geom_abline(slope=1) +
  facet_wrap(~trait, scales='free') +
  xlab('Original Value') + ylab('Imputed Value') +
  theme(strip.text.x = element_text(size = 12)) 
# ggsave('C:\\Users\\kjkomatsu\\Dropbox (Smithsonian)\\working groups\\CoRRE\\sDiv\\sDiv_sCoRRE_shared\\DataPaper\\2023_sCoRRE_traits\\figures\\Fig x_pre cleaning original v imputed.png', width=12, height=12, units='in', dpi=300, bg='white')


allTogether <- allContinuous %>% 
  pivot_longer(imputed_value:original_value, names_to='data_type', values_to='trait_value') %>% 
  # mutate(data_type=ifelse(data_type=='original_value', DatabaseID, data_type)) %>% 
  na.omit()

# Look at boxplots for each trait
ggplot(data=allTogether, aes(x=data_type, y=trait_value)) +
  geom_boxplot() +
  facet_wrap(~trait, scales='free')


#### Clean imputed continuous trait data ####
# Checked to ensure no negative values (confirmed that there are none)

# Things that look problematic but Kim thinks are real: leaf_area (some palms with huge leaves), plant_height_vegetative (vines that have big big heights like Vitus sp and virginia creeper), seed number (consistently high numbers for some species that probably do have lots of seeds)

meanSD <- allContinuous %>% 
  group_by(trait) %>% 
  summarize(across('imputed_value', .fns=list(mean=mean, sd=sd))) %>% 
  ungroup()

meanSDSpecies <- allContinuous %>% 
  group_by(trait, species_matched) %>% 
  summarize(across('imputed_value', .fns=list(species_mean=mean, species_sd=sd, species_length=length))) %>% 
  ungroup()

##### START HERE: check if the code to make the plot header categories is working across this and the pivot step below
cleanContinuous <- allContinuous %>% 
  #calculate z-scores (error risk) for continuous traits 
  left_join(meanSD) %>% 
  left_join(meanSDSpecies) %>% 
  mutate(error_risk_overall=(imputed_value-imputed_value_mean)/imputed_value_sd) %>% 
  mutate(error_risk_species=(imputed_value-imputed_value_species_mean)/imputed_value_species_sd) %>% 
  filter(error_risk_overall<4) %>%  #drops 4369 observations (0.5% of data)
  filter(error_risk_species<4 & error_risk_species>(-4)) %>% #drops an additional 7626 observations (0.9% of data), all of which were from species with at least 20 observations for the given trait value being dropped 
  mutate(trait2=ifelse(trait=='leaf_area', 'Leaf Area (leaf, +petiole)',
                         ifelse(trait=='SLA', 'Specific Leaf Area (+petiole)', 
                         ifelse(trait=='SRL', 'Specific Root Length (all root)',
                         ifelse(trait=='leaf_N', 'Leaf N Content',
                         ifelse(trait=='plant_height_vegetative', 'Plant Vegetative Height',
                         ifelse(trait=='seed_dry_mass', 'Seed Dry Mass',
                         ifelse(trait=='leaf_dry_mass', 'Leaf Dry Mass',
                         ifelse(trait=='LDMC', 'Leaf Dry Matter Content',
                                trait)))))))))

cleanContinousWide <- cleanContinuous %>% 
  pivot_longer(cols=c('original_value', 'imputed_value'), names_to='data_type', values_to='trait_value') %>% 
  mutate(data_type2=ifelse(data_type=='original_value', DatabaseID, data_type)) %>% 
  na.omit()

cleanContinousWide$trait2 = factor(cleanContinousWide$trait2, levels=c('Leaf Area (leaf, +petiole)', 'Leaf Dry Mass', 'Leaf Dry Matter Content', 'Specific Leaf Area (+petiole)', 'Leaf N Content', 'Plant Vegetative Height', 'Specific Root Length (all root)', 'Seed Dry Mass'))

# Look at boxplots for each trait
ggplot(data=cleanContinousWide, aes(x=as.factor(data_type2), y=trait_value)) +
  # geom_jitter(aes(color=data_type)) +
  geom_boxplot(aes(color=data_type2)) +
  facet_wrap(~trait2, scales='free_y', ncol=3, labeller=label_wrap_gen(width=25)) +
  scale_x_discrete(breaks=c("AusTraits", "BIEN", "CPTD2", "TIPleaf", "TRY", "imputed_value"),
                   limits=c("AusTraits", "BIEN", "CPTD2", "TIPleaf", "TRY", "imputed_value"),
                   labels=c("Au", "BN", "C2", "TP", "TRY", "imp.")) +
  scale_color_manual(values=c('#4E3686', '#5DA4D9', '#80D87F', 'darkgrey', '#FED23F', '#EE724C')) +
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        legend.position='top') +
  xlab('Data Type') + ylab('Trait Value')  +
  scale_y_continuous(trans='log10')
# ggsave('C:\\Users\\kjkomatsu\\Dropbox (Smithsonian)\\working groups\\CoRRE\\sDiv\\sDiv_sCoRRE_shared\\DataPaper\\2023_sCoRRE_traits\\figures\\Fig 4_boxplots of original and imputed_20230608.png', width=14, height=15, units='in', dpi=300, bg='white')

# Look at boxplots for each trait
ggplot(data=subset(cleanContinousWide, species_matched %in% c('Helianthus maximiliani', 'Potentilla anserina', 'Clintonia borealis')), aes(x=species_matched, y=trait_value)) +
  geom_boxplot(aes(color=data_type)) +
  facet_wrap(~trait2, scales='free')

ggplot(data=subset(cleanContinousWide, species_matched=='Andropogon gerardii'), aes(x=data_type, y=trait_value)) +
  geom_boxplot() +
  facet_wrap(~trait2, scales='free')


# Look at boxplots for each trait
ggplot(data=subset(cleanContinuousFamilyRisk, family %in% c('Asteraceae', 'Frankeniaceae', 'Cactaceae', 'Malpighiaceae', 'Liliaceae')), aes(x=family, y=imputed_value)) +
  geom_boxplot() +
  facet_wrap(~trait2, scales='free') +
  scale_x_discrete(breaks=c("Asteraceae", "Frankeniaceae", "Cactaceae", "Malpighiaceae", "Liliaceae"),
                   limits=c("Asteraceae", "Frankeniaceae", "Cactaceae", "Malpighiaceae", "Liliaceae"),
                   labels=c("A", "F", "C", "M", "L")) 



cleanContinuous$trait2 = factor(cleanContinuous$trait2, levels=c('Leaf Area (leaf, +petiole)', 'Leaf Dry Mass', 'Leaf Dry Matter Content', 'Specific Leaf Area (+petiole)', 'Leaf N Content', 'Plant Vegetative Height', 'Specific Root Length (all root)', 'Seed Dry Mass'))


# Compare cleaned imputed and original data
ggplot(data=na.omit(cleanContinuous), aes(x=original_value, y=imputed_value)) +
  geom_point() +
  geom_abline(slope=1) +
  facet_wrap(~trait2, scales='free', ncol=3, labeller=label_wrap_gen(width=25)) +
  xlab('Original Value') + ylab('Imputed Value') +
  theme(strip.text.x = element_text(size = 12)) 
# ggsave('C:\\Users\\kjkomatsu\\Dropbox (Smithsonian)\\working groups\\CoRRE\\sDiv\\sDiv_sCoRRE_shared\\DataPaper\\2023_sCoRRE_traits\\figures\\Fig 5_original v imputed_20230608.png', width=12, height=12, units='in', dpi=300, bg='white')


# look up some values for species that we know and make sure they are right
ggplot(data=subset(cleanContinuous, species_matched %in% c('Ruellia humilis', 'Andropogon gerardii', 'Parthenocissus quinquefolia')),
       aes(x=species_matched, y=imputed_value)) +
  geom_boxplot() +
  facet_wrap(~trait, scales='free') +
  scale_x_discrete(breaks=c("Andropogon gerardii", "Parthenocissus quinquefolia", "Ruellia humilis"),
                   limits=c("Andropogon gerardii", "Parthenocissus quinquefolia", "Ruellia humilis"),
                   labels=c("Ag", "Pq", "Rh")) 


##### Mean values for each species #####
meanCleanContinuous <- cleanContinuous %>% 
  group_by(family, genus, species_matched, trait) %>% 
  summarize(trait_value=mean(imputed_value)) %>% 
  ungroup()

meanSD <- meanCleanContinuous %>% 
  group_by(trait) %>% 
  summarize(across('trait_value', .fns=list(mean=mean, sd=sd))) %>% 
  ungroup()

meanSDFamily <- meanCleanContinuous %>% 
  group_by(trait, family) %>% 
  summarize(across('trait_value', .fns=list(family_mean=mean, family_sd=sd, family_length=length))) %>% 
  ungroup()

meanSDGenus <- meanCleanContinuous %>% 
  group_by(trait, genus) %>% 
  summarize(across('trait_value', .fns=list(genus_mean=mean, genus_sd=sd, genus_length=length))) %>% 
  ungroup()

meanCleanContinuousErrorRisk <- meanCleanContinuous %>% 
  left_join(meanSD) %>% 
  left_join(meanSDFamily) %>% 
  left_join(meanSDGenus) %>% 
  mutate(error_risk_overall=(trait_value-trait_value_mean)/trait_value_sd, 
         error_risk_family=ifelse(trait_value_family_length>2, (trait_value-trait_value_family_mean)/trait_value_family_sd, NA),
         error_risk_genus=ifelse(trait_value_genus_length>2, (trait_value-trait_value_genus_mean)/trait_value_genus_sd, NA)) %>% 
  select(family, genus, species_matched, trait, trait_value, error_risk_overall, error_risk_family, error_risk_genus) %>% 
  left_join(meanContinuous) %>% 
  select(-imputed_value_mean, imputed_value_sd, original_value_sd)

ggplot(data=na.omit(meanCleanContinuousErrorRisk), aes(x=original_value_mean, y=trait_value)) +
  geom_point() +
  geom_abline(slope=1) +
  facet_wrap(~trait, scales='free') +
  theme(strip.text.x = element_text(size = 12)) +
  xlab('Mean Original Value') + ylab('Mean Imputed Value')
# ggsave('C:\\Users\\kjkomatsu\\Dropbox (Smithsonian)\\working groups\\CoRRE\\sDiv\\sDiv_sCoRRE_shared\\DataPaper\\2023_sCoRRE_traits\\figures\\Fig 5_mean original v imputed_20230608.png', width=12, height=12, units='in', dpi=300, bg='white')


meanCleanContinuousWide <- meanCleanContinuousErrorRisk %>% 
  pivot_longer(cols=c('original_value_mean', 'trait_value'))

ggplot(data=na.omit(meanCleanContinuousWide), aes(x=name, y=value)) +
  geom_boxplot() +
  facet_wrap(~trait, scales='free') +
  theme(strip.text.x = element_text(size = 12)) +
  xlab('') + ylab('Trait Value') +
  scale_y_continuous(trans='log10')
# ggsave('C:\\Users\\kjkomatsu\\Dropbox (Smithsonian)\\working groups\\CoRRE\\sDiv\\sDiv_sCoRRE_shared\\DataPaper\\2023_sCoRRE_traits\\figures\\Fig 6_mean original v imputed_20230608.png', width=12, height=12, units='in', dpi=300, bg='white')


##### Combine continuous and categorical traits #####
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

sppNames <- rbind(correSpecies, GExSpecies) %>% 
  unique() %>% 
  mutate(drop=ifelse(species_matched=='Dianella longifolia'&family=='Xanthorrhoeaceae', 1, 
                     ifelse(species_matched=='Lancea tibetica'&family=='Phrymaceae', 1, 0))) %>% 
  filter(drop==0) %>% 
  select(-drop)

longCategorical <- categoricalTraits %>%
  pivot_longer(growth_form:n_fixation, names_to="trait", values_to="trait_value") %>% 
  mutate(error_risk_overall=NA,
         error_risk_family=NA,
         error_risk_genus=NA) %>% 
  left_join(sppNames)


#### START HERE: need GEx categorical to bind onto continuous data ####
traitsAll <- meanCleanContinuousErrorRisk %>%
  select(-original_value_sd, -original_value_mean, -imputed_value_sd) %>% 
  rbind(longCategorical)

# write.csv(traitsAll, 'CleanedData\\Traits\\CoRRE_allTraitData_June2023.csv')

traitsWide <- traitsAll %>% 
  select(-error_risk_overall, -error_risk_family, -error_risk_genus) %>% 
  pivot_wider(names_from=trait, values_from=trait_value)

# write.csv(traitsWide, 'CleanedData\\Traits\\CoRRE_allTraitData_wide_June2023.csv')


#### testing if imputation runs are different ####
imputedRaw0620 <- read.csv("CleanedData\\Traits\\gap filled continuous traits\\20230620\\imputed_traits_mice.csv") %>% 
  mutate(run='run_2') %>% 
  rowid_to_column(var="rowid")

imputedRaw0608 <- read.csv("CleanedData\\Traits\\gap filled continuous traits\\20230608\\imputed_traits_mice.csv") %>% 
  mutate(run='run_1') %>% 
  rowid_to_column(var="rowid")

compareImputed <- rbind(imputedRaw0608, imputedRaw0620) %>% 
  pivot_longer(cols=seed_dry_mass:SRL, names_to='trait', values_to='values') %>% 
  pivot_wider(names_from=run, values_from=values) %>% 
  left_join(originalRaw)

ggplot(data=subset(compareImputed, trait=='SLA'), aes(x=run_1, y=run_2)) +
  geom_point() +
  geom_abline(slope=1) +
  xlab('Run 1 Value') + ylab('Run 2 Value')

ggplot(data=subset(compareImputed, trait=='seed_dry_mass'), aes(x=run_1, y=run_2)) +
  geom_point() +
  geom_abline(slope=1) +
  xlab('Run 1 Value') + ylab('Run 2 Value') +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10')

ggplot(data=subset(compareImputed, trait=='plant_height_vegetative'), aes(x=run_1, y=run_2)) +
  geom_point() +
  geom_abline(slope=1) +
  xlab('Run 1 Value') + ylab('Run 2 Value') +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10')

ggplot(data=subset(compareImputed, trait=='SRL'), aes(x=run_1, y=run_2)) +
  geom_point() +
  geom_abline(slope=1) +
  xlab('Run 1 Value') + ylab('Run 2 Value')

ggplot(data=subset(compareImputed, trait=='LDMC'), aes(x=run_1, y=run_2)) +
  geom_point() +
  geom_abline(slope=1) +
  xlab('Run 1 Value') + ylab('Run 2 Value')

ggplot(data=subset(compareImputed, trait=='leaf_area'), aes(x=run_1, y=run_2)) +
  geom_point() +
  geom_abline(slope=1) +
  xlab('Run 1 Value') + ylab('Run 2 Value') +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10')

ggplot(data=subset(compareImputed, trait=='leaf_N'), aes(x=run_1, y=run_2)) +
  geom_point() +
  geom_abline(slope=1) +
  xlab('Run 1 Value') + ylab('Run 2 Value')

ggplot(data=subset(compareImputed, trait=='leaf_dry_mass'), aes(x=run_1, y=run_2)) +
  geom_point() +
  geom_abline(slope=1) +
  xlab('Run 1 Value') + ylab('Run 2 Value')


#only those with original data
imputedRawRun1 <- read.csv("CleanedData\\Traits\\gap filled continuous traits\\20230608\\imputed_traits_mice.csv") %>%
  bind_cols(read.csv('OriginalData\\Traits\\raw traits for gap filling\\TRYAusBIEN_continuous_June2023.csv')[,c('DatabaseID', 'DatasetID', 'ObservationID', 'family', 'genus', 'species_matched')]) %>%   
  left_join(mossKey) %>% 
  filter(moss!="moss") %>%
  dplyr::select(-moss) #removes 6 species observations

imputedLongRun1 <- imputedRawRun1 %>% 
  pivot_longer(names_to='trait', values_to='imputed_value', seed_dry_mass:SRL) %>% 
  rename(imputed_value_run1=imputed_value)

imputedRawRun2 <- read.csv("CleanedData\\Traits\\gap filled continuous traits\\20230620\\imputed_traits_mice.csv") %>%
  bind_cols(read.csv('OriginalData\\Traits\\raw traits for gap filling\\TRYAusBIEN_continuous_June2023.csv')[,c('DatabaseID', 'DatasetID', 'ObservationID', 'family', 'genus', 'species_matched')]) %>%   
  left_join(mossKey) %>% 
  filter(moss!="moss") %>%
  dplyr::select(-moss) #removes 6 species observations

imputedLongRun2 <- imputedRawRun2 %>% 
  pivot_longer(names_to='trait', values_to='imputed_value', seed_dry_mass:SRL) %>% 
  rename(imputed_value_run2=imputed_value)

# Read original trait data and join with imputed data
originalRaw <- read.csv('OriginalData\\Traits\\raw traits for gap filling\\TRYAusBIEN_continuous_June2023.csv') %>%
  pivot_longer(names_to='trait', values_to='original_value', seed_dry_mass:SRL) %>%
  na.omit()


# Join original trait data with imputed data. Only keep traits of interest.
allContinuousComparison <- imputedLongRun1 %>% 
  left_join(imputedLongRun2) %>% 
  left_join(originalRaw) %>% 
  filter(trait %in% c('LDMC', 'leaf_area', 'leaf_dry_mass', 'leaf_N', 'plant_height_vegetative', 'seed_dry_mass', 'SLA', 'SRL'))

originalComparison <- allContinuousComparison %>% 
  na.omit()

ggplot(data=subset(originalComparison, trait=='SLA'), aes(x=imputed_value_run1, y=imputed_value_run2)) +
  geom_point() +
  geom_abline(slope=1) +
  xlab('Run 1 Value') + ylab('Run 2 Value')

ggplot(data=subset(originalComparison, trait=='seed_dry_mass'), aes(x=imputed_value_run1, y=imputed_value_run2)) +
  geom_point() +
  geom_abline(slope=1) +
  xlab('Run 1 Value') + ylab('Run 2 Value') +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10')

ggplot(data=subset(originalComparison, trait=='plant_height_vegetative'), aes(x=imputed_value_run1, y=imputed_value_run2)) +
  geom_point() +
  geom_abline(slope=1) +
  xlab('Run 1 Value') + ylab('Run 2 Value') +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10')

ggplot(data=subset(originalComparison, trait=='SRL'), aes(x=imputed_value_run1, y=imputed_value_run2)) +
  geom_point() +
  geom_abline(slope=1) +
  xlab('Run 1 Value') + ylab('Run 2 Value')

ggplot(data=subset(originalComparison, trait=='LDMC'), aes(x=imputed_value_run1, y=imputed_value_run2)) +
  geom_point() +
  geom_abline(slope=1) +
  xlab('Run 1 Value') + ylab('Run 2 Value')

ggplot(data=subset(originalComparison, trait=='leaf_area'), aes(x=imputed_value_run1, y=imputed_value_run2)) +
  geom_point() +
  geom_abline(slope=1) +
  xlab('Run 1 Value') + ylab('Run 2 Value') +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10')

ggplot(data=subset(originalComparison, trait=='leaf_N'), aes(x=imputed_value_run1, y=imputed_value_run2)) +
  geom_point() +
  geom_abline(slope=1) +
  xlab('Run 1 Value') + ylab('Run 2 Value')

ggplot(data=subset(originalComparison, trait=='leaf_dry_mass'), aes(x=imputed_value_run1, y=imputed_value_run2)) +
  geom_point() +
  geom_abline(slope=1) +
  xlab('Run 1 Value') + ylab('Run 2 Value')
