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
library(scales)
library(ggpubr)
library(tidyverse)

setwd('C:\\Users\\kjkomatsu\\Dropbox (Smithsonian)\\working groups\\CoRRE\\CoRRE_database\\Data') #Kim's
# setwd("C:\\Users\\wilco\\Dropbox\\shared working groups\\sDiv_sCoRRE_shared\\CoRRE data\\") # Kevin's laptop wd

theme_set(theme_bw())
theme_update(axis.title.x=element_text(size=30, vjust=-0.35, margin=margin(t=15)), axis.text.x=element_text(size=26),
             axis.title.y=element_text(size=30, angle=90, vjust=0.5, margin=margin(r=15)), axis.text.y=element_text(size=26),
             plot.title = element_text(size=54, vjust=2),
             panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
             legend.title=element_blank(), legend.text=element_text(size=30))


#### Categorical trait data ####
categoricalTraitsCoRRE <- read.csv("CleanedData\\Traits\\complete categorical traits\\sCoRRE categorical trait data_20231006.csv")

correSpp <- categoricalTraitsCoRRE %>% 
  select(species_matched) %>% 
  mutate(drop=1)

categoricalTraitsGEx <- read.csv("CleanedData\\Traits\\complete categorical traits\\GEx categorical trait data_20231006.csv") %>% 
  left_join(correSpp) %>% 
  filter(is.na(drop)) %>% 
  select(-drop)

categoricalTraits <- rbind(categoricalTraitsCoRRE, categoricalTraitsGEx) %>% 
  filter(species_matched!='') %>% 
  dplyr::select(family, species_matched, leaf_type, leaf_compoundness, stem_support, growth_form, photosynthetic_pathway, lifespan,  clonal, mycorrhizal_type, n_fixation, rhizobial, actinorhizal, leaf_type_source, leaf_compoundness_source, growth_form_source, photosynthetic_pathway_source, lifespan_source, stem_support_source, clonal_source, mycorrhizal_source, n_fixation_source) %>%
  mutate(photosynthetic_pathway = replace(photosynthetic_pathway, grep("possible", photosynthetic_pathway), "uncertain")) %>%
  # mutate(clonal = replace(clonal, clonal=="uncertain", NA)) %>%
  mutate(mycorrhizal_type = replace(mycorrhizal_type, mycorrhizal_type %in% c("arbuscular", "facultative_AM"), "AM"),
         mycorrhizal_type = replace(mycorrhizal_type, mycorrhizal_type %in% c("double_AM_EcM", "EcM-AM", "facultative_AM_EcM", "NM-AM", "NM-AM, rarely EcM", "species-specific: AM or rarely EcM-AM or AM"), "multiple"),
         mycorrhizal_type = replace(mycorrhizal_type, mycorrhizal_type=="ecto", "EcM"),
         mycorrhizal_type = replace(mycorrhizal_type, mycorrhizal_type=="ericaceous", "ErM"),
         mycorrhizal_type = replace(mycorrhizal_type, mycorrhizal_type=="orchidaceous", "OM"),
         mycorrhizal_type = replace(mycorrhizal_type, mycorrhizal_type=="Thysanothus", "TM"),
         mycorrhizal_type = replace(mycorrhizal_type, mycorrhizal_type=="NM", "none"),
         mycorrhizal_type = replace(mycorrhizal_type, is.na(mycorrhizal_type), "uncertain")) %>%
  # mutate(lifespan = replace(lifespan, lifespan=="uncertain", NA)) %>%
  mutate(n_fixation_type=ifelse(rhizobial=='yes', 'rhizobial',
                         ifelse(actinorhizal=='yes', 'actinorhizal', 'none'))) %>% 
  filter(lifespan != "moss") %>% 
  select(-n_fixation, -rhizobial, -actinorhizal)

categorical_TRY <- categoricalTraits %>% 
  select(leaf_type_source, leaf_compoundness_source, growth_form_source, photosynthetic_pathway_source, lifespan_source, stem_support_source, clonal_source, mycorrhizal_source, n_fixation_source) %>% 
  pivot_longer(leaf_type_source:n_fixation_source, names_to='trait', values_to='source') %>% 
  filter(grepl("TRY", source))

categoricalTraitsFamilies <- rbind(categoricalTraitsCoRRE, categoricalTraitsGEx) %>% 
  filter(lifespan != "moss") %>% 
  filter(species_matched!='') %>% 
  select(family) %>% 
  unique()

categoricalTraitsError <-  rbind(categoricalTraitsCoRRE, categoricalTraitsGEx) %>% 
  filter(lifespan != "moss") %>% 
  filter(species_matched!='') %>% 
  select(species_matched, growth_form_error, photosynthetic_pathway_error, lifespan_error, stem_support_error, clonal_error) %>% 
  filter(growth_form_error!='')


#### Pie Charts for each categorical trait ####
# leaf type
leafType <- categoricalTraits %>% 
  group_by(leaf_type) %>%
  count() %>% 
  ungroup() %>% 
  mutate(proportion = round((n/sum(n)), digits=3)) %>% 
  arrange(proportion) %>%
  mutate(labels=scales::percent(proportion))

leafTypeFig <- ggplot(leafType, aes(x="", y=proportion, fill=leaf_type)) +
  geom_col() +
  coord_polar(theta="y") +
  scale_fill_manual(values=c('#7DCBBB', '#FFFFA4', '#B0AAD1', '#F7695F', '#6EA1C9', '#FBA550', '#A5DA56', '#AD68AF'))  +
  ggtitle('(d) Leaf Type') +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid  = element_blank(),
        plot.title = element_text(vjust = 0.5),
        legend.position = 'none')

# ggsave('C:\\Users\\kjkomatsu\\Dropbox (Smithsonian)\\working groups\\CoRRE\\sDiv\\sDiv_sCoRRE_shared\\DataPaper\\2023_sCoRRE_traits\\figures\\pie chart\\4_leaf_type_nolegend.png', width=8, height=8, units='in', dpi=300, bg='white')

# leaf compoundness
leafCompoundness <- categoricalTraits %>% 
  group_by(leaf_compoundness) %>%
  count() %>% 
  ungroup() %>% 
  mutate(proportion = round((n/sum(n)), digits=3)) %>% 
  arrange(proportion) %>%
  mutate(labels=scales::percent(proportion))

leafCompoundnessFig <- ggplot(leafCompoundness, aes(x="", y=proportion, fill=leaf_compoundness)) +
  geom_col() +
  coord_polar(theta="y") +
  scale_fill_manual(values=c('#7DCBBB', '#FFFFA4', '#B0AAD1', '#F7695F', '#6EA1C9', '#FBA550', '#A5DA56', '#AD68AF'))  +
  ggtitle('(e) Leaf Compoundness') +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid  = element_blank(),
        plot.title = element_text(vjust = 0.5),
        legend.position = 'none')

# ggsave('C:\\Users\\kjkomatsu\\Dropbox (Smithsonian)\\working groups\\CoRRE\\sDiv\\sDiv_sCoRRE_shared\\DataPaper\\2023_sCoRRE_traits\\figures\\pie chart\\5_leaf_compoundness_nolegend.png', width=8, height=8, units='in', dpi=300, bg='white')

# stem support
stemSupport <- categoricalTraits %>% 
  group_by(stem_support) %>%
  count() %>% 
  ungroup() %>% 
  mutate(proportion = round((n/sum(n)), digits=3)) %>% 
  arrange(proportion) %>%
  mutate(labels=scales::percent(proportion))

stemSupportFig <- ggplot(stemSupport, aes(x="", y=proportion, fill=stem_support)) +
  geom_col() +
  coord_polar(theta="y") +
  scale_fill_manual(values=c('#7DCBBB', '#FFFFA4', '#B0AAD1', '#F7695F', '#6EA1C9', '#FBA550', '#A5DA56', '#AD68AF'))  +
  ggtitle('(f) Stem Support') +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid  = element_blank(),
        plot.title = element_text(vjust = 0.5),
        legend.position = 'none')

# ggsave('C:\\Users\\kjkomatsu\\Dropbox (Smithsonian)\\working groups\\CoRRE\\sDiv\\sDiv_sCoRRE_shared\\DataPaper\\2023_sCoRRE_traits\\figures\\pie chart\\6_stem_support_nolegend.png', width=8, height=8, units='in', dpi=300, bg='white')

# growth form
growthForm <- categoricalTraits %>% 
  group_by(growth_form) %>%
  count() %>% 
  ungroup() %>% 
  mutate(proportion = round((n/sum(n)), digits=3)) %>% 
  arrange(proportion) %>%
  mutate(labels=scales::percent(proportion))

growthFormFig <- ggplot(growthForm, aes(x="", y=proportion, fill=growth_form)) +
  geom_col() +
  coord_polar(theta="y") +
  scale_fill_manual(values=c('#7DCBBB', '#FFFFA4', '#B0AAD1', '#F7695F', '#6EA1C9', '#FBA550', '#A5DA56', '#AD68AF')) +
  ggtitle('(a) Growth Form') +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid  = element_blank(),
        plot.title = element_text(vjust = 0.5),
        legend.position = 'none')

# ggsave('C:\\Users\\kjkomatsu\\Dropbox (Smithsonian)\\working groups\\CoRRE\\sDiv\\sDiv_sCoRRE_shared\\DataPaper\\2023_sCoRRE_traits\\figures\\pie chart\\1_growth_form_nolegend.png', width=8, height=8, units='in', dpi=300, bg='white')

# photosynthetic pathway
photosyntheticPathway <- categoricalTraits %>% 
  group_by(photosynthetic_pathway) %>%
  count() %>% 
  ungroup() %>% 
  mutate(proportion = round((n/sum(n)), digits=3)) %>% 
  arrange(proportion) %>%
  mutate(labels=scales::percent(proportion))

photoPathFig <- ggplot(photosyntheticPathway, aes(x="", y=proportion, fill=photosynthetic_pathway)) +
  geom_col() +
  coord_polar(theta="y") +
  scale_fill_manual(values=c('#7DCBBB', '#FFFFA4', '#B0AAD1', '#F7695F', '#6EA1C9', '#FBA550', '#A5DA56', '#AD68AF'))  +
  ggtitle('(g) Photosynthetic Path') +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid  = element_blank(),
        plot.title = element_text(vjust = 0.5),
        legend.position = 'none')

# ggsave('C:\\Users\\kjkomatsu\\Dropbox (Smithsonian)\\working groups\\CoRRE\\sDiv\\sDiv_sCoRRE_shared\\DataPaper\\2023_sCoRRE_traits\\figures\\pie chart\\7_photosynthetic_pathway_nolegend.png', width=8, height=8, units='in', dpi=300, bg='white')

# lifespan
lifespan <- categoricalTraits %>% 
  group_by(lifespan) %>%
  count() %>% 
  ungroup() %>% 
  mutate(proportion = round((n/sum(n)), digits=3)) %>% 
  arrange(proportion) %>%
  mutate(labels=scales::percent(proportion))

lifespanFig <- ggplot(lifespan, aes(x="", y=proportion, fill=lifespan)) +
  geom_col() +
  coord_polar(theta="y") +
  scale_fill_manual(values=c('#7DCBBB', '#FFFFA4', '#B0AAD1', '#F7695F', '#6EA1C9', '#FBA550', '#A5DA56', '#AD68AF'))  +
  ggtitle('(b) Lifespan') +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid  = element_blank(),
        plot.title = element_text(vjust = 0.5),
        legend.position = 'none')

# ggsave('C:\\Users\\kjkomatsu\\Dropbox (Smithsonian)\\working groups\\CoRRE\\sDiv\\sDiv_sCoRRE_shared\\DataPaper\\2023_sCoRRE_traits\\figures\\pie chart\\2_lifespan_nolegend.png', width=8, height=8, units='in', dpi=300, bg='white')

# clonal
clonal <- categoricalTraits %>% 
  group_by(clonal) %>%
  count() %>% 
  ungroup() %>% 
  mutate(proportion = round((n/sum(n)), digits=3)) %>% 
  arrange(proportion) %>%
  mutate(labels=scales::percent(proportion))

clonalFig <- ggplot(clonal, aes(x="", y=proportion, fill=clonal)) +
  geom_col() +
  coord_polar(theta="y") +
  scale_fill_manual(values=c('#7DCBBB', '#FFFFA4', '#B0AAD1', '#F7695F', '#6EA1C9', '#FBA550', '#A5DA56', '#AD68AF')) +
  ggtitle('(c) Clonality') +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid  = element_blank(),
        plot.title = element_text(vjust = 0.5),
        legend.position = 'none')

# ggsave('C:\\Users\\kjkomatsu\\Dropbox (Smithsonian)\\working groups\\CoRRE\\sDiv\\sDiv_sCoRRE_shared\\DataPaper\\2023_sCoRRE_traits\\figures\\pie chart\\3_clonal_nolegend.png', width=8, height=8, units='in', dpi=300, bg='white')

# mycorrhizal type
mycorrhizalType <- categoricalTraits %>% 
  group_by(mycorrhizal_type) %>%
  count() %>% 
  ungroup() %>% 
  mutate(proportion = round((n/sum(n)), digits=3)) %>% 
  arrange(proportion) %>%
  mutate(labels=scales::percent(proportion))

mycorrFig <- ggplot(mycorrhizalType, aes(x="", y=proportion, fill=mycorrhizal_type)) +
  geom_col() +
  coord_polar(theta="y") +
  scale_fill_manual(values=c('#7DCBBB', '#FFFFA4', '#B0AAD1', '#F7695F', '#6EA1C9', '#FBA550', '#A5DA56', '#AD68AF')) +
  ggtitle('(h) Mycorrhizal Type') +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid  = element_blank(),
        plot.title = element_text(vjust = 0.5),
        legend.position = 'none')

# ggsave('C:\\Users\\kjkomatsu\\Dropbox (Smithsonian)\\working groups\\CoRRE\\sDiv\\sDiv_sCoRRE_shared\\DataPaper\\2023_sCoRRE_traits\\figures\\pie chart\\8_mycorrhizal_type_nolegend.png', width=8, height=8, units='in', dpi=300, bg='white')

# n fixation type
nFixationType <- categoricalTraits %>% 
  group_by(n_fixation_type) %>%
  count() %>% 
  ungroup() %>% 
  mutate(proportion = round((n/sum(n)), digits=3)) %>% 
  arrange(proportion) %>%
  mutate(labels=scales::percent(proportion))

nFixFig <- ggplot(nFixationType, aes(x="", y=proportion, fill=n_fixation_type)) +
  geom_col() +
  coord_polar(theta="y") +
  scale_fill_manual(values=c('#7DCBBB', '#FFFFA4', '#B0AAD1', '#F7695F', '#6EA1C9', '#FBA550', '#A5DA56', '#AD68AF'))  +
  ggtitle('(i) N Fixation Type') +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid  = element_blank(),
        plot.title = element_text(vjust = 0.5),
        legend.position = 'none')

# ggsave('C:\\Users\\kjkomatsu\\Dropbox (Smithsonian)\\working groups\\CoRRE\\sDiv\\sDiv_sCoRRE_shared\\DataPaper\\2023_sCoRRE_traits\\figures\\pie chart\\9_n_fixation_type_nolegend.png', width=8, height=8, units='in', dpi=300, bg='white')

#groupped figure
ggarrange(growthFormFig, lifespanFig, clonalFig,
          leafTypeFig, leafCompoundnessFig, stemSupportFig,
          photoPathFig, mycorrFig, nFixFig,
          ncol = 3, nrow = 3)

# ggsave('C:\\Users\\kjkomatsu\\Dropbox (Smithsonian)\\working groups\\CoRRE\\sDiv\\sDiv_sCoRRE_shared\\DataPaper\\2023_sCoRRE_traits\\figures\\pie chart\\all_categorical_20231103.png', width=26, height=26, units='in', dpi=300, bg='white')


#### Continuous traits ####

# Read species data to remove mosses
mossKey <- read.csv("CleanedData\\Traits\\complete categorical traits\\sCoRRE categorical trait data_20231006.csv") %>%
  dplyr::select(species_matched, leaf_type) %>%
  mutate(moss = ifelse(leaf_type=="moss", "moss","non-moss")) %>%
  dplyr::select(-leaf_type)


# Read in imputed trait data and bind on species information
## this is trait data without replacement (all imputed)
imputedRaw <- read.csv("CleanedData\\Traits\\gap filled continuous traits\\20231006_final\\imputed_traits_mice.csv") %>%
  bind_cols(read.csv('OriginalData\\Traits\\raw traits for gap filling\\TRYAusBIEN_continuous_Oct2023.csv')[,c('DatabaseID', 'DatasetID', 'ObservationID', 'family', 'genus', 'species_matched')]) %>%   
  left_join(mossKey) %>% 
  mutate(moss2=ifelse(moss %in% c('non-moss', NA), 1, 0)) %>%  #accounts for all GEx spp being non-moss
  filter(moss2==1) %>%
  dplyr::select(-moss, -moss2) #removes 20 observations from 5 species

imputedLong <- imputedRaw %>% 
  pivot_longer(names_to='trait', values_to='imputed_value', seed_dry_mass:SRL)

# Read original trait data and join with imputed data
originalRaw <- read.csv('OriginalData\\Traits\\raw traits for gap filling\\TRYAusBIEN_continuous_Oct2023.csv') %>%
  pivot_longer(names_to='trait', values_to='original_value', seed_dry_mass:SRL) %>%
  na.omit() %>% 
  select(-Reference)


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
  ungroup() #147 families

sum(speciesCount$num_species)


#### Compare raw imputed and original data ####
allTogether <- allContinuous %>% 
  pivot_longer(imputed_value:original_value, names_to='data_type', values_to='trait_value') %>% 
  # mutate(data_type=ifelse(data_type=='original_value', DatabaseID, data_type)) %>% 
  na.omit()


#### Clean imputed continuous trait data ####
# Checked to ensure no negative values (confirmed that there are none)

transformed <- allContinuous %>% 
  group_by(trait) %>% 
  mutate(log=log10(imputed_value)) %>% 
  ungroup() 

# with(subset(transformed, trait=='LDMC'), hist(log)) #ensure normality

meanSD <- transformed %>% 
  group_by(trait) %>% 
  summarise(across('log', .fns=list(mean=mean, sd=sd))) %>% 
  ungroup()

meanSDSpecies <- transformed %>%  
  group_by(trait, species_matched) %>% 
  summarize(across('log', .fns=list(species_mean=mean, species_sd=sd, species_length=length))) %>% 
  ungroup()

cleanContinuous <- allContinuous %>% 
  #calculate z-scores (error risk) for continuous traits 
  left_join(transformed) %>% 
  left_join(meanSD) %>% 
  left_join(meanSDSpecies) %>% 
  mutate(error_risk_overall=(log-log_mean)/log_sd) %>% 
  mutate(error_risk_species=(log-log_species_mean)/log_species_sd) %>% 
  filter(error_risk_overall<abs(4)) %>%  #drops 590 observations (0.00036% of data)
  filter(error_risk_species<abs(4)) %>% #drops an additional 8138 observations (0.0053% of data), all of which were from species with at least 18 observations for the given trait value being dropped 
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

sppNum <- cleanContinuous %>% 
  select(species_matched) %>% 
  unique()



#### Root mean square error ####
cleanContinuousNRMSEtrait <- cleanContinuous %>% 
  select(trait, original_value, imputed_value) %>% 
  na.omit() %>% 
  mutate(sq_diff=(imputed_value-original_value)^2) %>% 
  group_by(trait) %>% 
  summarise(sum=sum(sq_diff), n=length(trait), min=min(original_value), max=max(original_value), mean=mean(original_value)) %>% 
  ungroup() %>% 
  mutate(NRMSE=sqrt(sum/n)/mean)

cleanContinuousNRMSE <- cleanContinuous %>% 
  select(original_value, imputed_value) %>% 
  na.omit() %>% 
  mutate(sq_diff=(imputed_value-original_value)^2) %>% 
  summarise(sum=sum(sq_diff), n=length(sq_diff), min=min(original_value), max=max(original_value), mean=mean(original_value)) %>% 
  mutate(NRMSE=sqrt(sum/n)/(mean))


#### Boxplots for each trait ####
cleanContinousWide$trait2 = factor(cleanContinousWide$trait2, levels=c('Leaf Area (leaf, +petiole)', 'Leaf Dry Mass', 'Leaf Dry Matter Content', 'Specific Leaf Area (+petiole)', 'Leaf N Content', 'Plant Vegetative Height', 'Specific Root Length (all root)', 'Seed Dry Mass'))

ggplot(data=cleanContinousWide, aes(x=as.factor(data_type2), y=trait_value)) +
  geom_jitter(aes(color=data_type2)) +
  geom_boxplot(color='black', alpha=0) +
  facet_wrap(~trait2, scales='free_y', ncol=3, labeller=label_wrap_gen(width=25)) +
  scale_x_discrete(breaks=c("AusTraits", "BIEN", "CPTD2", "TIPleaf", "TRY", "imputed_value"),
                   limits=c("AusTraits", "BIEN", "CPTD2", "TIPleaf", "TRY", "imputed_value"),
                   labels=c("Au", "BN", "C2", "TP", "TY", "imp")) +
  scale_color_manual(values=c('#4E3686', '#5DA4D9', '#80D87F', 'darkgrey', '#FED23F', '#EE724C')) +
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        legend.position='none',
        strip.text.x = element_text(size = 20),
        axis.title.x=element_text(size=22, vjust=-0.35, margin=margin(t=15)), axis.text.x=element_text(size=22),
        axis.title.y=element_text(size=22, angle=90, vjust=0.5, margin=margin(r=15)), axis.text.y=element_text(size=22)) +
  xlab('Data Type') + ylab(expression(log[10]("Trait Value")))  +
  scale_y_continuous(trans='log10', labels=label_comma())
# ggsave('C:\\Users\\kjkomatsu\\Dropbox (Smithsonian)\\working groups\\CoRRE\\sDiv\\sDiv_sCoRRE_shared\\DataPaper\\2023_sCoRRE_traits\\figures\\Fig 4_boxplots of original and imputed_20231108_jitter.png', width=14, height=15, units='in', dpi=300, bg='white')


#Look at boxplots for each trait -- means by species
cleanContinuousWideBoxplot <- cleanContinousWide %>% 
  group_by(DatabaseID, data_type2, species_matched, trait, trait2) %>% 
  summarise(trait_value_mean=trait_value) %>% 
  ungroup()

#logged
ggplot(data=cleanContinuousWideBoxplot, aes(x=as.factor(data_type2), y=trait_value_mean)) +
  geom_jitter(aes(color=data_type2)) +
  geom_boxplot(color='black', alpha=0) +
  facet_wrap(~trait2, scales='free_y', ncol=3, labeller=label_wrap_gen(width=25)) +
  scale_x_discrete(breaks=c("AusTraits", "BIEN", "CPTD2", "TIPleaf", "TRY", "imputed_value"),
                   limits=c("AusTraits", "BIEN", "CPTD2", "TIPleaf", "TRY", "imputed_value"),
                   labels=c("Au", "BN", "C2", "TP", "TY", "imp")) +
  scale_color_manual(values=c('#4E3686', '#5DA4D9', '#80D87F', 'darkgrey', '#FED23F', '#EE724C')) +
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        legend.position='none',
        strip.text.x = element_text(size = 20),
        axis.title.x=element_text(size=22, vjust=-0.35, margin=margin(t=15)), axis.text.x=element_text(size=22),
        axis.title.y=element_text(size=22, angle=90, vjust=0.5, margin=margin(r=15)), axis.text.y=element_text(size=22)) +
  xlab('Data Type') + ylab(expression(log[10]("Trait Value")))  +
  scale_y_continuous(trans='log10', labels=label_comma())
# ggsave('C:\\Users\\kjkomatsu\\Dropbox (Smithsonian)\\working groups\\CoRRE\\sDiv\\sDiv_sCoRRE_shared\\DataPaper\\2023_sCoRRE_traits\\figures\\Fig 5_boxplots of original and imputed_20231108_jitter_log_means.png', width=14, height=15, units='in', dpi=300, bg='white')

#not logged
ggplot(data=cleanContinuousWideBoxplot, aes(x=as.factor(data_type2), y=trait_value_mean)) +
  geom_jitter(aes(color=data_type2)) +
  geom_boxplot(color='black', alpha=0) +
  facet_wrap(~trait2, scales='free_y', ncol=3, labeller=label_wrap_gen(width=25)) +
  scale_x_discrete(breaks=c("AusTraits", "BIEN", "CPTD2", "TIPleaf", "TRY", "imputed_value"),
                   limits=c("AusTraits", "BIEN", "CPTD2", "TIPleaf", "TRY", "imputed_value"),
                   labels=c("Au", "BN", "C2", "TP", "TY", "imp")) +
  scale_color_manual(values=c('#4E3686', '#5DA4D9', '#80D87F', 'darkgrey', '#FED23F', '#EE724C')) +
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        legend.position='none',
        strip.text.x = element_text(size = 20),
        axis.title.x=element_text(size=22, vjust=-0.35, margin=margin(t=15)), axis.text.x=element_text(size=22),
        axis.title.y=element_text(size=22, angle=90, vjust=0.5, margin=margin(r=15)), axis.text.y=element_text(size=22)) +
  xlab('Data Type') + ylab("Trait Value")
# ggsave('C:\\Users\\kjkomatsu\\Dropbox (Smithsonian)\\working groups\\CoRRE\\sDiv\\sDiv_sCoRRE_shared\\DataPaper\\2023_sCoRRE_traits\\figures\\Fig 4_boxplots of original and imputed_20231108_jitter_means.png', width=14, height=15, units='in', dpi=300, bg='white')

cleanContinuous$trait2 = factor(cleanContinuous$trait2, levels=c('Leaf Area (leaf, +petiole)', 'Leaf Dry Mass', 'Leaf Dry Matter Content', 'Specific Leaf Area (+petiole)', 'Leaf N Content', 'Plant Vegetative Height', 'Specific Root Length (all root)', 'Seed Dry Mass'))



#### Correlation statistics for each trait ####

#leaf area
with(subset(cleanContinuous, trait=='leaf_area'), hist(log10(original_value)))
with(subset(cleanContinuous, trait=='leaf_area'), hist(log10(imputed_value)))

with(subset(cleanContinuous, trait=='leaf_area'), cor.test(original_value, imputed_value,method = "pearson", use = "complete.obs"))
# r 0.9822873  
summary(leaf_area <- lm(log10(imputed_value)~log10(original_value), data=subset(cleanContinuous, trait=='leaf_area'&!is.na(original_value))))
confint(leaf_area)
# slope:  0.9758327,  SE: 0.0009303 
# Adjusted R-squared:   0.9799  
# F-statistic: 1.1e+06 on 1 and 22526 DF,  p-value: < 2.2e-16
#                         2.5 %     97.5 %
# (Intercept)           0.05565768 0.06532532
# log10(original_value) 0.97400935 0.97765607


#leaf dry mass
with(subset(cleanContinuous, trait=='leaf_dry_mass'), hist(log10(original_value)))
with(subset(cleanContinuous, trait=='leaf_dry_mass'), hist(log10(imputed_value)))

with(subset(cleanContinuous, trait=='leaf_dry_mass'), cor.test(original_value, imputed_value,method = "pearson", use = "complete.obs"))
# r 0.9688328   
summary(leaf_dry_mass <- lm(log10(imputed_value)~log10(original_value), data=subset(cleanContinuous, trait=='leaf_dry_mass'&!is.na(original_value))))
confint(leaf_dry_mass)
# slope:  0.9699192,  SE: 0.0009306 
# Adjusted R-squared:  0.9731   
# F-statistic: 1.086e+06 on 1 and 29988 DF,  p-value: < 2.2e-16
#                          2.5 %      97.5 %
# (Intercept)           0.03063752 0.03613479
# log10(original_value) 0.96809523 0.97174314


#LDMC
with(subset(cleanContinuous, trait=='LDMC'), hist(log10(original_value)))
with(subset(cleanContinuous, trait=='LDMC'), hist(log10(imputed_value)))

with(subset(cleanContinuous, trait=='LDMC'), cor.test(original_value, imputed_value,method = "pearson", use = "complete.obs"))
# r 0.9741727  
summary(LDMC <- lm(log10(imputed_value)~log10(original_value), data=subset(cleanContinuous, trait=='LDMC'&!is.na(original_value))))
confint(LDMC)
# slope:  0.9491077,  SE: 0.0009509  
# Adjusted R-squared:  0.9566   
# F-statistic: 9.962e+05 on 1 and 45250 DF,  p-value: < 2.2e-16
#                          2.5 %      97.5 %
# (Intercept)           -0.03129672 -0.02898384
# log10(original_value)  0.94724387  0.95097148


#SLA
with(subset(cleanContinuous, trait=='SLA'), hist(log10(original_value)))
with(subset(cleanContinuous, trait=='SLA'), hist(log10(imputed_value)))

with(subset(cleanContinuous, trait=='SLA'), cor.test(original_value, imputed_value,method = "pearson", use = "complete.obs"))
# r 0.9636397    
summary(SLA <- lm(log10(imputed_value)~log10(original_value), data=subset(cleanContinuous, trait=='SLA'&!is.na(original_value))))
confint(SLA)
# slope:  0.917740     SE: 0.001691  
# Adjusted R-squared:   0.9234   
# F-statistic: 2.945e+05 on 1 and 24443 DF,  p-value: < 2.2e-16
#                          2.5 %      97.5 %
# (Intercept)           0.1026497 0.1112795
# log10(original_value) 0.9144254 0.9210545


#leaf N
with(subset(cleanContinuous, trait=='leaf_N'), hist(log10(original_value)))
with(subset(cleanContinuous, trait=='leaf_N'), hist(log10(imputed_value)))

with(subset(cleanContinuous, trait=='leaf_N'), cor.test(original_value, imputed_value,method = "pearson", use = "complete.obs"))
# r 0.9661722      
summary(leaf_N <- lm(log10(imputed_value)~log10(original_value), data=subset(cleanContinuous, trait=='leaf_N'&!is.na(original_value))))
confint(leaf_N)
# slope:  0.943737           SE: 0.001689    
# Adjusted R-squared:  0.942   
# F-statistic: 3.121e+05 on 1 and 19204 DF,  p-value: < 2.2e-16
#                          2.5 %      97.5 %
# (Intercept)            0.07054317 0.07947691
# log10(original_value) 0.94042568 0.94704755


#plant vegetative height
with(subset(cleanContinuous, trait=='plant_height_vegetative'), hist(log10(original_value)))
with(subset(cleanContinuous, trait=='plant_height_vegetative'), hist(log10(imputed_value)))

with(subset(cleanContinuous, trait=='plant_height_vegetative'), cor.test(original_value, imputed_value,method = "pearson", use = "complete.obs"))
# r 0.9677212       
summary(plant_height_vegetative <- lm(log10(imputed_value)~log10(original_value), data=subset(cleanContinuous, trait=='plant_height_vegetative'&!is.na(original_value))))
confint(plant_height_vegetative)
# slope:  0.933920           SE: 0.001074          
# Adjusted R-squared:  0.9409   
# F-statistic: 7.492e+05 on 1 and 47417 DF,  p-value: < 2.2e-16
#                          2.5 %      97.5 %
# (Intercept)             -0.03768294 -0.03446463
# log10(original_value)  0.93181569  0.93602393


#SRL
with(subset(cleanContinuous, trait=='SRL'), hist(log10(original_value)))
with(subset(cleanContinuous, trait=='SRL'), hist(log10(imputed_value)))

with(subset(cleanContinuous, trait=='SRL'), cor.test(original_value, imputed_value,method = "pearson", use = "complete.obs"))
# r 0.9235623        
summary(SRL <- lm(log10(imputed_value)~log10(original_value), data=subset(cleanContinuous, trait=='SRL'&!is.na(original_value))))
confint(SRL)
# slope:  0.89257             SE: 0.00681      
# Adjusted R-squared:  0.8768   
# F-statistic: 1.718e+04 on 1 and 2412 DF,  p-value: < 2.2e-16
#                          2.5 %      97.5 %
# (Intercept)           0.3577958 0.4624202
# log10(original_value) 0.8792130 0.9059220


#seed dry mass
with(subset(cleanContinuous, trait=='seed_dry_mass'), hist(log10(original_value)))
with(subset(cleanContinuous, trait=='seed_dry_mass'), hist(log10(imputed_value)))

with(subset(cleanContinuous, trait=='seed_dry_mass'), cor.test(original_value, imputed_value,method = "pearson", use = "complete.obs"))
# r 0.9973605         
summary(seed_dry_mass <- lm(log10(imputed_value)~log10(original_value), data=subset(cleanContinuous, trait=='seed_dry_mass'&!is.na(original_value))))
confint(seed_dry_mass)
# slope:  0.9875345          SE: 0.0003456     
# Adjusted R-squared:  0.9965  
# F-statistic: 8.164e+06 on 1 and 28865 DF,  p-value: < 2.2e-16
#                          2.5 %      97.5 %
# (Intercept)            0.001795201 0.002830967
# log10(original_value) 0.986857048 0.988211918


# Compare cleaned imputed and original data
ggplot(data=na.omit(cleanContinuous), aes(x=original_value, y=imputed_value)) +
  geom_abline(slope=1, linewidth=2, color='black') +
  geom_point(color='darkgrey') +
  geom_smooth(linewidth=2, se=T, color='#e47d0099', method='lm') +
  facet_wrap(~trait2, scales='free', ncol=3, labeller=label_wrap_gen(width=25)) +
  xlab('Original Value') + ylab('Imputed Value') +
  scale_y_continuous(trans='log10', labels = label_comma()) +
  scale_x_continuous(trans='log10', labels = label_comma()) +
  theme(strip.text.x = element_text(size = 28),
        axis.title.x=element_text(size=32, vjust=-0.35, margin=margin(t=15)), axis.text.x=element_text(size=32),
        axis.title.y=element_text(size=32, angle=90, vjust=0.5, margin=margin(r=15)), axis.text.y=element_text(size=32)) 
# ggsave('C:\\Users\\kjkomatsu\\Dropbox (Smithsonian)\\working groups\\CoRRE\\sDiv\\sDiv_sCoRRE_shared\\DataPaper\\2023_sCoRRE_traits\\figures\\Fig 6_original v imputed_20231108.png', width=20, height=16, units='in', dpi=300, bg='white')



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
  mutate(log=log10(trait_value)) %>% 
  group_by(trait) %>% 
  summarize(across('log', .fns=list(mean=mean, sd=sd))) %>% 
  ungroup()

meanSDFamily <- meanCleanContinuous %>% 
  mutate(log=log10(trait_value)) %>% 
  group_by(trait, family) %>% 
  summarize(across('log', .fns=list(family_mean=mean, family_sd=sd, family_length=length))) %>% 
  ungroup()

meanSDGenus <- meanCleanContinuous %>% 
  mutate(log=log10(trait_value)) %>% 
  group_by(trait, genus) %>% 
  summarize(across('log', .fns=list(genus_mean=mean, genus_sd=sd, genus_length=length))) %>% 
  ungroup()

meanCleanContinuousErrorRisk <- meanCleanContinuous %>% 
  left_join(meanSD) %>% 
  left_join(meanSDFamily) %>% 
  left_join(meanSDGenus) %>% 
  mutate(log=log10(trait_value)) %>% 
  mutate(error_risk_overall=(log-log_mean)/log_sd, 
         error_risk_family=ifelse(log_family_length>2, (log-log_family_mean)/log_family_sd, NA),
         error_risk_genus=ifelse(log_genus_length>2, (log-log_genus_mean)/log_genus_sd, NA)) %>% 
  select(family, genus, species_matched, trait, trait_value, error_risk_overall, error_risk_family, error_risk_genus) %>% 
  # left_join(meanContinuous) %>% 
  # select(-imputed_value_mean, imputed_value_sd, original_value_sd) %>% 
  mutate(trait2=ifelse(trait=='leaf_area', 'Leaf Area (leaf, +petiole)',
                ifelse(trait=='SLA', 'Specific Leaf Area (+petiole)', 
                ifelse(trait=='SRL', 'Specific Root Length (all root)',
                ifelse(trait=='leaf_N', 'Leaf N Content',
                ifelse(trait=='plant_height_vegetative', 'Plant Vegetative Height',
                ifelse(trait=='seed_dry_mass', 'Seed Dry Mass',
                ifelse(trait=='leaf_dry_mass', 'Leaf Dry Mass',
                ifelse(trait=='LDMC', 'Leaf Dry Matter Content',
                       trait)))))))))


coverage <- meanCleanContinuousErrorRisk %>% 
  select(family, species_matched, trait, trait_value) %>% 
  pivot_wider(names_from=trait, values_from=trait_value)
# 6 NAs across entire dataframe = 0.000256% of data missing; 99.97% complete for these 2927 species


##### Prepare data for EDI #####
correSpecies <- read.csv("CompiledData\\Species_lists\\FullList_Nov2021.csv") %>%  #species names are standardized
  left_join(read.csv("CompiledData\\Species_lists\\species_families_trees_2021.csv")) %>% 
  filter(tree.non.tree != "tree") %>% #Remove trees
  separate(species_matched, into=c('genus', 'species', 'subspp'), sep=' ') %>% 
  filter(species!='sp.') %>% 
  unite(col='species_matched', genus:species, sep=' ', remove=T) %>% 
  select(family, species_matched) %>% 
  unique()

GExSpecies <- read.csv('OriginalData\\Traits\\GEx_species_family_May2023.csv') %>%
  select(family, species_matched) %>%
  unique()

sppNames <- rbind(correSpecies, GExSpecies) %>%
  unique() %>%
  mutate(drop=ifelse(species_matched=='Dianella longifolia'&family=='Xanthorrhoeaceae', 1,
                     ifelse(species_matched=='Lancea tibetica'&family=='Phrymaceae', 1, 0))) %>%
  filter(drop==0) %>%
  select(-drop) %>% 
  na.omit()

categoricalReferences <- categoricalTraits %>% 
  select(family, species_matched, leaf_type_source:n_fixation_source) %>% 
  pivot_longer(leaf_type_source:n_fixation_source, names_to="trait", values_to="source") %>% 
  mutate(trait=gsub("_source", "", trait))

longCategorical <- categoricalTraits %>%
  select(-leaf_type_source:-n_fixation_source) %>% 
  pivot_longer(leaf_type:n_fixation_type, names_to="trait", values_to="trait_value") %>% 
  left_join(categoricalReferences) %>% 
  mutate(source=ifelse(trait=='n_fixation_type', 'Werner',
                ifelse(trait=='mycorrhizal_type', 'FungalRoot',
                      source))) %>% 
  mutate(error_risk_overall=ifelse(trait=='clonal', 0.05,
                            ifelse(trait=='growth_form', 0.009,
                            ifelse(trait=='leaf_compoundness', 0.002,
                            ifelse(trait=='leaf_type', 0.002,
                            ifelse(trait=='lifespan', 0.033,
                            ifelse(trait=='photosynthetic_pathway', 0.017,
                            ifelse(trait=='stem_support', 0.033, 
                                   NA)))))))) %>% 
  rename(species=species_matched)

# write.csv(longCategorical, 'CleanedData\\Traits\\CoRRE_categoricalTraitData_Nov2023.csv', row.names=F)

longContinuous <- meanCleanContinuousErrorRisk %>%
  mutate(source='Imputed Value') %>% 
  select(family, species_matched, trait, trait_value, error_risk_overall, error_risk_family, error_risk_genus, source) %>% 
  rename(species=species_matched)

# write.csv(longContinuous, 'CleanedData\\Traits\\CoRRE_continuousTraitData_Nov2023.csv', row.names=F)

#combine continuous and categorical
traitsAll <- meanCleanContinuousErrorRisk %>%
  mutate(source=NA) %>% 
  select(family, species_matched, trait, trait_value, error_risk_overall, error_risk_family, error_risk_genus, source) %>% 
  rbind(longCategorical)

# write.csv(traitsAll, 'CleanedData\\Traits\\CoRRE_allTraitData_Nov2023.csv', row.names=F)

traitsWide <- traitsAll %>% 
  select(-error_risk_overall, -error_risk_family, -error_risk_genus, -source) %>% 
  pivot_wider(names_from=trait, values_from=trait_value)

# write.csv(traitsWide, 'CleanedData\\Traits\\CoRRE_allTraitData_wide_Nov2023.csv', row.names=F)