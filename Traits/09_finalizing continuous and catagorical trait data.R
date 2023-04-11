################################################################################
##  finalizing continuous and categorical traits.R: Checking imputed continuous data and gathered categorical data.
##
##  Authors: Kimberly Komatsu, Meghan Avolio, Kevin Wilcox
################################################################################

#### Set up working space ####

# rm(list=ls()) clean up workspace
#library(FD)
library(PerformanceAnalytics)
library(tidyverse)

setwd('C:\\Users\\kjkomatsu\\Dropbox (Smithsonian)\\working groups\\CoRRE\\CoRRE_database\\Data') #Kim's
setwd("C:\\Users\\wilco\\Dropbox\\shared working groups\\sDiv_sCoRRE_shared\\CoRRE data\\") # Kevin's laptop wd

theme_set(theme_bw())
theme_update(axis.title.x=element_text(size=20, vjust=-0.35, margin=margin(t=15)), axis.text.x=element_text(size=16),
             axis.title.y=element_text(size=20, angle=90, vjust=0.5, margin=margin(r=15)), axis.text.y=element_text(size=16),
             plot.title = element_text(size=24, vjust=2),
             panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
             legend.title=element_blank(), legend.text=element_text(size=20))


#### Categorical trait data ####
# NOTE: Categorical traits to include: growth_form, life_span, mycorrhizal_type, n_fixation, clonal, photosynthetic_pathway.
# The rest were not complete (dispersal mode, pollinaton syndrome).

catagoricalTraits <- read.csv("CleanedData\\Traits\\complete categorical traits\\sCoRRE categorical trait data_12142022.csv") %>%
  dplyr::select(family, species_matched, growth_form, photosynthetic_pathway, lifespan,  clonal, mycorrhizal_type, n_fixation) %>%
  mutate(photosynthetic_pathway = replace(photosynthetic_pathway, grep("possible", photosynthetic_pathway), NA)) %>%
  mutate(clonal = replace(clonal, clonal=="uncertain", NA)) %>%
  mutate(mycorrhizal_type = replace(mycorrhizal_type, mycorrhizal_type=="uncertain", NA)) %>%
  mutate(lifespan = replace(lifespan, lifespan=="uncertain", NA)) %>%
  filter(lifespan != "moss")


#### Continuous traits ####
# NOTE: Continuous traits to include: LDMC, SLA, Vegetative_height, seed dry mass, seed number, rooting density, rooting depth. 

# Read species data to remove mosses
mossKey <- read.csv("CleanedData\\Traits\\complete categorical traits\\sCoRRE categorical trait data_12142022.csv") %>%
  dplyr::select(species_matched, leaf_type) %>%
  mutate(moss = ifelse(leaf_type=="moss", "moss","non-moss")) %>%
  dplyr::select(-leaf_type)

# Read in imputed trait data and bind on species information
imputedRaw <- read.csv("CleanedData\\Traits\\gap filled continuous traits\\20230331\\imputed_traits_mice.csv") %>%
  dplyr::select(-X) %>% 
  bind_cols(read.csv('OriginalData\\Traits\\raw traits for gap filling\\TRYAusBIEN_continuous_March2023d.csv')[,c('DatabaseID', 'DatasetID', 'ObservationID', 'family', 'genus', 'species_matched')]) %>%   
  left_join(mossKey) %>% 
  filter(moss!="moss") %>%
  dplyr::select(-moss) #removes 556 species observations

imputedLong <- imputedRaw %>% 
  pivot_longer(names_to='trait', values_to='imputed_value', seed_dry_mass:X58)

# Read original trait data and join with imputed data
originalRaw <- read.csv('OriginalData\\Traits\\raw traits for gap filling\\TRYAusBIEN_continuous_March2023d.csv') %>%
  pivot_longer(names_to='trait', values_to='original_value', seed_dry_mass:X58) %>%
  na.omit()


# Join original trait data with imputed data. Only keep traits of interest.
allContinuous <- imputedLong %>% 
  left_join(originalRaw) %>% 
  filter(trait %in% c('dark_resp_rate', 'LDMC', 'leaf_area', 'leaf_C', 'leaf_C.N', 'leaf_density', 'leaf_dry_mass',
                      'leaf_K', 'leaf_longevity', 'leaf_N', 'leaf_N.P', 'leaf_P', 'leaf_thickness', 'leaf_transp_rate', 
                      'leaf_width', 'photosynthesis_rate', 'plant_height_vegetative', 'RGR', 'root.shoot', 'root_C', 
                      'root_density', 'root_diameter', 'root_dry_mass', 'root_N', 'root_P', 'rooting_depth', 'seed_dry_mass', 
                      'seed_length', 'seed_number', 'seed_terminal_velocity', 'SLA', 'SRL', 'stem_spec_density', 
                      'stomatal_conductance')) # dropped J_max and Vc_max because they directly relate to photosynthesis

# Calculate averages for each species
meanContinuous <- allContinuous %>% 
  group_by(family, species_matched, trait) %>% 
  summarize_at(.vars=c('imputed_value', 'original_value'),
               .funs=list(mean=mean, sd=sd),
               na.rm=T) %>% 
  ungroup()

speciesCount <- meanContinuous %>% 
  group_by(family) %>% 
  summarize(num_species=length(family)) %>% 
  ungroup()


# Compare imputed to original continuous trait data
ggplot(data=na.omit(meanContinuous), aes(x=original_value_mean, y=imputed_value_mean)) +
  geom_point() +
  geom_abline(slope=1) +
  facet_wrap(~trait, scales='free')

# Only grasses -- 11620 species
ggplot(data=na.omit(subset(meanContinuous, family=='Poaceae')), aes(x=original_value_mean, y=imputed_value_mean)) +
  geom_point() +
  geom_abline(slope=1) +
  facet_wrap(~trait, scales='free')

# Only asters -- 11480
ggplot(data=na.omit(subset(meanContinuous, family=='Asteraceae')), aes(x=original_value_mean, y=imputed_value_mean)) +
  geom_point() +
  geom_abline(slope=1) +
  facet_wrap(~trait, scales='free')

# Only legumes -- 4620
ggplot(data=na.omit(subset(meanContinuous, family=='Fabaceae')), aes(x=original_value_mean, y=imputed_value_mean)) +
  geom_point() +
  geom_abline(slope=1) +
  facet_wrap(~trait, scales='free')


# Look at boxplots for each trait
ggplot(data=na.omit(allContinuous), aes(x=trait, y=imputed_value)) +
  geom_boxplot() +
  facet_wrap(~trait, scales='free')


#### Clean imputed continuous trait data ####
####Check these decisions with Meghan!

#Checked to ensure no negative values (confirmed that there are none)

#Things that look problematic but Kim thinks are real: leaf_area (some ferns and palms with huge leaves), plant_height_vegetative (vines that have big big heights like Vitus sp and virginia creeper), seed number (consistently high numbers for some species that probably do have lots of seeds)

#Things that are a problem: Some imputed seed number values are less than 1, which doesn't make sense.

cleanContinuous <- allContinuous %>% 
  mutate(drop=ifelse(trait=='seed_number' & imputed_value<1, 1, 0)) %>% 
  filter(drop==0) %>% #drops 925 observations
  select(-drop)


# Look at boxplots for each trait
ggplot(data=na.omit(cleanContinuous), aes(x=trait, y=imputed_value)) +
  geom_boxplot() +
  facet_wrap(~trait, scales='free')


##### TO DO #####
# doesn't seem like there are any big outliers for each trait overall, but when comparing individual species means there are some
# look up some values for species that we know and make sure they are right

ggplot(data=subset(meanContinuous, species_matched %in% c('Ruellia humilis', 'Andropogon gerardii', 'Parthenocissus quinquefolia')),
       aes(x=species_matched, y=imputed_value_mean)) +
  geom_boxplot() +
  theme(axis.text.x=element_text(angle=40, vjust=0.5, size=3)) +
  facet_wrap(~trait, scales='free')


##### Combine continuous and categorical traits #####
wideContinuous <- meanContinuous %>% 
  select(-family, -original_value_mean, -imputed_value_sd, -original_value_sd) %>% 
  pivot_wider(names_from=trait, values_from=imputed_value_mean, values_fill=NA)

traitsAll <- wideContinuous %>%
  full_join(catagoricalTraits, by="species_matched")

# write.csv(traitsAll, 'CleanedData\\Traits\\CoRRE_allTraitData_March2023.csv')