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



#### Categorical trait data ####
# NOTE: Categorical traits to include: growth_form, life_span, mycorrhizal_type, n_fixation, clonal, photosynthetic_pathway.
# The rest were not complete (dispersal mode, pollinaton syndrome).

catagoricalTraits <- read.csv("CleanedData\\Traits\\complete categorical traits\\sCoRRE categorical trait data_12142022.csv") %>%
  dplyr::select(species_matched, growth_form, photosynthetic_pathway, lifespan,  clonal, mycorrhizal_type, n_fixation) %>%
  mutate(photosynthetic_pathway = replace(photosynthetic_pathway, grep("possible", photosynthetic_pathway), NA)) %>%
  mutate(clonal = replace(clonal, clonal=="uncertain", NA)) %>%
  mutate(mycorrhizal_type = replace(mycorrhizal_type, mycorrhizal_type=="uncertain", NA)) %>%
  mutate(lifespan = replace(lifespan, lifespan=="uncertain", NA)) %>%
  filter(lifespan != "moss")


#### Continuous traits ####
# NOTE: Continuous traits to include: LDMC, SLA, Vegetative_height, seed dry mass, seed number, rooting density, rooting depth. 

# Read in imputed trait data and bind on species information
imputedRaw <- read.csv("CleanedData\\Traits\\gap filled continuous traits\\imputed_traits_mice.csv") %>%
  dplyr::select(-X) %>% 
  bind_cols(read.csv('OriginalData\\Traits\\raw traits for gap filling\\TRYAusBIEN_continuous_March2023.csv')[,c('DatabaseID', 'DatasetID', 'ObservationID', 'family', 'genus', 'species_matched')])

imputedLong <- imputedRaw %>% 
  pivot_longer(names_to='trait', values_to='imputed_value', seed_dry_mass:X58)


# Read original trait data and join with imputed data
originalRaw <- read.csv('OriginalData\\Traits\\raw traits for gap filling\\TRYAusBIEN_continuous_March2023.csv') %>%
  pivot_longer(names_to='trait', values_to='original_value', seed_dry_mass:X58) %>%
  na.omit()


# Join original trait data with imputed data. Only keep traits of interest.
allContinuous <- imputedLong %>% 
  left_join(originalRaw) %>% 
  filter(trait %in% c('dark_resp_rate', 'LDMC', 'leaf_area', 'leaf_C', 'leaf_C.N', 'leaf_density', 'leaf_dry_mass',
                      'leaf_K', 'leaf_longevity', 'leaf_N', 'leaf_N.P', 'leaf_P', 'leaf_thickness', 'leaf_transp_rate', 
                      'leaf_width', 'photosynthesis_rate', 'plant_height_generative', 'plant_height_vegetative', 'RGR',
                      'root.shoot', 'root_C', 'root_density', 'root_diameter', 'root_dry_mass', 'root_N', 'root_P',
                      'rooting_depth', 'seed_dry_mass', 'seed_length', 'seed_number', 'seed_terminal_velocity', 'SLA',
                      'SRL', 'stem_spec_density', 'stomatal_conductance')) # dropped J_max and Vc_max because they directly relate to photosynthesis


# Calculate averages for each species
meanContinuous <- allContinuous %>% 
  group_by(species_matched, trait) %>% 
  summarize_at(.vars=c('imputed_value', 'original_value'),
               .funs=list(mean=mean, sd=sd),
               na.rm=T) %>% 
  ungroup()


# Compare imputed to original continuous trait data
ggplot(data=na.omit(allContinuous), aes(x=original_value, y=imputed_value)) +
  geom_point() +
  geom_abline(slope=1) +
  facet_wrap(~trait, scales='free')

ggplot(data=na.omit(meanContinuous), aes(x=original_value_mean, y=imputed_value_mean)) +
  geom_point() +
  geom_abline(slope=1) +
  facet_wrap(~trait, scales='free')


# Look at boxplots for each trait
ggplot(data=na.omit(allContinuous), aes(x=trait, y=imputed_value)) +
  geom_boxplot() +
  facet_wrap(~trait, scales='free')


#### Clean imputed continuous trait data ####
####Check these decisions with Meghan!

cleanContinuous <- allContinuous %>% 
  #filtering out negative values for everything except leaf transpiration rate (where photosynthesis rate is negative, should actually be 0)
  mutate(drop=ifelse(trait!='leaf_transp_rate' & imputed_value<0, 1, 0)) %>% 
  filter(drop==0) %>% #drops 44695 observations
  #RGR and seed terminal velocity look great! shouldn't drop any of their values
  


# Look at boxplots for each trait
ggplot(data=na.omit(cleanContinuous), aes(x=trait, y=imputed_value)) +
  geom_boxplot() +
  facet_wrap(~trait, scales='free')


##### TO DO #####
# how were outliers removed before gap filling -- should maybe do it species by species, because there are some outliers for andro and achillea that might not be outliers in the entire dataset
# figure out how to merge the sd with the imputed backtransformed data
# determine how to do the sd cutoffs (we cannot just do quantiles because then the same amount of data is dropped from each trait, which is unfair to the good traits and generous to the bad traits)
# drop the outliers based on sd cutoffs and the extreme values
# correlations for each trait with imputed data (see code below)
# look up some values for species that we know and make sure they are right
















# hist(imputedSubset$LDMC_mean)
# hist(imputedSubset$SLA_mean)
# hist(imputedSubset$plant_height_vegetative_mean)
# hist(imputedSubset$seed_dry_mass_mean)
# hist(imputedSubset$seed_number_mean)
# hist(imputedSubset$rooting_depth_mean)

rm(imputedRaw)




#### Removing mosses ####
mossKey <- read.csv("complete categorical traits\\sCoRRE categorical trait data_12142022.csv") %>%
  dplyr::select(species_matched, leaf_type) %>%
  mutate(moss = ifelse(leaf_type=="moss", "moss","non-moss")) %>%
  dplyr::select(-leaf_type)

imputedSubset <- imputedRaw %>%
  left_join(mossKey, by="species_matched") %>%
  mutate(moss=ifelse(moss=="moss","moss","non-moss")) %>%
  filter(moss!="moss") %>%
  dplyr::select(-moss) #%>%
# group_by(genus, family, species_matched) %>%
# summarize_at(vars(seed_dry_mass:seed_number), list(mean=mean, sd=sd), na.rm=T) %>%
# ungroup() #total of 1786 species remain

# #total species count = 2403 (so 617 species dropped [likely because they had zero trait data and therefore can't have any data imputed])
# sppList <- mossKey %>%
#   filter(moss=='non-moss')



###########################################################
### Combine continuous and catagorical traits
###########################################################

traits_all <- dplyr::select(imputedSubset, -LDMC_sd:-rooting_depth_sd) %>%
  full_join(catagoricalTraits, by="species_matched")

# Find species that have continuous data but no categorical data -- 3 of these are mosses, so I will remove them from continuous data in that cleaning script
# TO DO: We will want to populate categorical data for three species: "Galium mollugo" "Heracleum sphondylium" "Trachypogon spicatus"