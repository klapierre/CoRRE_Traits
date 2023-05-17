################################################################################
##  siteBiotic.R: Generating relative richness and ANPP in control plots.
##
##  Author: Kimberly Komatsu
##  Date created: September 3, 2021
################################################################################


library(vegan)
library(tidyverse)


setwd("C:\\Users\\kjkomatsu\\Dropbox (Smithsonian)\\working groups\\CoRRE\\CoRRE_database\\")
setwd("C:\\Users\\wilco\\OneDrive - UNCG\\Working groups\\sDiv\\CoRRE data\\") ## wilcox personal laptop

###getting relative richness
cover <- read.csv("Data/CompiledData/RawAbundance.csv")%>%
  mutate(exp=paste(site_code, project_name, community_type, sep="::"))%>%
  filter(project_name!='NSFC') #filter out DL NSFC because of duplicate entries for each plot/date

sampleIntensity <- cover%>%
  select(site_code, project_name, community_type, treatment_year, plot_id)%>%
  unique()%>%
  group_by(site_code, project_name, community_type)%>%
  summarise(N=length(plot_id))%>%
  ungroup() #GVN FACE has 24 sampling points, but the next lowest is 34

exp <- cover%>%
  select(exp)%>%
  unique()
rownames(exp) <- NULL


#create empty dataframe for loop
estimatedRichness=data.frame(row.names=1) 

for(i in 1:length(exp$exp)) {
  
  #creates a dataset for each unique experiment
  subset <- cover[cover$exp==exp$exp[i],]%>%
    select(exp, plot_id, calendar_year, genus_species, abundance)
  
  #transpose data into wide form
  speciesData <- subset%>%
    spread(genus_species, abundance, fill=0)
  
  #calculate species accumulation curves
  pool <- poolaccum(speciesData[,4:ncol(speciesData)], permutations=100)
  chao <- as.data.frame(pool$chao) #this gives us estimated richness from 1-X samples
  chao$aveChao <- rowMeans(chao)
  chao$n <- row.names(chao)
  chao$exp <- exp$exp[i]
  chao2 <- chao%>%
    select(exp, n, aveChao)
  
  #rbind back
  estimatedRichness<-rbind(chao2, estimatedRichness)
}

expRichness <- estimatedRichness%>%
  filter(n==34)%>% #the lowest sampling intensity, not including GVN_FACE
  separate(exp, c("site_code", "project_name", "community_type"), sep="::")%>%
  mutate(rrich=aveChao)%>%
  select(-n, -aveChao)%>%
  filter(site_code!='GVN')

gface <- data.frame(site_code="GVN", project_name="FACE", community_type=0, rrich=30.85)

expRichness <- rbind(expRichness, gface)

###getting control ANPP
ANPP<-read.csv("Data/CompiledData/ANPP2021.csv")%>%
  filter(project_name!='TMECE')

expInfo <- read.csv("Data/CompiledData/ExperimentInfo.csv")%>%
  select(site_code, project_name, community_type, treatment, plot_mani)%>%
  unique()

controlANPP<-ANPP%>%
  left_join(expInfo)%>%
  filter(plot_mani==0, anpp!='NA')%>%
  group_by(site_code, project_name, community_type, treatment_year)%>%
  summarize(anpp=mean(anpp))%>%
  ungroup()%>%
  group_by(site_code, project_name, community_type)%>%
  summarize(anpp=mean(anpp))%>%
  ungroup()

noControlANPP <- read.csv("Data\\CleanedData\\ANPP_noControls.csv")%>%
  filter(site_code!='maerc')%>%
  select(site_code, project_name, community_type, anpp)

allANPP <- rbind(noControlANPP, controlANPP)

###
### Calculate % abundance of annual species for each site 
###

### THERE ARE MISSING GENUS SPECIES NAMES IN THE RELATIVE COVER DATA FILE
### FOR NOW, I AM JUST REMOVING THESE DATA...

### Read in and clean datasets
corre2try <- read.csv("Data\\OriginalData\\Traits\\TRY\\corre2trykey_2021.csv") %>% # key to merge genus_species column to cat trait data
  dplyr::select(genus_species, species_matched) %>%
  unique(.)
  #species_df <- read.csv("CoRRE_TRY_species_list.csv")

exp_info_df <- read.csv("Data\\CompiledData\\ExperimentInfo.csv")

unkn_sp_lifespan <- read.csv("Data\\CleanedData\\Traits\\complete categorical traits\\species with no cat traits.csv") %>%
  rename(species_matched=genus_species)

# Read in trait data and combine with manually entered lifespan data (for problem species)
trait_cat_df <- read.csv("Data\\CleanedData\\Traits\\complete categorical traits\\sCoRRE categorical trait data_12142022.csv") %>%
  dplyr::select(species_matched, lifespan) %>%
  bind_rows(unkn_sp_lifespan) %>%
  mutate(lifespan=replace(lifespan, lifespan=="moss","perennial")) %>%
  mutate(lifespan=replace(lifespan, species_matched=="Robinia pseudo","perennial")) %>%
  filter(species_matched!="")
  
### join genus_species names with species_matched for future joining with trait data
relcov_df <- read.csv("Data\\CompiledData\\RelativeCover.csv") %>%
  left_join(corre2try, by="genus_species") %>%
  filter(genus_species != "")

### Insert genus_species names into species_matched column only for species that we do not have trait data for -- this step is so we can include the manually entered lifespan data
relcov_df <- relcov_df %>%
  mutate(species_matched = replace(species_matched, is.na(species_matched), 
                                   relcov_df[is.na(relcov_df$species_matched),'genus_species']))

### Checking that no-trait species have been appended to the species_matched column
#missing <- rel_cov_df[is.na(rel_cov_df$species_matched),]
# missing <- rel_cov_df[is.na(rel_cov_df$AccSpeciesID),]
# missing_sp_vec <- unique(missing$genus_species)

### Combine relative cover with trait data
relcov_trait_df <- relcov_df %>%
  left_join(trait_cat_df, by="species_matched")

### Calculate percent unknown and percent annual
perc_annual <- relcov_trait_df %>%
  left_join(exp_info_df) %>%
  dplyr::select(site_code, project_name, community_type, calendar_year, plot_mani, plot_id, species_matched, relcov, lifespan) %>%
  filter(plot_mani==0) %>%
  group_by(site_code, project_name, community_type, calendar_year, lifespan, plot_id) %>%
  summarize(lifespan_cover=sum(relcov, na.rm=T)) %>%
  ungroup() %>%
  group_by(site_code, project_name, community_type, lifespan) %>%
  summarize(lifespan_cover = mean(lifespan_cover, na.rm=T)) %>%
  ungroup() %>%
  mutate(lifespan=replace(lifespan, lifespan=="fungus-lichen","FungusLichen")) %>%
  pivot_wider(names_from=lifespan, values_from=lifespan_cover) %>%
  replace(is.na(.), 0) %>%
  mutate(annual_relcov = annual/(annual+perennial+biennial+uncertain+FungusLichen)) %>%
  dplyr::select(site_code, project_name, community_type, annual_relcov) 

# test 
### Take a quick look
# hist(perc_annual$annual_relcov)
# hist(perc_annual$uncertain)
# 
# ggplot(perc_annual, aes(x=site_code, y=annual_relcov)) +
#   geom_boxplot() + theme(axis.text.x=element_text(angle=60, hjust=1))


###combine rrich and anpp
siteBiotic <- expRichness%>%
  full_join(allANPP) %>%
  full_join(perc_annual)
  
# write.csv(siteBiotic, "Data\\CompiledData\\siteBiotic.csv", row.names=F)