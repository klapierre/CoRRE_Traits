################################################################################
##  siteBiotic.R: Generating relative richness and ANPP in control plots.
##
##  Author: Kimberly Komatsu
##  Date created: September 3, 2021
################################################################################


library(vegan)
library(tidyverse)


setwd("C:\\Users\\lapie\\Dropbox (Smithsonian)\\working groups\\CoRRE\\CoRRE_database\\")

###getting relative richness
cover <- read.csv("Data/CompiledData/RawAbundance.csv")%>%
  mutate(exp=paste(site_code, project_name, community_type, sep="::"))

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
ANPP<-read.csv("Data/CompiledData/ANPP2021.csv")

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


noControlANPP <- read.csv("C:\\Users\\lapie\\Dropbox (Smithsonian)\\working groups\\CoRRE\\CoRRE_database\\Data\\CleanedData\\ANPP_noControls.csv")%>%
  filter(site_code!='maerc')

allANPP <- rbind(noControlANPP, controlANPP)


###combine rrich and anpp
siteBiotic <- expRichness%>%
  full_join(allANPP)

# write.csv(siteBiotic, "Data\\CompiledData\\siteBiotic.csv")
