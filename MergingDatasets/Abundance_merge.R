setwd("~/Dropbox/CoRRE_database/Data/CleanedData/Sites/Species csv")
# Add your working directories here if this doesn't work for you
setwd('C:\\Users\\lapie\\Dropbox (Smithsonian)\\working groups\\CoRRE\\CoRRE_database\\Data\\CleanedData\\Sites\\Species csv') #kim's laptop
# MEGHAN
setwd("C:\\Users\\mavolio2\\Dropbox\\CoRRE_database\\Data\\CleanedData\\Sites\\Species csv")

library(gtools)
library(tidyverse)

#kim's notes 2/8/2021: 
#Rengen_Nut has a species called '...41', ask PIs what this is. Meghan emailed 11/11/21
#SGS_Precip, I removed "Unknown", which had 9 entries. Might ask the PIs if this was a conglomerate of all unknowns or not. I left in "Unknown spurge", which I considered akin to something like Euphorbia sp.
#gmce and gmce2 both have a lot of 'Â' symbols following the genus name; fert1 has this problem for just the species ' Carex canescensÂ'. we should fix this if it isn't something the name cleaning will understand
# Kaitlin's notes: 
# Ask e6 PI about unknown grass A etc sp. 
# Konza BGP has several entries with missing species names. It is in the original datafile too...
# nov 20, 2015 -checked all plots have recorded species, so the filter abundance !=0 step will not remove any plots.

watering<-read.delim("ANG_watering.txt")%>%
  gather(species_code, abundance, sp1:sp43)%>%
  mutate(community_type=0,block=0, version = 1.0)
watering_names<-read.delim("ANG_watering_specieslist.txt")
watering2<-merge(watering, watering_names, by="species_code", all=T)%>%
  select(-species_code) %>%
  filter(abundance!=0) #this drops 3 plots in 2011 and 2013 (plots 3, 4, and 7) which had no pin hits but did have species

fert1 <- read.csv("ANR_Fert1.csv") %>% 
  mutate(version = 2.0, community_type = 0, block = 0) %>%
  filter(genus_species!="Unknown")

fert3 <- read.csv("ANR_Fert3.csv") %>% 
  mutate(version = 2.0, community_type = 0, block = 0)%>%
  filter(genus_species!="Unknown")

mat2<-read.delim("ARC_mat2.txt")%>%
  select(-id, -nutrients, -light, -carbon, -water, -other_manipulation, -num_manipulations, -experiment_year, -n, -p, -plot_mani, -species_num)%>%
  gather(species_code, abundance, sp1:sp232)%>%
  mutate(community_type=0, version = 1.0)
mat2_names<-read.delim("ARC_mat2_specieslist.txt")
mat22<-merge(mat2, mat2_names, by="species_code", all=T)%>%
  filter(abundance!=0)%>%
  filter(genus_species!="Caribou feces", genus_species!="lichen", genus_species!="moss")%>%
  select(-species_code)

mnt<-read.delim("ARC_mnt.txt")%>%
  select(-id, -nutrients, -light, -carbon, -water, -other_manipulation, -num_manipulations, -experiment_year, -n, -p, -plot_mani,   -species_num, -plot_id1)%>%
  gather(species_code, abundance, sp1:sp232)%>%
  mutate(community_type=0, version = 1.0)
mnt_names<-read.delim("ARC_mnt_specieslist.txt")
mnt2<-merge(mnt, mnt_names, by="species_code", all=T)%>%
  filter(abundance!=0)%>%
  filter(genus_species!="Nostoc sp.")%>%
  filter(genus_species!="Caribou feces", genus_species!="lichen", genus_species!="moss")%>%
  select(-species_code)

clonal<-read.delim("ASGA_Clonal.txt")%>%
  select(-id, -nutrients, -light, -carbon, -water, -other_manipulation, -num_manipulations, -experiment_year, -n, -plant_mani, -plot_id1, -plot_mani, -species_num)%>%
  gather(species_code, abundance, sp1:sp232)%>%
  mutate(community_type=0, version = 1.0)
clonal_names<-read.delim("ASGA_Clonal_specieslist.txt")
clonal2<-merge(clonal, clonal_names, by="species_code", all=T)%>%
  filter(abundance!=0)%>%
  select(-species_code) %>%
  filter(genus_species!="No plants present",
         genus_species!="unknown grass",
         genus_species!="unknown dicot",
         genus_species!="Standing Dead biomass from current year production",
         genus_species!="unknown Asteraceae",
         genus_species!="unknown Polygonaceae")

exp1<-read.delim("ASGA_Exp1.txt")%>%
  select(-id, -nutrients, -light, -carbon, -water, -other_manipulation, -num_manipulations, -experiment_year, -n, -burn, -clip, -precip, -p, -dist, -patchiness, -plant_mani, -plot_id1, -plot_mani,   -species_num)%>%
  gather(species_code, abundance, sp1:sp220)
exp1_names<-read.delim("ASGA_Exp1_specieslist.txt")
exp12<-merge(exp1, exp1_names, by="species_code", all=T)%>%
  filter(abundance!=0)%>% mutate(version = 1.0) %>%
  select(-species_code)

eelplot <- read.csv("AZI_EELplot.csv")%>%
  mutate(version = 2.0, community_type = 0, data_type='density')

nitphos <- read.csv("AZI_NitPhos.csv") %>%
  mutate(community_type = 0, block = 0, version=ifelse(calendar_year<=2014, 1, 2)) %>%
  filter(abundance != 0)

lind<-read.delim("BAY_LIND.txt")%>%
  select(-id, -nutrients, -light, -carbon, -water, -other_manipulation, -num_manipulations, -experiment_year, -precip,-plant_mani, -plot_id1, -plot_mani,   -species_num)%>%
  gather(species_code, abundance, sp1:sp56)%>%
  mutate(community_type=0, version = 1.0)
lind_names<-read.delim("BAY_LIND_specieslist.txt")%>%
  mutate(species_code=tolower(species_code))
lind2<-merge(lind, lind_names, by="species_code", all=T)%>%
  filter(abundance!=0)%>%
  select(-species_code)

events<-read.delim("Bt_EVENT2.txt")%>%
  select(-id, -nutrients, -light, -carbon, -water, -other_manipulation, -num_manipulations, -experiment_year, -clip,-precip_vari, -precip_vari_season, -true_plot_mani, -plot_id1, -plot_mani,   -species_num)%>%
  gather(species_code, abundance, sp1:sp56)%>%
  mutate(community_type=0, version = 1.0)
events_names<-read.delim("Bt_EVENT2_specieslist.txt")%>%
  mutate(species_code=tolower(species_code))
events2<-merge(events, events_names, by="species_code", all=T)%>%
  filter(abundance!=0)%>%
  select(-species_code)

btdrought <- read.csv("Bt_DroughtNet.csv") %>%
  mutate(community_type = 0, block = 0, version = 2.0)%>%
  separate(genus_species, into=c("g", "s"),remove = F, sep="_")%>%
  mutate(genus_species2=ifelse(!is.na(s), paste(g, s, sep=" "), genus_species))%>%
  select(-g, -s, -genus_species)%>%
  rename(genus_species=genus_species2)


btnpkd <- read.csv("Bt_NPKDNet.csv") %>%
  mutate(community_type = 0, block = 0, version = 2.0)

pq<-read.delim("BUX_PQ.txt")%>%
  select(-id, -nutrients, -light, -carbon, -water, -other_manipulation, -num_manipulations, -true_num_manipulations, -experiment_year, -clip, -precip, -temp, -true_plot_mani, -plot_id1, -plot_mani,   -species_num)%>%
  gather(species_code, abundance, sp1:sp66)%>%
  mutate(community_type=0, version = 1.0)
pq_names<-read.delim("BUX_PQ_specieslist.txt")
pq2<-merge(pq, pq_names,by="species_code", all=T)%>%
  filter(abundance!=0)%>%
  select(-species_code)

pennings<-read.delim("CAR_Pennings.txt")%>%
  select(-id, -nutrients, -light, -carbon, -water, -other_manipulation, -num_manipulations, -experiment_year, -n, -p, -k,  -plot_id1, -plot_mani,   -species_num)%>%
  gather(species_code, abundance, sp1:sp232)%>%
  mutate(block=0, version = 1.0)
pennings_names<-read.delim("CAR_Pennings_specieslist.txt")
pennings2<-merge(pennings, pennings_names,by="species_code", all=T)%>%
  filter(abundance!=0)%>%
  select(-species_code)

rmapc<-read.delim("CAU_RMAPC.txt")%>%
  select(-id, -nutrients, -light, -carbon, -water, -other_manipulation, -num_manipulations, -experiment_year, -n, -p, -lime,  -plot_id1, -precip, -plot_mani,   -species_num)%>%
  gather(species_code, abundance, sp1:sp232)%>%
  mutate(block=0, version = 1.0)
rmapc_names<-read.delim("CAU_RMAPC_specieslist.txt")%>%
  mutate(species_code=tolower(species_code))
rmapc2<-merge(rmapc, rmapc_names, by="species_code", all=T)%>%
  filter(abundance!=0)%>%
  select(-species_code)

biocon<-read.csv("CDR_BioCON.csv")%>%
  mutate(community_type=0, version = ifelse(calendar_year<=2013, 1.0,2.0)) %>% filter(abundance!=0)

e001<-read.csv("CDR_e001.csv")%>%
  mutate(block=0, version=ifelse(calendar_year<=2013, 1.0,2.0), data_type='biomass')%>%
  filter(abundance!=0) %>%
  filter(genus_species!="Forb seedlings",
         genus_species!="Fungi ",
         genus_species!="Miscellaneous grasses",
         genus_species!="Miscellaneous herbs",
         genus_species!="Miscellaneous legumes",
         genus_species!="Miscellaneous litter",
         genus_species!="Miscellaneous rushes",
         genus_species!="Miscellaneous sedges",
         genus_species!="Miscellaneous sp.",
         genus_species!="Miscellaneous woody",
         genus_species!="Pine needles", 
         genus_species!="Pine cones", 
         genus_species!="Miscellaneous herb",
         genus_species!="Mosses & lichens 2",
         genus_species!="Miscellaneous forb", 
         genus_species!="Miscellaneous woody plants",
         genus_species!="Miscellaneous grasses 2",
         genus_species!="Mosses & lichens",
         genus_species!="Fungi", 
         genus_species!="Miscellaneous Woody")

e002<-read.delim("CDR_e002.txt")%>%
  select(-id, -nutrients, -light, -carbon, -water, -other_manipulation, -num_manipulations, -true_num_manipulations, -experiment_year, -p, -k, -lime, -n, -other_nut, -burn, -herb_removal, -true_plot_mani, -plot_mani, -cessation, -dist,   -species_num)%>%
  gather(species_code, abundance, sp1:sp232)%>%
  mutate(block=0, version = 1.0)
e002_names<-read.delim("CDR_e002_specieslist.txt")%>%
  mutate(species_code=tolower(species_code))
e0022<-merge(e002, e002_names,by="species_code", all=T)%>%
  filter(abundance!=0, calendar_year<1992)%>% ##drops everything once cessation starts
  mutate(spcode=genus_species)%>%
  select(-species_code, -genus_species)
e001_names <- read.csv("CDR_e001_e002_specieslist.csv")
e0023<-merge(e0022, e001_names, by="spcode")%>%
  select(-spcode)%>%
  filter(genus_species!="Mosses &lichens")

megarich<-read.delim("CEH_Megarich.txt")%>%
  select(-id, -nutrients, -light, -carbon, -water, -other_manipulation, -num_manipulations, -true_num_manipulations, -experiment_year, -clip, -c, -temp, -n, -p, -k, -true_plot_mani, -plot_mani,   -species_num)%>%
  gather(species_code, abundance, sp1:sp232)%>%
  mutate(block=0,community_type=0, version = 1.0)
megarich_names<-read.delim("CEH_Megarich_specieslist.txt")
megarich2<-merge(megarich, megarich_names,by="species_code", all=T)%>%
  filter(abundance!=0)%>%
  select(-species_code)

imagine<-read.delim("CLE_imagine.txt")%>%
  select(-id, -nutrients, -light, -carbon, -water, -other_manipulation, -num_manipulations, -experiment_year, -c,  -plot_mani, -plant_mani, -precip, -temp, -species_num)%>%
  gather(species_code, abundance, sp1:sp12)%>%
  mutate(community_type=0, version = 1.0)
imagine_names<-read.delim("CLE_imagine_specieslist.txt")%>%
  mutate(species_code=tolower(species_code))
imagine2<-merge(imagine, imagine_names, by="species_code", all=T)%>%
  filter(abundance!=0)%>%
  select(-species_code)

culardoch<-read.delim("CUL_culardoch.txt")%>%
  select(-id, -nutrients, -light, -carbon, -water, -other_manipulation, -num_manipulations, -experiment_year, -burn, -clip, -n,  -plot_mani, -species_num)%>%
  gather(species_code, abundance, sp1:sp43)%>%
  mutate(community_type=0, version = 1.0)
culardoch_names<-read.delim("CUL_culardoch_specieslist.txt")
culardoch2<-merge(culardoch, culardoch_names, by="species_code", all=T)%>%
  filter(abundance!=0)%>%
  select(-species_code)

gap2<-read.csv("DCGS_gap.csv")%>%
  mutate(community_type=0, version=ifelse(calendar_year<=2008, 1.0,2.0)) %>% filter(abundance!=0)

gcme <- read.csv("DL_GCME.csv")%>%
  mutate(version = 2.0, block = 0, community_type = 0)

gcme2 <- read.csv("DL_GCME2.csv")%>%
  mutate(version = 2.0, block = 0, community_type = 0)

d_precip <- read.csv("DL_Precip.csv")%>%
  mutate(version = 2.0, block = 0, community_type = 0)

nsfc<-read.csv("DL_NSFC.csv")%>%
 mutate(data_type=0, version = 2.0, block=0)

Nmow<-read.csv("EGN_Nmow.csv")%>%
  rename(abundance=cover)%>%
  mutate(version = 2.0, block=0)%>%
  separate(genus_species, into=c("g", "s", "a", "b", "c"), sep=" ", remove=F)%>%
  mutate(genus_species2=paste(g, s, sep=" "))%>%
  select(-genus_species, -g, -s, -a, -b, -c)%>%
  rename(genus_species=genus_species2)

warmnut<-read.delim("Finse_WarmNut.txt")%>%
  select(-id, -nutrients, -light, -carbon, -water, -other_manipulation, -num_manipulations, -experiment_year, -n, -p, -k, -temp,  -plot_mani, -species_num, -plot_id1)%>%
  gather(species_code, abundance, sp1:sp228)%>%
  mutate(community_type=0, version = 1.0)
warmnut_names<-read.delim("Finse_WarmNut_specieslist.txt")
warmnut2<-merge(warmnut, warmnut_names, by="species_code", all=T)%>%
  filter(abundance!=0)%>%
  select(-species_code)%>%
  filter(genus_species!="mosses and lichens")

gfert <- read.csv("Glen_Fert.csv") %>%
  mutate(community_type = 0, version = 2.0)%>%
  filter(!(genus_species %in% c("Bare Ground", "Dead Calluna", "lichen", "Liverwort", "Lycopodium sp. (clubmos)", "Rock", "Tiny moss")))


face<-read.delim("GVN_FACE.txt")%>%
  select(-id, -nutrients, -light, -carbon, -water, -other_manipulation, -num_manipulations, -experiment_year, -c,  -plot_mani, -species_num, -plot_id1)%>%
  gather(species_code, abundance, sp1:sp232)%>%
  mutate(community_type=0, block=0, version = 1.0)
face_names<-read.delim("GVN_FACE_specieslist.txt")
face2<-merge(face, face_names, by="species_code", all=T)%>%
  filter(abundance!=0)%>%
  select(-species_code)

warmnit <- read.csv("Hayoka_WarmNit.csv")%>%
  mutate(version = 2.0, community_type = 0, block = 0)

h_precip <- read.csv("HAYS_Precip.csv")

phace <- read.csv("CHY_PHACE.csv")%>%
  mutate(version = 2.0, community_type = 0)


nde <- read.csv("IMGERS_NDE.csv") %>% 
  mutate(community_type = 0, version = 1.0) %>% 
  filter(abundance !=0)

yu<-read.delim("IMGERS_Yu.txt")%>%
  gather(genus_species, abundance, Leymus.chinensis:Heteropappus.altaicus)%>%
  mutate(community_type=0, block = 0, data_type = "biomass", version = 1.0) %>%
  filter(abundance != 0) %>%
  select(-experiment_year)

study119<-read.delim("JRN_Study119.txt")%>%
  select(-id, -nutrients, -light, -carbon, -water, -other_manipulation, -num_manipulations, -experiment_year, -n,  -plot_mani, -species_num, -plot_id1)%>%
  gather(species_code, abundance, sp1:sp232)
study119_names<-read.delim("JRN_Study119_specieslist.txt")
study1192<-merge(study119, study119_names, by="species_code", all=T)%>%
  filter(abundance!=0)%>%
  select(-species_code)%>% mutate(version = 1.0) %>%
  #in 1986 the control was mistakenly fertilized; delete from there
  filter(calendar_year<1986)

study278<-read.delim("JRN_study278.txt")%>%
  select(-id, -nutrients, -light, -carbon, -water, -other_manipulation, -num_manipulations, -experiment_year, -n, -precip,  -plot_mani, -species_num)%>%
  gather(species_code, abundance, sp1:sp232)%>%
  mutate(community_type=0, version = 1.0)
study278_names<-read.delim("JRN_study278_specieslist.txt")
study2782<-merge(study278, study278_names,by="species_code", all=T)%>%
  filter(abundance!=0)%>%
  select(-species_code)

gce<-read.delim("JSP_GCE2.txt")%>%
  select(-id, -nutrients, -light, -carbon, -water, -other_manipulation, -num_manipulations, -experiment_year, -n, -precip, -temp, -c,  -plot_mani, -species_num, -plot_id1)%>%
  gather(species_code, abundance, sp1:sp92)%>%
  mutate(community_type=0, version = 1.0)
gce_names<-read.delim("JSP_GCE2_specieslist.txt")
gce2<-merge(gce, gce_names, by="species_code", all=T)%>%
  filter(abundance!=0)%>%
  select(-species_code)

wapaclip<-read.delim("KAEFS_WaPaClip.txt")%>%
  select(-id, -nutrients, -light, -carbon, -water, -other_manipulation, -num_manipulations, -experiment_year, -clip, -precip, -temp,  -plot_mani, -species_num, -plot_id1)%>%
  gather(species_code, abundance, sp1:sp232)%>%
  mutate(community_type=0, version = 1.0)
wapaclip_names<-read.delim("KAEFS_WaPaClip_specieslist.txt")
wapaclip2<-merge(wapaclip, wapaclip_names, by="species_code", all=T)%>%
  filter(abundance!=0)%>%
  select(-species_code)

t7<-read.delim("KBS_T7.txt")%>%
  select(-id, -nutrients, -light, -carbon, -water, -other_manipulation, -num_manipulations, -experiment_year, -dist,-n,  -plot_mani, -species_num)%>%
  gather(species_code, abundance, sp1:sp232)%>%
  mutate(community_type=0, version = 1.0)
t7_names<-read.delim("KBS_T7_specieslist.txt")%>%
  mutate(species_code=tolower(species_code))
t72<-merge(t7, t7_names, by="species_code", all=T)%>%
  filter(abundance!=0)%>%
  select(-species_code)

bffert<-read.delim("KLU_BFFert.txt")%>%
  select(-id, -nutrients, -light, -carbon, -water, -other_manipulation, -num_manipulations, -experiment_year, -p,-n,-k,-herb_removal,-plot_id1,  -plot_mani, -species_num)%>%
  gather(species_code, abundance, sp1:sp232)%>%
  mutate(community_type=0, version = 1.0)
bffert_names<-read.delim("KLU_BFFert_specieslist.txt")%>%
  mutate(species_code=tolower(species_code))
bffert2<-merge(bffert, bffert_names, by="species_code", all=T)%>%
  filter(abundance!=0)%>%
  select(-species_code)%>%
  filter(genus_species!="Moss")

kgfert<-read.delim("KLU_KGFert.txt")%>%
  select(-id, -nutrients, -light, -carbon, -water, -other_manipulation, -num_manipulations, -experiment_year, -p,-n,-k,-fungicide,  -plot_mani, -species_num)%>%
  gather(species_code, abundance, sp1:sp232)%>%
  mutate(community_type=0, version = 1.0)
kgfert_names<-read.delim("KLU_KGFert_specieslist.txt")%>%
  mutate(species_code=tolower(species_code))
kgfert2<-merge(kgfert, kgfert_names, by="species_code", all=T)%>%
  filter(abundance!=0)%>%
  select(-species_code)

bgp<-read.csv("KNZ_BGP.csv")%>%
  mutate(community_type=0, block=0, version=ifelse(calendar_year<=2015, 1.0,2.0)) %>%
  filter(abundance !=0) %>% filter(genus_species != "NA NA")

change<-read.csv("KNZ_SGS_change.csv")%>%
  mutate(community_type=0, version=2.0, data_type='cover') %>%
  select(-notes)%>%
  filter(abundance !=0)%>%
  filter(!(genus_species %in% c("Unknown forb", "unknown forb", "Unknown fungi")))

irg<-read.delim("KNZ_IRG.txt")%>%
  select(-id, -nutrients, -light, -carbon, -water, -other_manipulation, -num_manipulations, -experiment_year, -precip,  -plot_mani, -species_num)%>%
  gather(species_code, abundance, sp1:sp220)%>%
  mutate(block=0, version = 1.0)
irg_names<-read.delim("KNZ_IRG_specieslist.txt")
irg2<-merge(irg, irg_names, by="species_code", all=T)%>%
  filter(abundance!=0)%>%
  select(-species_code)

pplots<-read.csv("KNZ_pplots.csv")%>%
  mutate(site_code="KNZ", project_name="pplots", data_type="cover", community_type=0, block = 0, version=ifelse(calendar_year<=2015, 1.0,2.0))%>%
  filter(abundance!=0)
trtyr<-pplots%>%
  select(calendar_year)%>%
  unique()%>%
  arrange(calendar_year)%>%
  mutate(treatment_year=0:17)
pplots2<-pplots%>%
  left_join(trtyr)

ramps<-read.csv("KNZ_RaMPS.csv")%>%
  mutate(community_type=0, block = 0, version = 1.0)%>%
  filter(abundance!=0)

rhps<-read.csv("KNZ_RHPs.csv")%>%
  mutate(community_type=0, version=ifelse(calendar_year<=2015, 1.0,2.0)) %>%
  filter(abundance!=0)

gfp <- read.csv("KNZ_KNP_GFP.csv")%>%
  mutate(block = 0, version = 1.0)%>%
  filter(abundance!=0)
  

e2 <- read.csv("KUFS_E2.csv") %>%
  mutate(community_type = 0, version = 2.0) %>%
  filter(abundance !=0)

e6<-read.csv("KUFS_E6.csv")%>%
  filter(abundance!=0) %>% mutate(version=ifelse(calendar_year<=2013, 1.0,2.0))


clip<-read.delim("LATNJA_CLIP.txt")%>%
  select(-id, -nutrients, -light, -carbon, -water, -other_manipulation, -num_manipulations, -experiment_year, -n, -p, -temp,   -plot_mani, -species_num, -plot_id1)%>%
  gather(species_code, abundance, sp1:sp227)
clip_names<-read.delim("LATNJA_CLIP_specieslist.txt")
clip2<-merge(clip, clip_names, by="species_code", all=T)%>%
  filter(abundance!=0)%>% mutate(version = 1.0) %>%
  select(-species_code)

pme <- read.csv("LEFT_PME.csv") %>%
  mutate(community_type = 0, version = 1.0) %>%
  filter(abundance !=0)

herbwood<-read.delim("LG_HerbWood.txt")%>%
  select(-id, -nutrients, -light, -carbon, -water, -other_manipulation, -num_manipulations, -experiment_year, -n,-precip, -p, -k,   -plot_mani, -species_num, -plot_id1)%>%
  gather(species_code, abundance, sp1:sp232)%>%
  mutate(block=0,community_type=0, version = 1.0)
herbwood_names<-read.delim("LG_HerbWood_specieslist.txt")
herbwood2<-merge(herbwood, herbwood_names, by="species_code", all=T)%>%
  filter(abundance!=0)%>%
  select(-species_code)

fireplots<-read.delim("MAERC_fireplots.txt")%>%
  select(-id, -nutrients, -light, -carbon, -water, -other_manipulation, -num_manipulations, -experiment_year, -n,-burn, -p, -clip,   -plot_mani, -species_num)%>%
  gather(species_code, abundance, sp1:sp232)%>%
  mutate(community_type=0, version = 1.0)
fireplots_names<-read.delim("MAERC_fireplots_specieslist.txt")
fireplots2<-merge(fireplots, fireplots_names, by="species_code", all=T)%>%
  filter(abundance!=0)%>%
  select(-species_code)

mwatfer<-read.csv("MNR_watfer.csv")%>%
  mutate(data_type = "cover", version = 1.0)%>%
  filter(abundance!=0)

wet<-read.delim("NANT_wet.txt")%>%
  select(-id, -nutrients, -light, -carbon, -water, -other_manipulation, -num_manipulations, -experiment_year, -n, -p,   -plot_mani, -species_num, -plot_id1)%>%
  gather(species_code, abundance, sp1:sp101)%>%
  mutate(block=0, version = 1.0)
wet_names<-read.delim("NANT_wet_specieslist.txt")
wet2<-merge(wet, wet_names, by="species_code", all=T)%>%
  filter(abundance!=0)%>%
  select(-species_code)

gb<-read.delim("NGBER_gb.txt")%>%
  select(-id, -nutrients, -light, -carbon, -water, -other_manipulation, -num_manipulations, -experiment_year, -precip_vari_season,   -plot_mani, -species_num)%>%
  gather(species_code, abundance, sp1:sp21)%>%
  mutate(community_type=0, version = 1.0)
gb_names<-read.delim("NGBER_gb_specieslist.txt")
gb2<-merge(gb, gb_names, by="species_code", all=T)%>%
  filter(abundance!=0)%>%
  filter(treatment !="AMBIENT") %>%
  select(-species_code)

herbdiv<-read.csv("NIN_herbdiv.csv")%>%
  mutate(community_type=0, version=ifelse(calendar_year<=2015, 1.0,2.0))%>%
  filter(abundance!=0)
herbdiv <- herbdiv[which(duplicated(herbdiv) == FALSE),]

ccd<-read.delim("NTG_CCD.txt")%>%
  select(-id, -nutrients, -light, -carbon, -water, -other_manipulation, -num_manipulations, -experiment_year, -clip,-precip, -temp,   -plot_mani, -species_num)%>%
  gather(species_code, abundance, sp1:sp94)%>%
  mutate(community_type=0, block=0, version = 1.0)
ccd_names<-read.delim("NTG_CCD_specieslist.txt")
ccd2<-merge(ccd, ccd_names, by="species_code", all=T)%>%
  filter(site_code!='Saskatchewan')%>% #take Saskatchewan out because one plot needs to be dropped
  filter(abundance!=0)%>%
  select(-species_code)
sask <- merge(ccd, ccd_names, by="species_code", all=T)%>%
  filter(site_code=='Saskatchewan'&plot_id!=32)%>% #drop plot 32 because it is missing data from 2 years
  filter(abundance!=0)%>%
  select(-species_code)

Nprecip <- read.csv('Naiman_Nprecip.csv')%>%
  mutate(block=0)%>%
  select(-fertilization, -water, -Total.g.m2)

nutnet <- read.csv("NutNet.csv")%>%
  mutate(version = 2.0, community_type = 0) %>% 
  filter(genus_species!="Bryophyte",
         genus_species!="Bryophyte ",
         genus_species!="Forb sp.",
         genus_species!="Lichen", 
         genus_species!="Unknown", 
         genus_species!="Unknown ",
         genus_species!="Unknown dicot",
         genus_species!="Unknown fabaceae",
         genus_species!="Unknown grass",
         genus_species!="Unknown grass ",
         genus_species!="Unknown grass sp.",
         genus_species!="Unknown sp.")

nfert<-read.delim("NWT_246NFert.txt")%>%
  select(-id, -nutrients, -light, -carbon, -water, -other_manipulation, -num_manipulations, -experiment_year, -n, -plot_mani, -species_num, -plot_id1)%>%
  gather(species_code, abundance, sp1:sp232)%>%
  mutate(community_type=0, version = 1.0)
nfert_names<-read.delim("NWT_246NFert_specieslist.txt")
nfert2<-merge(nfert, nfert_names, by="species_code", all=T)%>%
  filter(abundance!=0) %>%
  select(-species_code)

atwe<-read.csv("NWT_ATWE.csv")%>%
  mutate(version=2.0, block=0, data_type='cover')

bowman<-read.delim("NWT_bowman.txt")%>%
  select(-id, -nutrients, -light, -carbon, -water, -other_manipulation, -num_manipulations, -experiment_year, -n, -p,   -plot_mani, -species_num)%>%
  gather(species_code, abundance, sp1:sp232)
bowman_names<-read.delim("NWT_bowman_specieslist.txt")
bowman2<-merge(bowman, bowman_names, by="species_code", all=T)%>%
  filter(abundance!=0)%>% mutate(version = 1.0) %>%
  select(-species_code)

snow<-read.csv("NWT_snow.csv")%>%
  mutate(community_type=0, version=ifelse(calendar_year<=2012, 1.0,2.0))%>%
  filter(abundance!=0) %>% 
  filter(genus_species != "Litter", genus_species != "Lichen", 
         genus_species != "Moss", genus_species != "Bare Ground", 
         genus_species != "Rock, fragments")

oface<-read.delim("ORNL_FACE.txt")%>%
  select(-id, -nutrients, -light, -carbon, -water, -other_manipulation, -num_manipulations, -experiment_year, -c,   -plot_mani, -species_num, -plot_id1)%>%
  gather(species_code, abundance, sp1:sp232)%>%
  mutate(community_type=0, block=0, version = 1.0)
oface_names<-read.delim("ORNL_FACE_specieslist.txt")
oface2<-merge(oface, oface_names, by="species_code", all=T)%>%
  filter(abundance!=0)%>%
  select(-species_code)

tide<-read.delim("PIE_Tide.txt")%>%
  select(-id, -nutrients, -light, -carbon, -water, -other_manipulation, -num_manipulations, -experiment_year, -n,   -plot_mani, -species_num)%>%
  gather(species_code, abundance, sp1:sp232)%>%
  mutate(community_type=0, version = 1.0)
tide_names<-read.delim("PIE_Tide_specieslist.txt")
tide2<-merge(tide, tide_names, by="species_code", all=T)%>%
  filter(abundance!=0)%>%
  select(-species_code)

nut <- read.csv("Rengen_Nut.csv")%>% #species '...41', what is this? ask PIs
  mutate(version = 2.0, community_type = 0, block = 0)%>%
  filter(!(genus_species %in% c('...41', 'Cover of forbs', 'Cover of graminoids', 'Cover of legumes', 'Cover of short forbs', 'Cover of short forbs with legumes', 'Cover of short graminoids', 'Cover of tall graminoids', 'number of all species', 'number of species > 1%', 'short forbs (number of species)', 'short graminoids (number of species)', 'tall grasses (number of species)', 'Total cover', 'Cover of tall forbs', 'Cover of tall forbs with legumes', 'tall forbs (number of species)', 'number of woods', 'cover of woods')))%>%
  mutate(genus_species=ifelse(genus_species=='Picea abies seedling', 'Picea abies', as.character(genus_species)))

interaction<-read.delim("RIO_interaction.txt")%>%
  select(-n, -precip, -precip_vari, -plot_mani)%>%
  gather(species_code, abundance, sp1:sp10)%>%
  mutate(block=0, version = 1.0)
interaction_names<-read.delim("RIO_interaction_specieslist.txt")
interaction2<-merge(interaction, interaction_names, by="species_code", all=T)%>%
  filter(abundance!=0)%>%
  select(-species_code)

lucero <- read.csv("SCL_Lucero.csv") %>%
  mutate(community_type = 0, version = 1.0) %>%
  filter(abundance !=0) %>%
  filter(genus_species!="Standing.dead")

ter <- read.csv("SCL_TER.csv") %>%
  mutate(community_type = 0, version = 1.0) %>%
  filter(abundance != 0)

cxn <- read.csv("SERC_CXN.csv") 

tmece <- read.csv("SERC_TMECE.csv") 
  
sev_edge <- read.csv("SEV_EDGE20.csv") %>%
  filter(abundance != 0) %>% mutate(version = 2.0)

snfert3 <- read.csv("SEV_NFERT20.csv") %>%
  mutate(community_type = 0, block = 0, version = 2.0) %>%
  filter(abundance!= 0)%>%
  select(-project_name)%>%
  mutate(project_name="Nfert")
  
# wenndex<-read.delim("SEV_WENNDEx.txt")%>%
#   select(-id, -nutrients, -light, -carbon, -water, -other_manipulation, -num_manipulations, -experiment_year, -n, -temp, -precip,   -plot_mani, -species_num)%>%
#   gather(species_code, abundance, sp1:sp232)%>%
#   mutate(community_type=0, block=0, version = 1)
# wenndex_names<-read.delim("SEV_WENNDEx_specieslist.txt")
# wenndex2<-merge(wenndex, wenndex_names, by="species_code", all=T)%>%
#   filter(abundance!=0)%>% 
#   select(-species_code) 
### Removing old file because ANPP per species is based on cover of species. 
wenndex3 <- read.csv("SEV_WENNDEx20.csv") %>%
  mutate(community_type = 0, block = 0, version = 2) %>%
  filter(abundance !=0)
# wenndex4 <- rbind(wenndex2, wenndex3)

graze <- read.csv("SFREC_GrazePrecip.csv") %>%
  filter(abundance != 0) %>% mutate(version = 1.0)

s_irg <- read.csv("SGS_Irg.csv")%>%
  mutate(version = 2.0, community_type = 0, block = 0)%>%
  filter(!(genus_species %in% c('Unknown')))

s_drought <- read.csv("SGS_Drought.csv")%>%
  mutate(version = 2.0, community_type = 0, block = 0)%>%
  filter(!(genus_species %in% c('Unknown')))

ton <- read.csv("SIU_TON.csv") %>%
  mutate(community_type = 0, version = 2.0) %>%
  filter(genus_species != "Soil", 
         genus_species != "Grass", 
         genus_species != "Moss", 
         genus_species != "Unknown 2",
         genus_species != "Unknown 6", 
         genus_species != "Poly 4", 
         genus_species != "Mint")

uk<-read.delim("SKY_UK.txt")%>%
  select(-id, -nutrients, -light, -carbon, -water, -other_manipulation, -num_manipulations, -temp, -precip, -plot_id1,   -plot_mani, -species_num)%>%
  gather(species_code, abundance, sp1:sp26)%>%
  mutate(community_type=0, version = 1.0)
uk_names<-read.delim("SKY_UK_specieslist.txt")
uk2<-merge(uk, uk_names, by="species_code", all=T)%>%
  filter(abundance!=0)%>%
  select(-species_code)

nitrogen <- read.csv("SR_Nitrogen.csv") %>%
  mutate(version = 1.0) %>%
  filter(abundance !=0, 
         genus_species!="UNKNOWN SPECIES", 
         genus_species!="GRASS SPECIES", 
         genus_species!="FORB SPECIES")

water <- read.csv("SR_Water.csv") %>%
  mutate(version = 1.0) %>%
  filter(abundance != 0, 
         genus_species!="UNKNOWN SPECIES", 
         genus_species!="GRASS SPECIES", 
         genus_species!="FORB SPECIES")

gane<-read.delim("SVA_GANE.txt")%>%
  select(-id, -nutrients, -light, -carbon, -water, -other_manipulations, -num_manipulations, -experiment_year, -n, -p,   -plot_mani, -species_num)%>%
  gather(species_code, abundance, sp1:sp232)%>%
  mutate(community_type=0, block=0, version = 1.0)
gane_names<-read.delim("SVA_GANE_specieslist.txt")
gane2<-merge(gane, gane_names, by="species_code", all=T)%>%
  filter(abundance!=0)%>%
  select(-species_code)

tface <- read.csv("TAS_FACE.csv") %>%
  mutate(community_type = 0, block = 0, version = 1.0) %>%
  select(-X) %>% filter (abundance != 0)

lovegrass <- read.csv("TRA_Lovegrass.csv") %>%
  mutate(community_type = 0, version = 1.0) %>%
  filter(abundance != 0)%>%
  filter(genus_species!="Lichen")

edge <- read.csv("USA_EDGE.csv") %>% ## Added new data 2020
  mutate(data_type = "cover", version = 1.0)%>%
  filter(abundance !=0, site_code != "SEV")

vcrnutnet <- read.csv('VCR_NutNet.csv')%>%
  mutate(community_type = 0, version = 2.0, data_type='cover')%>%
  rename(plot_id=plot, genus_species=taxa, abundance=cover)%>%
  select(calendar_year, treatment, plot_id, data_type, treatment_year, site_code, project_name, community_type, genus_species, abundance, community_type, block, version)%>%
  filter(abundance != 0, !(genus_species %in% c('Litter', 'Bare_ground')))

nitadd <- read.csv("YMN_NitAdd.csv") %>%
  mutate(community_type = 0, block = 0, version = 1.0) %>%
  filter(abundance != 0)

names<-nitadd%>%
  select(genus_species)%>%
  unique()

#merge all datasets
combine<-rbind(atwe, bffert2, bgp, biocon, bowman2, btdrought, btnpkd, ccd2, change, clip2, clonal2, culardoch2, cxn, d_precip, e001, e0023, e2, e6, edge, eelplot, events2, exp12, face2, fert1, fert3, fireplots2, gane2, gap2, gb2, gce2, gcme, gcme2, gfert, gfp, graze, h_precip, herbdiv, herbwood2, imagine2, interaction2, irg2, kgfert2, lind2, lovegrass,  lucero, mat22, megarich2, mnt2, mwatfer, nde, nfert2, nitadd, nitphos, nitrogen, Nmow, Nprecip, nsfc, nut, nutnet, oface2, pennings2, phace, pme, pplots2, pq2, ramps, rhps, rmapc2, s_drought, s_irg, sask, sev_edge, snfert3, snow,  study1192, study2782, t72, ter, tface, tide2, tmece, ton, uk2, wapaclip2, warmnut2, warmnit, water, watering2,  wenndex3, wet2, vcrnutnet, yu)%>%
  filter(abundance!='NA')

#cleaning the speices name to remove double spaces "\\s\\", remove "." and "_" and clean spaces at at front and end of names 

combine2 <- combine %>% 
  mutate(genus_species1 = trimws(genus_species, which='both'))%>%
  mutate(genus_species2 = gsub("\\s\\s"," ",genus_species1, perl = TRUE)) %>%
  mutate(genus_species3 = gsub("\\s\\s"," ",genus_species2, perl = TRUE)) %>%
  mutate(genus_species4 = gsub("[.]"," ",genus_species3))  %>%
  mutate(genus_species5 = trimws(genus_species4, which='both'))%>%
  mutate(genus_species6 = gsub("_", " ", genus_species5, fixed = TRUE))%>%
  mutate(genus_species7 = gsub("\u00A0", " ",genus_species6, fixed = TRUE))%>%
  mutate(genus_species8 = tolower(genus_species7))%>%
  select(-genus_species)%>%
  rename(genus_species=genus_species8)%>%
  select(-genus_species1, -genus_species2, -genus_species3, -genus_species4, -genus_species5, -genus_species6, -genus_species7)
           

# write.csv(combine2, "C:/Users/lapie/Dropbox (Smithsonian)/working groups/CoRRE/CoRRE_database/Data/CompiledData/RawAbundance.csv")
# write.csv(combine2, "C:/Users/mavolio2/Dropbox/CoRRE_database/Data/CompiledData/RawAbundance.csv", row.names = F)

###get species list
species_list<-combine2%>%
  select(genus_species)%>%
  unique()

# write.csv(species_list, "C:/Users/lapie/Dropbox (Smithsonian)/working groups/CoRRE/CoRRE_database/Data/CompiledData/Species_lists/SpeciesList_Nov2021.csv", row.names=F)
# write.csv(species_list, "C:/Users/mavolio2/Dropbox/CoRRE_database/Data/CompiledData/Species_lists/SpeciesList_Nov2021.csv", row.names=F)

###Getting Relative Cover
totcov<-combine2%>%
  group_by(site_code, project_name, community_type, calendar_year, treatment_year, treatment, block, plot_id, data_type)%>%
  summarise(totcov=sum(abundance))%>%
  ungroup()

relcov<-merge(totcov, combine2, by=c("site_code", "project_name", "community_type", "calendar_year", "treatment_year", "treatment", "block", "plot_id", "data_type"))%>%
  mutate(relcov=abundance/totcov)%>%
  select(-abundance, -totcov)

# write.csv(relcov, "C:/Users/lapie/Dropbox (Smithsonian)/working groups/CoRRE/CoRRE_database/Data/CompiledData/RelativeCover.csv", row.names = FALSE)

# write.csv(relcov, "C:/Users/mavolio2/Dropbox/CoRRE_database/Data/CompiledData/RelativeCover.csv", row.names = FALSE)


# ##### Relative cover and raw abundance for sCoRRE
# 
# write.csv(sCoRRERaw, "~/Dropbox/sDiv_sCoRRE_shared/CoRRE data/CoRRE data/community composition/CoRRE_RawAbundance_Dec2021.csv", row.names = FALSE)  
# sCoRRERel <- relcov[-which(relcov$project_name %in% c("BioCON", "EELplot") & relcov$data_type == "cover"),]
# write.csv(sCoRRERel, "~/Dropbox/sDiv_sCoRRE_shared/CoRRE data/CoRRE data/community composition/CoRRE_RelativeAbundance_Dec2021.csv", row.names = FALSE)  
