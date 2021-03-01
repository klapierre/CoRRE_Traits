setwd("~/Dropbox/CoRRE_database/Data/CleanedData/Sites/Species csv")
# Add your working directories here if this doesn't work for you
#setwd('C:\\Users\\lapie\\Dropbox (Smithsonian)\\working groups\\CoRRE\\CoRRE_database\\Data\\CleanedData\\Sites\\Species csv') #kim's laptop
# MEGHAN
library(gtools)
library(reshape2)
library(tidyr)
library(dplyr)
library(stringr)

#kim's notes 2/8/2021: 
#Rengen_Nut has a species called '...41', ask PIs what this is
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
  mutate(version = 2.0, community_type = 0, block = 0)
  
fert2 <- read.csv("ANR_Fert2.csv") %>%
  mutate(version = 2.0, community_type = 0) #has "Other lichens" and "Pioneer mosses (unspecified), which make up a large part of the community

mat2<-read.delim("ARC_mat2.txt")%>%
  select(-id, -nutrients, -light, -carbon, -water, -other_manipulation, -num_manipulations, -experiment_year, -n, -p, -plot_mani, -species_num)%>%
  gather(species_code, abundance, sp1:sp232)%>%
  mutate(community_type=0, version = 1.0)
mat2_names<-read.delim("ARC_mat2_specieslist.txt")
mat22<-merge(mat2, mat2_names, by="species_code", all=T)%>%
  filter(abundance!=0)%>%
  filter(genus_species!="Caribou feces")%>%
  select(-species_code)

mnt<-read.delim("ARC_mnt.txt")%>%
  select(-id, -nutrients, -light, -carbon, -water, -other_manipulation, -num_manipulations, -experiment_year, -n, -p, -plot_mani,   -species_num, -plot_id1)%>%
  gather(species_code, abundance, sp1:sp232)%>%
  mutate(community_type=0, version = 1.0)
mnt_names<-read.delim("ARC_mnt_specieslist.txt")
mnt2<-merge(mnt, mnt_names, by="species_code", all=T)%>%
  filter(abundance!=0)%>%
  filter(genus_species!="Nostoc sp.")%>%
  filter(genus_species!="Caribou feces")%>%
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
  mutate(version = 2.0, community_type = 0)

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
  mutate(community_type=0, version = 1.0) %>% filter(abundance!=0)

e001<-read.csv("CDR_e001.csv")%>%
  mutate(block=0, version=ifelse(calendar_year<=2013, 1.0,2.0))%>%
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
  select(-spcode)

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

gcme <- read.csv("DCMIC_GCME.csv")%>%
  mutate(version = 2.0, block = 0, community_type = 0)

gcme2 <- read.csv("DCMIC_GCME2.csv")%>%
  mutate(version = 2.0, block = 0, community_type = 0)

d_precip <- read.csv("DCMIC_Precip.csv")%>%
  mutate(version = 2.0, block = 0, community_type = 0)

nsfc<-read.delim("DL_NSFC.txt")%>%
  select(-id, -nutrients, -light, -carbon, -water, -other_manipulation, -num_manipulations, -experiment_year, -n, -precip, -plot_mani, -species_num)%>%
  gather(species_code, abundance, sp1:sp53)%>%
  mutate(community_type=0, version = 1.0)
nsfc_names<-read.delim("DL_NSFC_specieslist.txt")%>%
  mutate(species_code=tolower(species_list))
nsfc2<-merge(nsfc, nsfc_names, by="species_code", all=T)%>%
  filter(abundance!=0)%>%
  select(-species_code, -species_list)
nsfc3 <- read.csv("DL_NSFC20132016.csv") %>%
  mutate(community_type = 0, version = 2.0)
nsfc4 <- rbind(nsfc2, nsfc3)

warmnut<-read.delim("Finse_WarmNut.txt")%>%
  select(-id, -nutrients, -light, -carbon, -water, -other_manipulation, -num_manipulations, -experiment_year, -n, -p, -k, -temp,  -plot_mani, -species_num, -plot_id1)%>%
  gather(species_code, abundance, sp1:sp228)%>%
  mutate(community_type=0, version = 1.0)
warmnut_names<-read.delim("Finse_WarmNut_specieslist.txt")
warmnut2<-merge(warmnut, warmnut_names, by="species_code", all=T)%>%
  filter(abundance!=0)%>%
  select(-species_code)

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

h_precip <- read.csv("HAYS_Precip.csv")%>%
  mutate(version = 2.0, community_type = 0) %>%
  filter(genus_species!="Cacti")

phace <- read.csv("HPGRS_PHACE.csv")%>%
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
  select(-species_code)

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

irg<-read.delim("KNZ_IRG.txt")%>%
  select(-id, -nutrients, -light, -carbon, -water, -other_manipulation, -num_manipulations, -experiment_year, -precip,  -plot_mani, -species_num)%>%
  gather(species_code, abundance, sp1:sp220)%>%
  mutate(block=0, version = 1.0)
irg_names<-read.delim("KNZ_IRG_specieslist.txt")
irg2<-merge(irg, irg_names, by="species_code", all=T)%>%
  filter(abundance!=0)%>%
  select(-species_code)

pplots<-read.csv("KNZ_PPLOTS.csv")%>%
  mutate(community_type=0, block = 0, version=ifelse(calendar_year<=2015, 1.0,2.0))%>%
  filter(abundance!=0)

ramps<-read.csv("KNZ_RaMPS.csv")%>%
  mutate(community_type=0, block = 0, version = 1.0)%>%
  filter(abundance!=0)

rhps<-read.csv("KNZ_RHPs.csv")%>%
  mutate(community_type=0, version=ifelse(calendar_year<=2015, 1.0,2.0)) %>%
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
  mutate(genus_species=species_name, data_type = "cover", version = 1.0)%>%
  select(-species_name, -species)%>%
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
  select(-species_code)

herbdiv<-read.csv("NIN_herbdiv.csv")%>%
  mutate(community_type=0, version=ifelse(calendar_year<=2015, 1.0,2.0))%>%
  filter(abundance!=0)

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
  filter(!(genus_species %in% c('Cover of forbs', 'Cover of graminoids', 'Cover of legumes', 'Cover of short forbs', 'Cover of short forbs with legumes', 'Cover of short graminoids', 'Cover of tall graminoids', 'number of all species', 'number of species > 1%', 'short forbs (number of species)', 'short graminoids (number of species)', 'tall grasses (number of species)', 'Total cover', 'Cover of tall forbs', 'Cover of tall forbs with legumes', 'tall forbs (number of species)', 'number of woods', 'cover of woods')))%>%
  mutate(genus_species=ifelse(genus_species=='Picea abies seedling', 'Picea abies', as.character(genus_species)))

interaction<-read.delim("RIO_interaction.txt")%>%
  select(-n, -precip, -precip_vari, -plot_mani, -data_type)%>%
  gather(species_code, abundance, sp1:sp10)%>%
  mutate(block=0, version = 1.0)
interaction_names<-read.delim("RIO_interaction_specieslist.txt")
interaction2<-merge(tide, tide_names, by="species_code", all=T)%>%
  filter(abundance!=0)%>%
  select(-species_code)

lucero <- read.csv("SCL_Lucero.csv") %>%
  mutate(community_type = 0, version = 1.0) %>%
  filter(abundance !=0) %>%
  filter(genus_species!="Standing dead")

ter <- read.csv("SCL_TER.csv") %>%
  mutate(community_type = 0, version = 1.0) %>%
  filter(abundance != 0)

cxn <- read.csv("SERC_CXN.csv") %>%
  mutate(block = 0, community_type = 0, version = 1.0) %>%
  filter(abundance != 0)

tmece <- read.csv("SERC_TMECE.csv") %>%
  mutate(block = 0, version = 1.0) %>%
  filter(abundance != 0)
  
sev_edge <- read.csv("SEV_EDGE20.csv") %>%
  filter(abundance != 0) %>% mutate(version = 2.0)

###mrme <- read.csv("SEV_MRME.csv")

snfert<-read.delim("SEV_NFert.txt")%>%
  select(-id, -nutrients, -light, -carbon, -water, -other_manipulation, -num_manipulations, -experiment_year, -n, -plot_mani, -species_num)%>%
  gather(species_code, abundance, sp1:sp232)%>%
  mutate(community_type=0, block=0, version = 1.0)
snfert_names<-read.delim("SEV_NFert_specieslist.txt")%>%
  mutate(species_code=tolower(species_code))
snfert2<-merge(snfert, snfert_names, by="species_code", all=T)%>%
  filter(abundance!=0)%>%
  select(-species_code)
snfert3 <- read.csv("SEV_NFERT20.csv") %>%
  mutate(community_type = 0, block = 0, version = 2.0) %>%
  filter(abundance!= 0)
snfert4 <- rbind(snfert2,snfert3)
  
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

s_precip <- read.csv("SGS_Precip.csv")%>%
  mutate(version = 2.0, community_type = 0, block = 0)%>%
  filter(!(genus_species %in% c('Unknown')))

nash <- read.csv("Sil_NASH.csv") %>%
  mutate(version = 2.0, community_type = 0)

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

climarid <- read.csv("SORBAS_CLIMARID.csv") %>%
  mutate(version = 2.0, community_type = 0, block = 0)

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
  filter(abundance != 0)

edge <- read.csv("USA_EDGE.csv") %>% ## Added new data 2020
  mutate(data_type = "cover", version = 1.0)%>%
  filter(abundance !=0)

shet <- read.csv("WAG_SHet.csv") %>%
  mutate(version = 2.0, community_type = 0)

nitadd <- read.csv("YMN_NitAdd.csv") %>%
  mutate(community_type = 0, block = 0, version = 1.0) %>%
  filter(abundance != 0)

#merge all datasets
combine<-rbind(bffert2, bgp, biocon, bowman2, ccd2, climarid, clip2, clonal2, culardoch2, cxn, d_precip, e001, e0023,
               e2, e6, edge, eelplot, events2, exp12, face2, fert1, fert2, fireplots2, gane2, gap2, gb2, gce2, 
               gcme, gcme2, graze, h_precip, herbdiv, herbwood2, imagine2, interaction2, irg2, kgfert2, lind2, lovegrass, 
               lucero, mat22, megarich2, mnt2, mwatfer, nash, nde, nfert2, nitadd, nitphos, nitrogen, nsfc4, nut, nutnet,
               oface2, pennings2, phace, pme, pplots, pq2, ramps, rhps, rmapc2, s_precip, sask, sev_edge, snfert3, snow, 
               study1192, study2782, t72, ter, tface, tide2, tmece, ton, uk2, wapaclip2, warmnut2, warmnit, water, watering2, 
               wenndex3, wet2, yu)

combine <- combine %>% mutate(genus_species = trimws(genus_species, 'both')) %>%
  mutate(genus_species = gsub("\\s\\s"," ",genus_species, perl = TRUE)) %>%
  mutate(genus_species = gsub("\\s\\s"," ",genus_species, perl = TRUE)) %>%
  mutate(genus_species = gsub("[.]"," ",genus_species))  %>%
  mutate(genus_species = gsub("\u00A0", " ",genus_species, fixed = TRUE))

combine$genus_species <- str_trim(combine$genus_species, "right") # get rid of spaces after full species name
combine$genus_species <- tolower(combine$genus_species)

write.csv(combine, "~/Dropbox/CoRRE_database/Data/CompiledData/RawAbundance.csv")

###get species list
species_list<-combine%>%
  select(genus_species)%>%
  unique()

write.csv(species_list, "~/Dropbox/CoRRE_database/Data/CompiledData/Species_lists/SpeciesList.csv")

###Getting Relative Cover
totcov<-combine%>%
  tbl_df()%>%
  group_by(site_code, project_name, community_type, calendar_year, treatment_year, treatment, block, plot_id, data_type)%>%
  summarise(totcov=sum(abundance))

relcov<-merge(totcov, combine, by=c("site_code", "project_name", "community_type", "calendar_year", "treatment_year", "treatment", "block", "plot_id", "data_type"))%>%
  mutate(relcov=abundance/totcov)%>%
  select(-abundance, -totcov)

write.csv(relcov, "~/Dropbox/CoRRE_database/Data/CompiledData/RelativeCover.csv")


##### Relative cover and raw abundance for sCoRRE

sCoRRERaw <- combine[-which(combine$project_name %in% c("BioCON", "EELplot", "NASH") & combine$data_type == "cover"),]
write.csv(sCoRRERaw, "~/Dropbox/sDiv_sCoRRE_shared/CoRRE data/CoRRE data/community composition/CoRRE_RawAbundance_Feb2021.csv", row.names = FALSE)  
sCoRRERel <- relcov[-which(relcov$project_name %in% c("BioCON", "EELplot", "NASH") & relcov$data_type == "cover"),]
write.csv(sCoRRERel, "~/Dropbox/sDiv_sCoRRE_shared/CoRRE data/CoRRE data/community composition/CoRRE_RelativeAbundance_Feb2021.csv", row.names = FALSE)  
