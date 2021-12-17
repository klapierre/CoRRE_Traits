setwd("~/Dropbox/CoRRE_database/Data/CleanedData/Sites/ANPP csv")
setwd('C:\\Users\\lapie\\Dropbox (Smithsonian)\\working groups\\CoRRE\\CoRRE_database\\Data\\CleanedData\\Sites\\ANPP csv') #kim's laptop

library(gtools)
library(tidyverse)

# Read in sites
watering<-read.delim("ANG_watering_anpp.txt")%>%
  select(site_code, project_name, treatment_year, calendar_year, treatment, plot_id, anpp)%>%
  mutate(community_type=0, block = 0)
clonal<-read.delim("ASGA_Clonal_anpp.txt")%>%
  select(site_code, project_name, treatment_year, calendar_year, treatment, plot_id, anpp, block)%>%
  mutate(community_type=0)
exp1<-read.delim("ASGA_Exp1_anpp.txt")%>%
  select(site_code, project_name, treatment_year, calendar_year, treatment, plot_id, anpp, community_type, block)
eel <- read.csv("AZI_EELplot_anpp.csv")%>%
  select(site_code, project_name, treatment_year, calendar_year, treatment, plot_id, anpp)%>%
  mutate(community_type=0, block = 0)
nit <- read.csv("AZI_NitPhos.csv") %>%
  select(site_code, project_name, treatment_year, calendar_year, treatment, plot_id, anpp) %>%
  mutate(community_type = 0, block = 0)
lind<-read.delim("BAY_LIND_anpp.txt")%>%
  select(site_code, project_name, treatment_year, calendar_year, treatment, plot_id, anpp, block)%>%
  mutate(community_type=0)
events<-read.delim("Bt_EVENT2_anpp.txt")%>%
  select(site_code, project_name, treatment_year, calendar_year, treatment, plot_id, anpp, block)%>%
  mutate(community_type=0)
btdrought <- read.csv("Bt_DroughtNet_anpp.csv", row.names = 1) %>%
  mutate(community_type = 0, block = 0)
btnpkd <- read.csv("Bt_NPKDNet_anpp.csv") %>%
  mutate(community_type = 0, block = 0)
rmapc<-read.delim("CAU_RMAPC_anpp.txt")%>%
  select(site_code, project_name, community_type, treatment_year, calendar_year, treatment, plot_id, anpp) %>%
  mutate(block = 0)
biocon<-read.csv("CDR_BioCON_anpp.csv")%>%
  select(site_code, project_name, treatment_year, calendar_year, treatment, plot_id, anpp, block)%>%
  mutate(community_type=0)
e001<-read.csv("CDR_e001_anpp.csv")%>%
  select(site_code, project_name, community_type, treatment_year, calendar_year, treatment, plot_id, anpp) %>%
  mutate(block = 0)
e002<-read.delim("CDR_e002_anpp.txt")%>%
  select(site_code, project_name, community_type, treatment_year, calendar_year, treatment, plot_id, anpp) %>%
  mutate(block = 0)
megarich<-read.delim("CEH_Megarich_anpp.txt")%>%
  select(site_code, project_name, treatment_year, calendar_year, treatment, plot_id, anpp, block)%>%
  mutate(community_type=0)
imagine<-read.delim("CLE_imagine_anpp.txt")%>%
  select(site_code, project_name, treatment_year, calendar_year, treatment, plot_id, anpp, block)%>%
  mutate(community_type=0)
nsfc<-read.csv("DL_NSFC_anpp.csv")%>%
  mutate(block=0)%>%
  select(site_code, project_name, treatment_year, calendar_year, treatment, plot_id, anpp, block)%>%
  mutate(community_type=0)
phace <- read.csv("CHY_PHACE_anpp.csv")%>%
  select(site_code, project_name, treatment_year, calendar_year, treatment, plot_id, block, anpp) %>%
  mutate(community_type = 0)
NDE <- read.csv("IMGERS_NDE_anpp.csv") %>%
  select(site_code, project_name, treatment_year, calendar_year, treatment, plot_id, anpp, block)%>%
  mutate(community_type=0)
imgers<-read.delim("IMGERS_Yu_anpp.txt")%>%
  select(site_code, project_name, treatment_year, calendar_year, treatment, plot_id, anpp)%>%
  mutate(community_type=0, block = 0)
wapaclip<-read.delim("KAEFS_WaPaClip_anpp.txt")%>%
  select(site_code, project_name, treatment_year, calendar_year, treatment, plot_id, anpp, block)%>%
  mutate(community_type=0)
t7<-read.delim("KBS_T7_anpp.txt")%>%
  select(site_code, project_name, treatment_year, calendar_year, treatment, plot_id, anpp, block)%>%
  mutate(community_type=0)
kgfert<-read.delim("KLU_KGFert_anpp.txt")%>%
  select(site_code, project_name, treatment_year, calendar_year, treatment, plot_id, anpp)%>%
  mutate(community_type=0, block = 0)
bgp<-read.csv("KNZ_BGP_anpp.csv")%>%
  select(site_code, project_name, treatment_year, calendar_year, treatment, plot_id, anpp)%>%
  mutate(community_type=0, block = 0)
change<-read.csv("KNZ_SGS_change_anpp.csv")%>%
  select(site_code, project_name, treatment_year, calendar_year, treatment, block, plot_id, anpp)%>%
  mutate(community_type=0)
irg<-read.delim("KNZ_IRG_anpp.txt")%>%
  select(site_code, project_name, community_type, treatment_year, calendar_year, treatment, plot_id, anpp, block)
pplots<-read.csv("KNZ_PPLOTS_anpp.csv")%>%
  select(site_code, project_name, treatment_year, calendar_year, treatment, plot_id, anpp)%>%
  mutate(community_type=0, block = 0)
ramps<-read.csv("KNZ_Ramps_anpp.csv")%>%
  select(site_code, project_name, treatment_year, calendar_year, treatment, plot_id, anpp, block)%>%
  mutate(community_type=0)
rhps<-read.csv("KNZ_RHPs_anpp.csv")%>%
  select(site_code, project_name, treatment_year, calendar_year, treatment, plot_id, anpp, block)%>%
  mutate(community_type=0)
gfp <- read.csv("KNZ_KNP_GFP_anpp.csv") %>%
  select(site_code, project_name, treatment_year, calendar_year, treatment, plot_id, anpp, community_type) %>%
  mutate(block = 0)
e2 <- read.csv("KUFS_E2_anpp.csv")%>%
  select(site_code, project_name, treatment_year, calendar_year, treatment, plot_id, anpp, block) %>%
  mutate(community_type = 0)
e6 <- read.csv("KUFS_E6_anpp.csv")%>%
  select(site_code, project_name, treatment_year, calendar_year, treatment, plot_id, anpp, community_type, block)
pme <- read.csv("LEFT_PME_anpp.csv")%>%
  select(site_code, project_name, treatment_year, calendar_year, treatment, plot_id, anpp, block) %>%
  mutate(community_type = 0)
fireplots<-read.csv("MAERC_fireplots_anpp.csv")
names(fireplots)[1]<-'site_code'
fireplots<- fireplots%>%
  select(site_code, project_name, treatment_year, calendar_year, treatment, plot_id, anpp, block)%>%
  mutate(community_type=0)
mnr<-read.delim("MNR_watfer_anpp.txt")%>%
  select(site_code, project_name, treatment_year, calendar_year, treatment, plot_id, anpp)%>%
  mutate(community_type=0, block = 0)
wet<-read.delim("NANT_wet_anpp.txt")%>%
  select(site_code, project_name, community_type, treatment_year, calendar_year, treatment, plot_id, anpp) %>%
  mutate(block = 0)
gb<-read.delim("NGBER_gb_anpp.txt")%>%
  select(site_code, project_name, treatment_year, calendar_year, treatment, plot_id, anpp, block)%>%
  mutate(community_type=0)
Nprecip <- read.csv('Naiman_Nprecip_anpp.csv')%>%
  select(site_code, project_name, community_type, treatment_year, calendar_year, treatment, plot_id, anpp)%>%
  mutate(block=0)
nutnet <- read.csv("NutNet_anpp.csv")%>%
  select(site_code, project_name, treatment_year, calendar_year, treatment, plot_id, anpp, block)%>%
  mutate(community_type=0)
nfert<-read.delim("NWT_246NFert_anpp.txt")%>%
  select(site_code, project_name, treatment_year, calendar_year, treatment, plot_id, anpp, block)%>%
  mutate(community_type=0)
atwe<-read.csv("NWT_ATWE_anpp.csv")%>%
  select(site_code, project_name, treatment_year, calendar_year, treatment, plot_id, anpp)%>%
  mutate(community_type=0, block=0)
bowman<-read.delim("NWT_bowman_anpp.txt")%>%
  select(site_code, project_name, community_type, treatment_year, calendar_year, treatment, plot_id, anpp) %>%
  mutate(block = 0)
snow <- read.csv("NWT_snow_anpp.csv")%>%
  select(site_code, project_name, treatment_year, calendar_year, treatment, plot_id, anpp, block)%>%
  mutate(community_type = 0)
oface<-read.delim("ORNL_FACE_anpp.txt")%>%
  select(site_code, project_name, treatment_year, calendar_year, treatment, plot_id, anpp)%>%
  mutate(community_type=0, block = 0)
tide<-read.delim("PIE_Tide_anpp.txt")%>%
  select(site_code, project_name, treatment_year, calendar_year, treatment, plot_id, anpp)%>%
  mutate(community_type=0, block = 0)
rio<-read.delim("RIO_interaction_anpp.txt")%>%
  select(site_code, project_name, community_type, treatment_year, calendar_year, treatment, plot_id, anpp) %>%
  mutate(block = 0)
cxn <- read.csv("SERC_CXN_anpp.csv")%>%
  select(site_code, project_name, treatment_year, calendar_year, treatment, plot_id, anpp)%>%
  mutate(community_type = 0, block = 0)
tmece <- read.csv("SERC_TMECE_anpp.csv")%>%
  select(site_code, project_name, treatment_year, calendar_year, treatment, plot_id, anpp, community_type) %>%
  mutate(block = 0)%>%
  filter(treatment!='C')
snfert<-read.delim("SEV_NFert_anpp.txt")%>%
  mutate(project_name='Nfert')%>%
  select(site_code, project_name, treatment_year, calendar_year, treatment, plot_id, anpp)%>%
  mutate(community_type=0, block = 0)
wenndex<-read.delim("SEV_WENNDEx_anpp.txt")%>%
  select(site_code, project_name, treatment_year, calendar_year, treatment, plot_id, anpp)%>%
  mutate(community_type=0, block = 0)
# nash <- read.csv("Sil_NASH_anpp.csv")%>%
#   select(site_code, project_name, treatment_year, calendar_year, treatment, plot_id, anpp, block)%>%
#   mutate(community_type=0)
ton <- read.csv("SIU_TON_anpp.csv") %>%
  mutate(community_type = 0)
uk<-read.delim("SKY_UK_anpp.txt")%>%
  select(site_code, project_name, treatment_year, calendar_year, treatment, plot_id, anpp, block)%>%
  mutate(community_type=0)
nitrogen <- read.csv("SR_Nitrogen_anpp.csv")%>%
  select(site_code, project_name, treatment_year, calendar_year, treatment, plot_id, anpp, community_type, block)
water <- read.csv("SR_Water_anpp.csv")%>%
  select(site_code, project_name, treatment_year, calendar_year, treatment, plot_id, anpp, community_type, block)
# shet <- read.csv("WAG_Shet_anpp.csv")%>%
#   select(site_code, project_name, treatment_year, calendar_year, treatment, plot_id, anpp, block)%>%
#   mutate(community_type=0)
vcrnutnet<- read.csv('VCR_NutNet_anpp.csv')%>%
  mutate(site_code='VCR', project_name='NutNet', community_type=0, block=0)%>%
  filter(biomass!='NA')%>%
  group_by(site_code, project_name, community_type, calendar_year, treatment_year, treatment, block, plot_id)%>%
  summarise(anpp=sum(biomass))%>%
  ungroup()%>%
  select(site_code, project_name, community_type, treatment_year, calendar_year, treatment, block, plot_id, anpp)
nitadd <- read.csv("YMN_NitAdd_anpp.csv")%>%
  select(site_code, project_name, treatment_year, calendar_year, treatment, plot_id, anpp)%>%
  mutate(community_type = 0, block = 0)

anpp <- rbind(atwe, bgp, biocon, bowman, btdrought, btnpkd, change, clonal, cxn, e001, e002, e2, e6, eel, 
              events, exp1, fireplots, gb, gfp, imagine, imgers, irg, kgfert, 
              lind, megarich, mnr, NDE, nfert, nit, nitadd, nitrogen, Nprecip, nsfc, 
              nutnet, oface, phace, pme, pplots, ramps, rhps, rio, rmapc, 
              snfert, snow, t7, tide, tmece, ton, uk, wapaclip, water, watering, wenndex, wet, vcrnutnet)

# write.csv(anpp, 'C:/Users/lapie/Dropbox (Smithsonian)/working groups/CoRRE/CoRRE_database/Data/CompiledData/ANPP2021.csv', row.names=F)


