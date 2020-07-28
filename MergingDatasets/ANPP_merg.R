setwd("~/Dropbox/CoRRE_database/Data/CleanedData/Sites/ANPP csv")

library(gtools)
library(reshape2)
library(tidyr)
library(dplyr)

# Read in sites
watering<-read.delim("ANG_watering_anpp.txt")%>%
  select(site_code, project_name, treatment_year, calendar_year, treatment, plot_id, anpp)%>%
  mutate(community_type=0)
clonal<-read.delim("ASGA_Clonal_anpp.txt")%>%
  select(site_code, project_name, treatment_year, calendar_year, treatment, plot_id, anpp)%>%
  mutate(community_type=0)
exp1<-read.delim("ASGA_Exp1_anpp.txt")%>%
  select(site_code, project_name, treatment_year, calendar_year, treatment, plot_id, anpp, community_type)
nit <- read.csv("AZI_NitPhos.csv") %>%
  select(site_code, project_name, treatment_year, calendar_year, treatment, plot_id, anpp, community_type)
lind<-read.delim("BAY_LIND_anpp.txt")%>%
  select(site_code, project_name, treatment_year, calendar_year, treatment, plot_id, anpp)%>%
  mutate(community_type=0)
events<-read.delim("Bt_EVENT2_anpp.txt")%>%
  select(site_code, project_name, treatment_year, calendar_year, treatment, plot_id, anpp)%>%
  mutate(community_type=0)
rmapc<-read.delim("CAU_RMAPC_anpp.txt")%>%
  select(site_code, project_name, community_type, treatment_year, calendar_year, treatment, plot_id, anpp)
biocon<-read.csv("CDR_BioCON_anpp.csv")%>%
  select(site_code, project_name, treatment_year, calendar_year, treatment, plot_id, anpp)%>%
  mutate(community_type=0)
e001<-read.csv("CDR_e001_anpp.csv")%>%
  select(site_code, project_name, community_type, treatment_year, calendar_year, treatment, plot_id, anpp)
e002<-read.delim("CDR_e002_anpp.txt")%>%
  select(site_code, project_name, community_type, treatment_year, calendar_year, treatment, plot_id, anpp)
megarich<-read.delim("CEH_Megarich_anpp.txt")%>%
  select(site_code, project_name, treatment_year, calendar_year, treatment, plot_id, anpp)%>%
  mutate(community_type=0)
imagine<-read.delim("CLE_imagine_anpp.txt")%>%
  select(site_code, project_name, treatment_year, calendar_year, treatment, plot_id, anpp)%>%
  mutate(community_type=0)
nsfc<-read.delim("DL_NSFC_anpp.txt")%>%
  select(site_code, project_name, treatment_year, calendar_year, treatment, plot_id, anpp)%>%
  mutate(community_type=0)
nsfc2<-read.csv("DL_NSFC_anpp20132016.csv")%>%
  select(site_code, project_name, treatment_year, calendar_year, treatment, plot_id, anpp)%>%
  mutate(community_type=0)
NDE <- read.csv("IMGERS_NDE_anpp.csv") %>%
  select(site_code, project_name, treatment_year, calendar_year, treatment, plot_id, anpp)%>%
  mutate(community_type=0)
imgers<-read.delim("IMGERS_Yu_anpp.txt")%>%
  select(site_code, project_name, treatment_year, calendar_year, treatment, plot_id, anpp)%>%
  mutate(community_type=0)
wapaclip<-read.delim("KAEFS_WaPaClip_anpp.txt")%>%
  select(site_code, project_name, treatment_year, calendar_year, treatment, plot_id, anpp)%>%
  mutate(community_type=0)
t7<-read.delim("KBS_T7_anpp.txt")%>%
  select(site_code, project_name, treatment_year, calendar_year, treatment, plot_id, anpp)%>%
  mutate(community_type=0)
kgfert<-read.delim("KLU_KGFert_anpp.txt")%>%
  select(site_code, project_name, treatment_year, calendar_year, treatment, plot_id, anpp)%>%
  mutate(community_type=0)
bgp<-read.csv("KNZ_BGP_anpp.csv")%>%
  select(site_code, project_name, treatment_year, calendar_year, treatment, plot_id, anpp)%>%
  mutate(community_type=0)
irg<-read.delim("KNZ_IRG_anpp.txt")%>%
  select(site_code, project_name, community_type, treatment_year, calendar_year, treatment, plot_id, anpp)
pplots<-read.csv("KNZ_PPLOTS_anpp.csv")%>%
  select(site_code, project_name, treatment_year, calendar_year, treatment, plot_id, anpp)%>%
  mutate(community_type=0)
ramps<-read.csv("KNZ_Ramps_anpp.csv")%>%
  select(site_code, project_name, treatment_year, calendar_year, treatment, plot_id, anpp)%>%
  mutate(community_type=0)
rhps<-read.csv("KNZ_RHPs_anpp.csv")%>%
  select(site_code, project_name, treatment_year, calendar_year, treatment, plot_id, anpp)%>%
  mutate(community_type=0)
gfp <- read.csv("KNZ_KNP_GFP_anpp.csv") %>%
  select(site_code, project_name, treatment_year, calendar_year, treatment, plot_id, anpp)
e2 <- read.csv("KUFS_E2_anpp.csv")%>%
  select(site_code, project_name, treatment_year, calendar_year, treatment, plot_id, anpp)
e6 <- read.csv("KUFS_E6_anpp.csv")%>%
  select(site_code, project_name, treatment_year, calendar_year, treatment, plot_id, anpp)
pme <- read.csv("LEFT_PME_anpp.csv")%>%
  select(site_code, project_name, treatment_year, calendar_year, treatment, plot_id, anpp)
fireplots<-read.csv("MAERC_fireplots_anpp.csv")%>%
  select(site_code, project_name, treatment_year, calendar_year, treatment, plot_id, anpp)%>%
  mutate(community_type=0)
mnr<-read.delim("MNR_watfer_anpp.txt")%>%
  select(site_code, project_name, treatment_year, calendar_year, treatment, plot_id, anpp)%>%
  mutate(community_type=0)
wet<-read.delim("NANT_wet_anpp.txt")%>%
  select(site_code, project_name, community_type, treatment_year, calendar_year, treatment, plot_id, anpp)
gb<-read.delim("NGBER_gb_anpp.txt")%>%
  select(site_code, project_name, treatment_year, calendar_year, treatment, plot_id, anpp)%>%
  mutate(community_type=0)
nfert<-read.delim("NWT_246NFert_anpp.txt")%>%
  select(site_code, project_name, treatment_year, calendar_year, treatment, plot_id, anpp)%>%
  mutate(community_type=0)
bowman<-read.delim("NWT_bowman_anpp.txt")%>%
  select(site_code, project_name, community_type, treatment_year, calendar_year, treatment, plot_id, anpp)
snow <- read.csv("NWT_snow_anpp.csv")%>%
  select(site_code, project_name, treatment_year, calendar_year, treatment, plot_id, anpp)
oface<-read.delim("ORNL_FACE_anpp.txt")%>%
  select(site_code, project_name, treatment_year, calendar_year, treatment, plot_id, anpp)%>%
  mutate(community_type=0)
tide<-read.delim("PIE_Tide_anpp.txt")%>%
  select(site_code, project_name, treatment_year, calendar_year, treatment, plot_id, anpp)%>%
  mutate(community_type=0)
rio<-read.delim("RIO_interaction_anpp.txt")%>%
  select(site_code, project_name, community_type, treatment_year, calendar_year, treatment, plot_id, anpp)
cxn <- read.csv("SERC_CXN_anpp.csv")%>%
  select(site_code, project_name, treatment_year, calendar_year, treatment, plot_id, anpp)
tmece <- read.csv("SERC_TMECE_anpp.csv")%>%
  select(site_code, project_name, treatment_year, calendar_year, treatment, plot_id, anpp)
snfert<-read.delim("SEV_NFert_anpp.txt")%>%
  select(site_code, project_name, treatment_year, calendar_year, treatment, plot_id, anpp)%>%
  mutate(community_type=0)
wenndex<-read.delim("SEV_WENNDEx_anpp.txt")%>%
  select(site_code, project_name, treatment_year, calendar_year, treatment, plot_id, anpp)%>%
  mutate(community_type=0)
uk<-read.delim("SKY_UK_anpp.txt")%>%
  select(site_code, project_name, treatment_year, calendar_year, treatment, plot_id, anpp)%>%
  mutate(community_type=0)
nitrogen <- read.csv("SR_Nitrogen_anpp.csv")%>%
  select(site_code, project_name, treatment_year, calendar_year, treatment, plot_id, anpp)
water <- read.csv("SR_Water_anpp.csv")%>%
  select(site_code, project_name, treatment_year, calendar_year, treatment, plot_id, anpp)
nitadd <- read.csv("YMN_NitAdd_anpp.csv")%>%
  select(site_code, project_name, treatment_year, calendar_year, treatment, plot_id, anpp)

anpp <- rbind(bgp, biocon, bowman, clonal, e001, e002, events, exp1, fireplots, gb, imagine, imgers, irg, kgfert, lind, megarich, mnr, nfert, nsfc, oface, pplots, ramps, rhps, rio, rmapc, snfert, t7, tide, uk, wapaclip, watering, wenndex, wet)

