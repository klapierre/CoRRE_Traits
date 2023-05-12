# Load packages:
library(ape)
library(PVR)
library(dplyr)

# Set directory:
my.wd<-"/Users/padulles/Documents/PD_MasarykU/sCoRRE/sCoRre/" #padu
setwd('C:\\Users\\kjkomatsu\\Dropbox (Smithsonian)\\working groups\\CoRRE\\CoRRE_database\\Data') #kim

# Load traits:
traits <- read.table("github/scorre/TRYAusBIEN_continuous_May2023.csv", row.names=NULL, sep=",", header=T)
traits <- traits[,c(6:18)] #select columns
traits$species_matched<-gsub(" ", "_", traits$species_matched) #change species names:

#Get mean values per species:
traits <- traits%>%group_by(species_matched)%>%summarise_at(vars("seed_dry_mass", "leaf_N", "X3115", "LDMC", "leaf_dry_mass", "X50", 
                                                                 "plant_height_vegetative", "SLA", "X3117", "leaf_area", "SRL", "X3114"), mean) %>% as.data.frame
rownames(traits)<-traits$species_matched #get rownames
traits$species_matched<-NULL

# Load the tree:
tree<-read.tree(paste(my.wd, "scorre.phylo.tree.S3.tre", sep=""))

# Get phylognetic eigenvectors:
#(note: we could potentially add columns with taxonomic ranks instead
#of eigenvectors; e.g., genus, family, order)
phylo_vrs <- PVR::PVRdecomp(tree)@Eigen$vectors
rownames(phylo_vrs)<-tree$tip.label #set rownames

# Put table together:
# NOTE: I have arbitrarily chose the first 10 phylogenetic eigenvectors
# I have tried with 20 or so, but the final NRMSE was higher (worse)
traits_phylo <- merge(traits, phylo_vrs[,c(1:10)], by ="row.names", all.x=T)
rownames(traits_phylo)<-traits_phylo$Row.names
traits_phylo$Row.names<-NULL

# Impute traits:
imputed_RF_scorre<- missForest::missForest(
  traits_phylo,
  maxiter=10, #maximum number of iterations
  ntree=100, #number of trees in the forest
  mtry = round(.333*ncol(traits_phylo)),
  variablewise=FALSE
)

# Print NRMSE (the lower the better):
imputed_RF_scorre$OOBerror 

# Save output:
write.table(imputed_RF_scorre$ximp, paste(my.wd, "imputed_RF_scorre.csv", sep=""))

#clean-up:
rm(list = ls())
