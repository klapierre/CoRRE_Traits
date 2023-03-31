################################################################################
##  bhpmf.R: Continuous trait imputation.
##
##  Authors: Franzisca Schrodt, Josep Padulles Cubino
################################################################################

# install development version from github

library(devtools)
#install_github("fisw10/BHPMF")
library(BHPMF)
library(plyr)
library(abind)
library(mice)

#####
#read original trait matrix for imputation:
setwd('C:\\Users\\kjkomatsu\\Dropbox (Smithsonian)\\working groups\\CoRRE\\CoRRE_database\\Data')

traits <- read.table("OriginalData\\Traits\\raw traits for gap filling\\TRYAusBIEN_continuous_March2023d.csv", row.names=NULL, sep=",", header=T)

#remove trait values with > 4 SD:
spp<-unique(traits$species_matched) #get vector with species names

out<-NULL
for(i in 1:length(spp)) { #loop for each species
  print(i/length(spp))
  sub<-traits[traits$species_matched %in% spp[i],]
  for(j in 7:ncol(traits)) { #loop for each trait (column)
    mean.sub<-mean(sub[,j], na.rm=T)
    sd.sub<-sd(sub[,j], na.rm=T)
    
    lim_up<-mean.sub+4*sd.sub
    lim_dn<-mean.sub-4*sd.sub
    
    sub[,j][sub[,j] > lim_up] <- NA
    sub[,j][sub[,j] < lim_dn] <- NA
  }
  out<-rbind(out, sub)
}

#create hierarchy file:
hierarchy.info <- subset(traits, select = c(ObservationID, species_matched, genus, family))
names(hierarchy.info)<-c("plant_id","species", "genus", "family")
hierarchy.info$plant_id<-1:nrow(hierarchy.info)

#I have noticed that the some genera are assigned to different families. Need to be unified:
hierarchy.info$family[hierarchy.info$genus=="Lancea"]<- "Mazaceae"
hierarchy.info$family[hierarchy.info$genus=="Toxicoscordion"]<- "Melanthiaceae"

#create trait info file:
trait.info <- as.data.frame(subset(traits, select = -c(family, genus, species_matched, ObservationID,
                                                       DatabaseID, DatasetID)))

#check if both datasets are equal
nrow(hierarchy.info) == nrow(trait.info)

#z % log transform
back_trans_pars <- list()
rm_col <- c()
for(i in 1:ncol(trait.info)){
  x <- trait.info[,i] # goes through the columns
  min_x <- min(x,na.rm = T) # takes the min of each column
  if(min_x < 0.00000000001){
    x <- x - min_x + 1 # make this optional if min x is neg
  }
  logx <- log10(x)
  mlogx <- mean(logx, na.rm = T)
  slogx <- sd(logx, na.rm = T)
  x <- (logx - mlogx)/slogx # Z transformation
  back_trans_pars[[i]] <- list(min_x = min_x,
                               mlogx = mlogx,
                               slogx = slogx)
  trait.info[,i] <- x
}
#write.table(back_trans_pars, "AllTraits/back_trans_pars.csv")
write.table(back_trans_pars, "CleanedData\\Traits\\gap filled continuous traits\\20230331\\back_trans_pars.csv")

#Split-out code here and load data here:

#set-directory
tmp.dir<-dirname("CleanedData\\Traits\\gap filled continuous traits\\20230331\\tmp")

#set parameters
smpl<-911:1000
fold<-c(rep(10:20, 8), 10, 11)

#set number of iterations:
repe<-90 #should be 90

#gap-filling:
for(i in 1:repe) { #loop for each trait (column)
  set.seed(123)
  GapFilling(as.matrix(trait.info), hierarchy.info,
             num.samples=smpl[i], num.folds.tuning=fold[i], burn=187,
             mean.gap.filled.output.path = paste0(tmp.dir,"/mean_gap_filled_",i,".txt"),
             std.gap.filled.output.path= paste0(tmp.dir,"/std_gap_filled_",i,".txt"), 
             tmp.dir=tmp.dir, verbose=F)
}

####
# Load imputed traits and clean-up table:

mean.trait<-list()
for(i in 1:repe) { #loop for each trait (column)
  print(i)
  trt<-read.table(paste0("CleanedData\\Traits\\gap filled continuous traits\\20230331\\mean_gap_filled_",i,".txt"), row.names=NULL, header=T)
  std<-read.table(paste0("CleanedData\\Traits\\gap filled continuous traits\\20230331\\std_gap_filled_",i,".txt"), row.names=NULL, header=T)

  #Return to NA those values with SD > 1:
  for(j in 1:ncol(trt)) {
    trt[,j][std[,j]>1]<-NA
  }

  #Return to NA values > 1.5*max observed trait:
  for(j in 1:ncol(trt)) {
    maxt<-max(trait.info[,j], na.rm=T)
    trt[,j][trt[,j]>(maxt*1.5)]<-NA
  }
  
  mean.trait[[i]]<-trt
}

#get mean across them all:
mean.trait <- abind(mean.trait, along=3)
mean.trait <- apply(mean.trait, c(1,2), mean, na.rm=T)
mean.trait[is.nan(mean.trait)]<-NA

#replace missing values in the original table:
for(j in 1:ncol(trait.info)) {
  trait.info[,j]<-ifelse(is.na(trait.info[,j]),mean.trait[,j],trait.info[,j])
}

#return to original values:
back<-read.table("CleanedData\\Traits\\gap filled continuous traits\\20230331\\back_trans_pars.csv")

o<-1 #to select the appropriate columns:
for(i in 1:ncol(trait.info)){
  
  #recover values:
  min_x<-back[1,o]
  mlogx<-back[1,o+1]
  slogx<-back[1,o+2]
  
  #back transform:
  x <- trait.info[,i] # goes through the columns
  logx<- (x*slogx) + mlogx
  b <- 10^logx
  
  #for negative values
  if(min_x < 0.00000000001){
    b <- b + min_x - 1 # make this optional if min x is neg
  }
  
  trait.info[,i] <- b
  o<-o+3
}

#save output:
write.table(trait.info, "CleanedData\\Traits\\gap filled continuous traits\\20230331\\imputed_traits.csv")

#Impute missing values with "mice":
trait.info.mice<-complete(mice(trait.info, method="cart"))

#save output:
write.table(trait.info.mice, "CleanedData\\Traits\\gap filled continuous traits\\20230331\\imputed_traits_mice.csv")

#clean-up:
rm(list = ls())
