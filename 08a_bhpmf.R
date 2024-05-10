################################################################################
##  08a_bhpmf.R: Continuous trait imputation: training and validation.
##
##  Authors: Kimberly Komatsu, Franzisca Schrodt, Josep Padulles Cubino
################################################################################

# install development version from github

library(tidyverse)
library(scales)
library(ggpubr)
library(devtools)
# install_github("fisw10/BHPMF")
library(BHPMF)
library(plyr)
library(abind)
library(mice)

##### read original trait matrix for imputation #####
setwd('C:\\Users\\kjkomatsu\\Dropbox (Smithsonian)\\working groups\\CoRRE\\CoRRE_database\\Data')

traits <- read.table("OriginalData\\Traits\\raw traits for gap filling\\TRYAusBIEN_continuous_Apr2024.csv", row.names=NULL, sep=",", header=T) %>% 
  select(-Reference) %>% 
  pivot_longer(cols=seed_dry_mass:SRL, names_to='trait', values_to='values') %>% 
  arrange(family, genus, species_matched, trait) %>%
  filter(!is.na(values))
traits$training=rep_len(1:3, nrow(traits))

#some genera are assigned to different families. Need to be unified:
traits$family[traits$genus=="Lancea"] <- "Mazaceae"
traits$family[traits$genus=="Toxicoscordion"] <- "Melanthiaceae"
traits$family[traits$genus=="Heliotropium"] <- "Boraginaceae"
traits$family[traits$genus=="Phacelia"] <- "Boraginaceae"
traits$family[traits$genus=="Pholistoma"] <- "Boraginaceae"

traits12 <- filter(traits, training %in% c(1,2)) %>%  select(-training) %>% pivot_wider(names_from=trait, values_from=values) %>% as.data.frame()
traits13 <- filter(traits, training %in% c(1,3)) %>% select(-training) %>% pivot_wider(names_from=trait, values_from=values) %>% as.data.frame()
traits23 <- filter(traits, training %in% c(2,3)) %>% select(-training) %>% pivot_wider(names_from=trait, values_from=values) %>% as.data.frame()

sum(is.na(traits12$seed_dry_mass), is.na(traits12$X3117), is.na(traits12$LDMC), is.na(traits12$leaf_area), is.na(traits12$leaf_N), is.na(traits12$SLA), is.na(traits12$leaf_dry_mass), is.na(traits12$plant_height_vegetative), is.na(traits12$SRL), is.na(traits12$X3114), is.na(traits12$X3115), is.na(traits12$X3109), is.na(traits12$X614))
# 1587707 missing of 1776437 (89.4% missing)

sum(is.na(traits13$seed_dry_mass), is.na(traits13$X3117), is.na(traits13$LDMC), is.na(traits13$leaf_area), is.na(traits13$leaf_N), is.na(traits13$SLA), is.na(traits13$leaf_dry_mass), is.na(traits13$plant_height_vegetative), is.na(traits13$SRL), is.na(traits13$X3114), is.na(traits13$X3115), is.na(traits13$X3109), is.na(traits13$X614))
# 1587928 missing of 1776658 (89.4% missing)

sum(is.na(traits23$seed_dry_mass), is.na(traits23$X3117), is.na(traits23$LDMC), is.na(traits23$leaf_area), is.na(traits23$leaf_N), is.na(traits23$SLA), is.na(traits23$leaf_dry_mass), is.na(traits23$plant_height_vegetative), is.na(traits23$SRL), is.na(traits23$X3114), is.na(traits23$X3115), is.na(traits23$X3109), is.na(traits23$X614))
# 1586355 missing of 1775085 (89.4% missing)


##### training with sets 1 and 2; validation of set 3 #####

#remove trait values with > 4 SD:
spp <- unique(traits12$species_matched) #get vector with species names

out<-NULL
for(i in 1:length(spp)) { #loop for each species
  print(i/length(spp))
  sub <- traits12[traits12$species_matched %in% spp[i],]
  for(j in 7:ncol(traits12)) { #loop for each trait (column)
    mean.sub <- mean(sub[,j], na.rm=T)
    sd.sub <- sd(sub[,j], na.rm=T)

    lim_up <- mean.sub + 4*sd.sub
    lim_dn <- mean.sub - 4*sd.sub

    sub[,j][sub[,j] > lim_up] <- NA
    sub[,j][sub[,j] < lim_dn] <- NA
  }
  out <- rbind(out, sub)
}

#create hierarchy file:
hierarchy.info <- subset(traits12, select = c(ObservationID, species_matched, genus, family))
names(hierarchy.info) <- c("plant_id","species", "genus", "family")
hierarchy.info$plant_id <- 1:nrow(hierarchy.info)


#create trait info file:
trait.info <- as.data.frame(subset(traits12, select = -c(family, genus, species_matched, ObservationID,
                                                         DatabaseID, DatasetID)))
#check if both datasets are equal
nrow(hierarchy.info) == nrow(trait.info)

# z % log transform
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

write.table(back_trans_pars, "CleanedData\\Traits\\gap filled continuous traits\\20240510_trainingValidation\\20240510_trainingValidation_12\\back_trans_pars.csv")


# gap-filling
#set-directory
tmp.dir <- dirname("CleanedData\\Traits\\gap filled continuous traits\\20240510_trainingValidation\\20240510_trainingValidation_12\\tmp")

#set parameters
smpl <- 900:1000
fold <- c(rep(10:20, 8), 10, 11)

#set number of iterations:
repe <- 90 #should be 90

for(i in 1:repe) { #loop for each trait (column)
  set.seed(123)
  GapFilling(as.matrix(trait.info), hierarchy.info,
             num.samples = smpl[i], num.folds.tuning=fold[i], burn=187,
             mean.gap.filled.output.path = paste0(tmp.dir,"/mean_gap_filled_",i,".txt"),
             std.gap.filled.output.path = paste0(tmp.dir,"/std_gap_filled_",i,".txt"),
             tmp.dir = tmp.dir, verbose=F)
}


# load imputed traits and clean-up table
mean.trait<-list()
for(i in 1:repe) { #loop for each trait (column)
  print(i)
  trt <- read.table(paste0("CleanedData\\Traits\\gap filled continuous traits\\20240510_trainingValidation\\20240510_trainingValidation_12\\mean_gap_filled_",i,".txt"), row.names=NULL, header=T)
  std <- read.table(paste0("CleanedData\\Traits\\gap filled continuous traits\\20240510_trainingValidation\\20240510_trainingValidation_12\\std_gap_filled_",i,".txt"), row.names=NULL, header=T)

  #Return to NA those values with SD > 1:
  for(j in 1:ncol(trt)) {
    trt[,j][std[,j]>1] <- NA
  }

  #Return to NA values > 1.5*max observed trait:
  for(j in 1:ncol(trt)) {
    maxt <- max(trait.info[,j], na.rm=T)
    trt[,j][trt[,j] > (maxt*1.5)] <- NA
  }
  
  mean.trait[[i]] <- trt
}

#get mean across them all:
mean.trait <- abind(mean.trait, along=3)
mean.trait <- apply(mean.trait, c(1,2), mean, na.rm=T)
mean.trait[is.nan(mean.trait)] <- NA

#data for back transforming output
back <- read.table("CleanedData\\Traits\\gap filled continuous traits\\20240510_trainingValidation\\20240510_trainingValidation_12\\back_trans_pars.csv")


#don't replace original values:
trait.info.noreplacement <- as.data.frame(mean.trait)

o <- 1 #to select the appropriate columns:
for(i in 1:ncol(trait.info.noreplacement)){
  
  #recover values:
  min_x <- back[1,o]
  mlogx <- back[1,o+1]
  slogx <- back[1,o+2]
  
  #back transform:
  x <- trait.info.noreplacement[,i] # goes through the columns
  logx <- (x*slogx) + mlogx
  b <- 10^logx
  
  #for negative values
  if(min_x < 0.00000000001){
    b <- b + min_x - 1 # make this optional if min x is neg
  }
  
  trait.info.noreplacement[,i] <- b
  o <- o+3
}


#save output
write.csv(trait.info.noreplacement, "CleanedData\\Traits\\gap filled continuous traits\\20240510_trainingValidation\\imputed_traits_12.csv", row.names=F)

# Impute missing values with "mice"
trait.info.mice <- complete(mice(trait.info.noreplacement, method="cart"))

write.csv(trait.info.mice, "CleanedData\\Traits\\gap filled continuous traits\\20240510_trainingValidation\\imputed_traits_mice_12.csv", row.names=F)

#clean-up:
# rm(list = ls())


##### training with sets 1 and 3; validation of set 2 #####

# #remove trait values with > 4 SD:
# spp <- unique(traits13$species_matched) #get vector with species names
# 
# out<-NULL
# for(i in 1:length(spp)) { #loop for each species
#   print(i/length(spp))
#   sub <- traits[traits13$species_matched %in% spp[i],]
#   for(j in 7:ncol(traits13)) { #loop for each trait (column)
#     mean.sub <- mean(sub[,j], na.rm=T)
#     sd.sub <- sd(sub[,j], na.rm=T)
#     
#     lim_up <- mean.sub + 4*sd.sub
#     lim_dn <- mean.sub - 4*sd.sub
#     
#     sub[,j][sub[,j] > lim_up] <- NA
#     sub[,j][sub[,j] < lim_dn] <- NA
#   }
#   out <- rbind(out, sub)
# }

#create hierarchy file:
hierarchy.info <- subset(traits13, select = c(ObservationID, species_matched, genus, family))
names(hierarchy.info) <- c("plant_id","species", "genus", "family")
hierarchy.info$plant_id <- 1:nrow(hierarchy.info)

#some genera are assigned to different families. Need to be unified:
hierarchy.info$family[hierarchy.info$genus=="Lancea"] <- "Mazaceae"
hierarchy.info$family[hierarchy.info$genus=="Toxicoscordion"] <- "Melanthiaceae"
hierarchy.info$family[hierarchy.info$genus=="Heliotropium"] <- "Boraginaceae"
hierarchy.info$family[hierarchy.info$genus=="Phacelia"] <- "Boraginaceae"
hierarchy.info$family[hierarchy.info$genus=="Pholistoma"] <- "Boraginaceae"

#create trait info file:
trait.info <- as.data.frame(subset(traits13, select = -c(family, genus, species_matched, ObservationID,
                                                         DatabaseID, DatasetID)))
#check if both datasets are equal
nrow(hierarchy.info) == nrow(trait.info)

# z % log transform
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

write.table(back_trans_pars, "CleanedData\\Traits\\gap filled continuous traits\\20240510_trainingValidation\\20240510_trainingValidation_13\\back_trans_pars.csv")


# gap-filling
#set-directory
tmp.dir <- dirname("CleanedData\\Traits\\gap filled continuous traits\\20240510_trainingValidation\\20240510_trainingValidation_13\\tmp")

#set parameters
smpl <- 900:1000
fold <- c(rep(10:20, 8), 10, 11)

#set number of iterations:
repe <- 90 #should be 90

for(i in 1:repe) { #loop for each trait (column)
  set.seed(123)
  GapFilling(as.matrix(trait.info), hierarchy.info,
             num.samples = smpl[i], num.folds.tuning=fold[i], burn=187,
             mean.gap.filled.output.path = paste0(tmp.dir,"/mean_gap_filled_",i,".txt"),
             std.gap.filled.output.path = paste0(tmp.dir,"/std_gap_filled_",i,".txt"),
             tmp.dir = tmp.dir, verbose=F)
}


# load imputed traits and clean-up table
mean.trait<-list()
for(i in 1:repe) { #loop for each trait (column)
  print(i)
  trt <- read.table(paste0("CleanedData\\Traits\\gap filled continuous traits\\20240510_trainingValidation\\20240510_trainingValidation_13\\mean_gap_filled_",i,".txt"), row.names=NULL, header=T)
  std <- read.table(paste0("CleanedData\\Traits\\gap filled continuous traits\\20240510_trainingValidation\\20240510_trainingValidation_13\\std_gap_filled_",i,".txt"), row.names=NULL, header=T)
  
  #Return to NA those values with SD > 1:
  for(j in 1:ncol(trt)) {
    trt[,j][std[,j]>1] <- NA
  }
  
  #Return to NA values > 1.5*max observed trait:
  for(j in 1:ncol(trt)) {
    maxt <- max(trait.info[,j], na.rm=T)
    trt[,j][trt[,j] > (maxt*1.5)] <- NA
  }
  
  mean.trait[[i]] <- trt
}

#get mean across them all:
mean.trait <- abind(mean.trait, along=3)
mean.trait <- apply(mean.trait, c(1,2), mean, na.rm=T)
mean.trait[is.nan(mean.trait)] <- NA

#data for back transforming output
back <- read.table("CleanedData\\Traits\\gap filled continuous traits\\20240510_trainingValidation\\20240510_trainingValidation_13\\back_trans_pars.csv")


#don't replace original values:
trait.info.noreplacement <- as.data.frame(mean.trait)

o <- 1 #to select the appropriate columns:
for(i in 1:ncol(trait.info.noreplacement)){
  
  #recover values:
  min_x <- back[1,o]
  mlogx <- back[1,o+1]
  slogx <- back[1,o+2]
  
  #back transform:
  x <- trait.info.noreplacement[,i] # goes through the columns
  logx <- (x*slogx) + mlogx
  b <- 10^logx
  
  #for negative values
  if(min_x < 0.00000000001){
    b <- b + min_x - 1 # make this optional if min x is neg
  }
  
  trait.info.noreplacement[,i] <- b
  o <- o+3
}


#save output
write.csv(trait.info.noreplacement, "CleanedData\\Traits\\gap filled continuous traits\\20240510_trainingValidation\\imputed_traits_13.csv", row.names=F)

# Impute missing values with "mice"
trait.info.mice <- complete(mice(trait.info.noreplacement, method="cart"))

write.csv(trait.info.mice, "CleanedData\\Traits\\gap filled continuous traits\\20240510_trainingValidation\\imputed_traits_mice_13.csv", row.names=F)

# clean-up:
# rm(list = ls())




##### training with sets 2 and 3; validation of set 1 #####

# #remove trait values with > 4 SD:
# spp <- unique(traits23$species_matched) #get vector with species names
# 
# out<-NULL
# for(i in 1:length(spp)) { #loop for each species
#   print(i/length(spp))
#   sub <- traits[traits23$species_matched %in% spp[i],]
#   for(j in 7:ncol(traits23)) { #loop for each trait (column)
#     mean.sub <- mean(sub[,j], na.rm=T)
#     sd.sub <- sd(sub[,j], na.rm=T)
#     
#     lim_up <- mean.sub + 4*sd.sub
#     lim_dn <- mean.sub - 4*sd.sub
#     
#     sub[,j][sub[,j] > lim_up] <- NA
#     sub[,j][sub[,j] < lim_dn] <- NA
#   }
#   out <- rbind(out, sub)
# }

#create hierarchy file:
hierarchy.info <- subset(traits23, select = c(ObservationID, species_matched, genus, family))
names(hierarchy.info) <- c("plant_id","species", "genus", "family")
hierarchy.info$plant_id <- 1:nrow(hierarchy.info)

#some genera are assigned to different families. Need to be unified:
hierarchy.info$family[hierarchy.info$genus=="Lancea"] <- "Mazaceae"
hierarchy.info$family[hierarchy.info$genus=="Toxicoscordion"] <- "Melanthiaceae"
hierarchy.info$family[hierarchy.info$genus=="Heliotropium"] <- "Boraginaceae"
hierarchy.info$family[hierarchy.info$genus=="Phacelia"] <- "Boraginaceae"
hierarchy.info$family[hierarchy.info$genus=="Pholistoma"] <- "Boraginaceae"

#create trait info file:
trait.info <- as.data.frame(subset(traits23, select = -c(family, genus, species_matched, ObservationID,
                                                         DatabaseID, DatasetID)))
#check if both datasets are equal
nrow(hierarchy.info) == nrow(trait.info)

# z % log transform
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

write.table(back_trans_pars, "CleanedData\\Traits\\gap filled continuous traits\\20240510_trainingValidation\\20240510_trainingValidation_23\\back_trans_pars.csv")


# gap-filling
#set-directory
tmp.dir <- dirname("CleanedData\\Traits\\gap filled continuous traits\\20240510_trainingValidation\\20240510_trainingValidation_23\\tmp")

#set parameters
smpl <- 900:1000
fold <- c(rep(10:20, 8), 10, 11)

#set number of iterations:
repe <- 90 #should be 90

for(i in 1:repe) { #loop for each trait (column)
  set.seed(123)
  GapFilling(as.matrix(trait.info), hierarchy.info,
             num.samples = smpl[i], num.folds.tuning=fold[i], burn=187,
             mean.gap.filled.output.path = paste0(tmp.dir,"/mean_gap_filled_",i,".txt"),
             std.gap.filled.output.path = paste0(tmp.dir,"/std_gap_filled_",i,".txt"),
             tmp.dir = tmp.dir, verbose=F)
}

# load imputed traits and clean-up table
mean.trait<-list()
for(i in 1:repe) { #loop for each trait (column)
  print(i)
  trt <- read.table(paste0("CleanedData\\Traits\\gap filled continuous traits\\20240510_trainingValidation\\20240510_trainingValidation_23\\mean_gap_filled_",i,".txt"), row.names=NULL, header=T)
  std <- read.table(paste0("CleanedData\\Traits\\gap filled continuous traits\\20240510_trainingValidation\\20240510_trainingValidation_23\\std_gap_filled_",i,".txt"), row.names=NULL, header=T)
  
  #Return to NA those values with SD > 1:
  for(j in 1:ncol(trt)) {
    trt[,j][std[,j]>1] <- NA
  }
  
  #Return to NA values > 1.5*max observed trait:
  for(j in 1:ncol(trt)) {
    maxt <- max(trait.info[,j], na.rm=T)
    trt[,j][trt[,j] > (maxt*1.5)] <- NA
  }
  
  mean.trait[[i]] <- trt
}

#get mean across them all:
mean.trait <- abind(mean.trait, along=3)
mean.trait <- apply(mean.trait, c(1,2), mean, na.rm=T)
mean.trait[is.nan(mean.trait)] <- NA

#data for back transforming output
back <- read.table("CleanedData\\Traits\\gap filled continuous traits\\20240510_trainingValidation\\20240510_trainingValidation_23\\back_trans_pars.csv")


#don't replace original values:
trait.info.noreplacement <- as.data.frame(mean.trait)

o <- 1 #to select the appropriate columns:
for(i in 1:ncol(trait.info.noreplacement)){
  
  #recover values:
  min_x <- back[1,o]
  mlogx <- back[1,o+1]
  slogx <- back[1,o+2]
  
  #back transform:
  x <- trait.info.noreplacement[,i] # goes through the columns
  logx <- (x*slogx) + mlogx
  b <- 10^logx
  
  #for negative values
  if(min_x < 0.00000000001){
    b <- b + min_x - 1 # make this optional if min x is neg
  }
  
  trait.info.noreplacement[,i] <- b
  o <- o+3
}


#save output
write.csv(trait.info.noreplacement, "CleanedData\\Traits\\gap filled continuous traits\\20240510_trainingValidation\\imputed_traits_23.csv", row.names=F)

# Impute missing values with "mice"
trait.info.mice <- complete(mice(trait.info.noreplacement, method="cart"))

write.csv(trait.info.mice, "CleanedData\\Traits\\gap filled continuous traits\\20240510_trainingValidation\\imputed_traits_mice_23.csv", row.names=F)

#clean-up:
rm(list = ls())



##### Validating output #####

theme_set(theme_bw())
theme_update(axis.title.x=element_text(size=30, vjust=-0.35, margin=margin(t=15)), axis.text.x=element_text(size=26),
             axis.title.y=element_text(size=30, angle=90, vjust=0.5, margin=margin(r=15)), axis.text.y=element_text(size=26),
             plot.title = element_text(size=54, vjust=2),
             panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
             legend.title=element_blank(), legend.text=element_text(size=30))


#validation set 1 and 2 predicting 3
traits12long <- traits12 %>% 
  pivot_longer(cols=seed_dry_mass:X614, names_to='trait', values_to='original_value')

trial12 <- read.csv('CleanedData\\Traits\\gap filled continuous traits\\20240510_trainingValidation\\imputed_traits_mice_12.csv') %>%
  bind_cols(traits12[,c('DatabaseID', 'DatasetID', 'ObservationID', 'family', 'genus', 'species_matched')]) %>% 
  mutate(validate=3) %>% 
  pivot_longer(cols=seed_dry_mass:X614, names_to='trait', values_to='imputed_value') %>% 
  left_join(traits12long) %>% 
  na.omit()


#validation set 1 and 3 predicting 2
traits13long <- traits13 %>% 
  pivot_longer(cols=seed_dry_mass:X614, names_to='trait', values_to='original_value')

trial13 <- read.csv('CleanedData\\Traits\\gap filled continuous traits\\20240510_trainingValidation\\imputed_traits_mice_13.csv') %>%
  bind_cols(traits13[,c('DatabaseID', 'DatasetID', 'ObservationID', 'family', 'genus', 'species_matched')]) %>% 
  mutate(validate=2) %>% 
  pivot_longer(cols=seed_dry_mass:X614, names_to='trait', values_to='imputed_value') %>% 
  left_join(traits13long) %>% 
  na.omit()


#validation set 2 and 3 predicting 1
traits23long <- traits23 %>% 
  pivot_longer(cols=seed_dry_mass:X614, names_to='trait', values_to='original_value')

trial23 <- read.csv('CleanedData\\Traits\\gap filled continuous traits\\20240510_trainingValidation\\imputed_traits_mice_23.csv') %>%
  bind_cols(traits23[,c('DatabaseID', 'DatasetID', 'ObservationID', 'family', 'genus', 'species_matched')]) %>% 
  mutate(validate=1) %>% 
  pivot_longer(cols=seed_dry_mass:X614, names_to='trait', values_to='imputed_value') %>% 
  left_join(traits23long) %>% 
  na.omit()


#bind together
trial <- rbind(trial12, trial13, trial23) %>% 
  mutate(trait2=ifelse(trait=='leaf_area', 'Leaf Area (leaf, +petiole)',
                ifelse(trait=='SLA', 'Specific Leaf Area (+petiole)', 
                ifelse(trait=='SRL', 'Specific Root Length (all root)',
                ifelse(trait=='leaf_N', 'Leaf N Content',
                ifelse(trait=='plant_height_vegetative', 'Plant Vegetative Height',
                ifelse(trait=='seed_dry_mass', 'Seed Dry Mass',
                ifelse(trait=='leaf_dry_mass', 'Leaf Dry Mass',
                ifelse(trait=='LDMC', 'Leaf Dry Matter Content',
                trait)))))))))

trial$trait2 = factor(trial$trait2, levels=c('Leaf Area (leaf, +petiole)', 'Leaf Dry Mass', 'Leaf Dry Matter Content', 'Specific Leaf Area (+petiole)', 'Leaf N Content', 'Plant Vegetative Height', 'Specific Root Length (all root)', 'Seed Dry Mass'))

ggplot(data=na.omit(trial), aes(x=original_value, y=imputed_value)) +
  geom_abline(slope=1, linewidth=2, color='black') +
  geom_point(aes(color=as.factor(validate))) +
  geom_smooth(linewidth=2, se=T, aes(color=as.factor(validate)), method='lm') +
  scale_color_manual(values=c('#ffa726', '#e65100', '#795548')) +
  facet_wrap(~trait2, scales='free', ncol=3, labeller=label_wrap_gen(width=25)) +
  xlab('Original Value') + ylab('Imputed Value') +
  scale_y_continuous(trans='log10', labels = label_comma()) +
  scale_x_continuous(trans='log10', labels = label_comma()) +
  theme(strip.text.x = element_text(size = 28),
        axis.title.x=element_text(size=32, vjust=-0.35, margin=margin(t=15)), axis.text.x=element_text(size=32),
        axis.title.y=element_text(size=32, angle=90, vjust=0.5, margin=margin(r=15)), axis.text.y=element_text(size=32)) 

# ggsave('C:\\Users\\kjkomatsu\\Dropbox (Smithsonian)\\working groups\\CoRRE\\sDiv\\sDiv_sCoRRE_shared\\DataPaper\\2023_sCoRRE_traits\\figures\\Fig 6_original v imputed_20240509.png', width=20, height=16, units='in', dpi=300, bg='white')


#### NMRSE ####

NMRSEtrial <- trial %>% 
  mutate(sq_diff=(imputed_value-original_value)^2) %>% 
  group_by(trait, validate) %>% 
  dplyr::summarise(sum=sum(sq_diff), n=length(trait), min=min(original_value), max=max(original_value), mean=mean(original_value)) %>% 
  ungroup() %>% 
  mutate(NRMSE=sqrt(sum/n)/mean)


#### Correlation statistics for each trait ####

#leaf area
with(subset(trial, trait=='leaf_area'), hist(log10(original_value)))
with(subset(trial, trait=='leaf_area'), hist(log10(imputed_value)))

with(subset(trial, trait=='leaf_area' & validate==1), cor.test(original_value, imputed_value,method = "pearson", use = "complete.obs")) 
# r 0.9612228    
with(subset(trial, trait=='leaf_area' & validate==2), cor.test(original_value, imputed_value,method = "pearson", use = "complete.obs")) 
# r 0.952561    
with(subset(trial, trait=='leaf_area' & validate==3), cor.test(original_value, imputed_value,method = "pearson", use = "complete.obs")) 
# r 0.9717914    

summary(leaf_area <- lm(log10(imputed_value)~log10(original_value), data=subset(trial, trait=='leaf_area'&!is.na(original_value)&validate==1)))
# log10(original_value) 0.938390   0.002095  447.92   <2e-16 ***
summary(leaf_area <- lm(log10(imputed_value)~log10(original_value), data=subset(trial, trait=='leaf_area'&!is.na(original_value)&validate==2)))
# log10(original_value) 0.939899   0.002192  428.84   <2e-16 ***
summary(leaf_area <- lm(log10(imputed_value)~log10(original_value), data=subset(trial, trait=='leaf_area'&!is.na(original_value)&validate==3)))
# log10(original_value) 0.943708   0.002046  461.25   <2e-16 ***


#leaf dry mass
with(subset(trial, trait=='leaf_dry_mass'), hist(log10(original_value)))
with(subset(trial, trait=='leaf_dry_mass'), hist(log10(imputed_value)))

with(subset(trial, trait=='leaf_dry_mass' & validate==1), cor.test(original_value, imputed_value,method = "pearson", use = "complete.obs"))
# r 0.9286539     
with(subset(trial, trait=='leaf_dry_mass' & validate==2), cor.test(original_value, imputed_value,method = "pearson", use = "complete.obs"))
# r 0.9183957     
with(subset(trial, trait=='leaf_dry_mass' & validate==3), cor.test(original_value, imputed_value,method = "pearson", use = "complete.obs"))
# r 0.9385037     

summary(leaf_dry_mass <- lm(log10(imputed_value)~log10(original_value), data=subset(trial, trait=='leaf_dry_mass'&!is.na(original_value)&validate==1)))
# log10(original_value) 0.952097   0.001587  600.10   <2e-16 ***
summary(leaf_dry_mass <- lm(log10(imputed_value)~log10(original_value), data=subset(trial, trait=='leaf_dry_mass'&!is.na(original_value)&validate==2)))
# log10(original_value) 0.953243   0.001529  623.46   <2e-16 ***
summary(leaf_dry_mass <- lm(log10(imputed_value)~log10(original_value), data=subset(trial, trait=='leaf_dry_mass'&!is.na(original_value)&validate==3)))
# log10(original_value) 0.952106   0.001538  618.94   <2e-16 ***


#LDMC
with(subset(trial, trait=='LDMC'), hist(log10(original_value)))
with(subset(trial, trait=='LDMC'), hist(log10(imputed_value)))

with(subset(trial, trait=='LDMC' & validate==1), cor.test(original_value, imputed_value,method = "pearson", use = "complete.obs"))
# r 0.9587842   
with(subset(trial, trait=='LDMC' & validate==2), cor.test(original_value, imputed_value,method = "pearson", use = "complete.obs"))
# r 0.9718856    
with(subset(trial, trait=='LDMC' & validate==3), cor.test(original_value, imputed_value,method = "pearson", use = "complete.obs"))
# r 0.9702715    

summary(LDMC <- lm(log10(imputed_value)~log10(original_value), data=subset(trial, trait=='LDMC'&!is.na(original_value)&validate==1)))
# og10(original_value)  0.9317824  0.0014584  638.89   <2e-16 ***
summary(LDMC <- lm(log10(imputed_value)~log10(original_value), data=subset(trial, trait=='LDMC'&!is.na(original_value)&validate==2)))
# log10(original_value)  0.9383542  0.0013525  693.77   <2e-16 ***
summary(LDMC <- lm(log10(imputed_value)~log10(original_value), data=subset(trial, trait=='LDMC'&!is.na(original_value)&validate==3)))
# log10(original_value)  0.9339309  0.0014242   655.8   <2e-16 ***


#SLA
with(subset(trial, trait=='SLA'), hist(log10(original_value)))
with(subset(trial, trait=='SLA'), hist(log10(imputed_value)))

with(subset(trial, trait=='SLA' & validate==1), cor.test(original_value, imputed_value,method = "pearson", use = "complete.obs"))
# r 0.9561315     
with(subset(trial, trait=='SLA' & validate==2), cor.test(original_value, imputed_value,method = "pearson", use = "complete.obs"))
# r 0.9530478      
with(subset(trial, trait=='SLA' & validate==3), cor.test(original_value, imputed_value,method = "pearson", use = "complete.obs"))
# r 0.9583306     

summary(SLA <- lm(log10(imputed_value)~log10(original_value), data=subset(trial, trait=='SLA'&!is.na(original_value)&validate==1)))
# log10(original_value) 0.909172   0.002199  413.43   <2e-16 ***
summary(SLA <- lm(log10(imputed_value)~log10(original_value), data=subset(trial, trait=='SLA'&!is.na(original_value)&validate==2)))
# log10(original_value) 0.911848   0.002265  402.50   <2e-16 ***
summary(SLA <- lm(log10(imputed_value)~log10(original_value), data=subset(trial, trait=='SLA'&!is.na(original_value)&validate==3)))
# log10(original_value) 0.916130   0.002117  432.81   <2e-16 ***


#leaf N
with(subset(trial, trait=='leaf_N'), hist(log10(original_value)))
with(subset(trial, trait=='leaf_N'), hist(log10(imputed_value)))

with(subset(trial, trait=='leaf_N' & validate==1), cor.test(original_value, imputed_value,method = "pearson", use = "complete.obs"))
# r 0.9703424        
with(subset(trial, trait=='leaf_N' & validate==2), cor.test(original_value, imputed_value,method = "pearson", use = "complete.obs"))
# r 0.9685136      
with(subset(trial, trait=='leaf_N' & validate==3), cor.test(original_value, imputed_value,method = "pearson", use = "complete.obs"))
# r 0.9364026       

summary(leaf_N <- lm(log10(imputed_value)~log10(original_value), data=subset(trial, trait=='leaf_N'&!is.na(original_value)&validate==1)))
# log10(original_value) 0.948261   0.001971  481.09   <2e-16 ***
summary(leaf_N <- lm(log10(imputed_value)~log10(original_value), data=subset(trial, trait=='leaf_N'&!is.na(original_value)&validate==2)))
# log10(original_value) 0.942481   0.002044  461.13   <2e-16 ***
summary(leaf_N <- lm(log10(imputed_value)~log10(original_value), data=subset(trial, trait=='leaf_N'&!is.na(original_value)&validate==3)))
# log10(original_value) 0.948495   0.002035  466.06   <2e-16 ***


#plant vegetative height
with(subset(trial, trait=='plant_height_vegetative'), hist(log10(original_value)))
with(subset(trial, trait=='plant_height_vegetative'), hist(log10(imputed_value)))

with(subset(trial, trait=='plant_height_vegetative' & validate==1), cor.test(original_value, imputed_value,method = "pearson", use = "complete.obs"))
# r 0.9830581   
with(subset(trial, trait=='plant_height_vegetative' & validate==2), cor.test(original_value, imputed_value,method = "pearson", use = "complete.obs"))
# r 0.9806463       
with(subset(trial, trait=='plant_height_vegetative' & validate==3), cor.test(original_value, imputed_value,method = "pearson", use = "complete.obs"))
# r 0.9576646       

summary(plant_height_vegetative <- lm(log10(imputed_value)~log10(original_value), data=subset(trial, trait=='plant_height_vegetative'&!is.na(original_value)&validate==1)))
# log10(original_value)  0.9410341  0.0012587  747.65   <2e-16 ***
summary(plant_height_vegetative <- lm(log10(imputed_value)~log10(original_value), data=subset(trial, trait=='plant_height_vegetative'&!is.na(original_value)&validate==2)))
# log10(original_value)  0.9379691  0.0012598  744.52   <2e-16 ***
summary(plant_height_vegetative <- lm(log10(imputed_value)~log10(original_value), data=subset(trial, trait=='plant_height_vegetative'&!is.na(original_value)&validate==3)))
# log10(original_value)  0.9394736  0.0012648  742.77   <2e-16 ***


#SRL
with(subset(trial, trait=='SRL'), hist(log10(original_value)))
with(subset(trial, trait=='SRL'), hist(log10(imputed_value)))

with(subset(trial, trait=='SRL' & validate==1), cor.test(original_value, imputed_value,method = "pearson", use = "complete.obs"))
# r 0.9034755     
with(subset(trial, trait=='SRL' & validate==2), cor.test(original_value, imputed_value,method = "pearson", use = "complete.obs"))
# r 0.9056699          
with(subset(trial, trait=='SRL' & validate==3), cor.test(original_value, imputed_value,method = "pearson", use = "complete.obs"))
# r 0.9427789          

summary(SRL <- lm(log10(imputed_value)~log10(original_value), data=subset(trial, trait=='SRL'&!is.na(original_value)&validate==1)))
# log10(original_value) 0.90758    0.00753  120.53   <2e-16 ***
summary(SRL <- lm(log10(imputed_value)~log10(original_value), data=subset(trial, trait=='SRL'&!is.na(original_value)&validate==2)))
# log10(original_value) 0.900836   0.007105  126.78   <2e-16 ***
summary(SRL <- lm(log10(imputed_value)~log10(original_value), data=subset(trial, trait=='SRL'&!is.na(original_value)&validate==3)))
# log10(original_value) 0.904751   0.007015  128.97   <2e-16 ***


#seed dry mass
with(subset(trial, trait=='seed_dry_mass'), hist(log10(original_value)))
with(subset(trial, trait=='seed_dry_mass'), hist(log10(imputed_value)))

with(subset(trial, trait=='seed_dry_mass' & validate==1), cor.test(original_value, imputed_value,method = "pearson", use = "complete.obs"))
# r 0.994225     
with(subset(trial, trait=='seed_dry_mass' & validate==2), cor.test(original_value, imputed_value,method = "pearson", use = "complete.obs"))
# r 0.995912        
with(subset(trial, trait=='seed_dry_mass' & validate==3), cor.test(original_value, imputed_value,method = "pearson", use = "complete.obs"))
# r 0.9978231        

summary(seed_dry_mass <- lm(log10(imputed_value)~log10(original_value), data=subset(trial, trait=='seed_dry_mass'&!is.na(original_value)&validate==1)))
# log10(original_value)  0.9856396  0.0005943 1658.603  < 2e-16 ***
summary(seed_dry_mass <- lm(log10(imputed_value)~log10(original_value), data=subset(trial, trait=='seed_dry_mass'&!is.na(original_value)&validate==2)))
# log10(original_value)  0.9870789  0.0004944 1996.642  < 2e-16 ***
summary(seed_dry_mass <- lm(log10(imputed_value)~log10(original_value), data=subset(trial, trait=='seed_dry_mass'&!is.na(original_value)&validate==3)))
# log10(original_value)  0.9866485  0.0004932 2000.554  < 2e-16 ***

