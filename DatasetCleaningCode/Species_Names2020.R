###########################
### Species cleaning ####
########################
setwd("/Users/kaitlinkimmel/Dropbox/CoRRE_database/")

# load libraries
library(Taxonstand)

# import data
full.list <- read.csv ("Data/CompiledData/SpeciesList.csv", row.names = 1)
oldsp <- read.csv("Data/CompiledData/CoRRE_TRY_species_list.csv", row.names = 1)

oldsp$oldsp <- 1 # create a column to indicate name was in old species list
oldsp1 <- oldsp[,c(1,4)] # get rid of extra columns

merged.list<- merge(full.list, oldsp1, all.x = TRUE) #merge new list with old list

# get species that are not in old list 
unidentified.sp <- data.frame(genus_species=merged.list[which(is.na(merged.list$oldsp)),1])

clean.list <- TPL(unidentified.sp$genus_species)

matched.sp <- clean.list[clean.list$Plant.Name.Index == TRUE | clean.list$Taxonomic.status == "Accepted",]
matched.sp$species_matched <- paste(matched.sp$New.Genus, matched.sp$New.Species, sep = " ")
matched.sp <- matched.sp[,c(1,27)]
names(matched.sp)[1] <- "genus_species"
matched.sp <- matched.sp[-which(is.na(matched.sp$genus_species)),]
write.csv(matched.sp, "good_sp.csv")
unmatched.sp <- clean.list[clean.list$Plant.Name.Index == FALSE & clean.list$Taxonomic.status != "Accepted",]
unmatched.sp$species_matched <- paste(unmatched.sp$New.Genus, unmatched.sp$New.Species, sep = " ")
unmatched.sp <- unmatched.sp[,c(1,27)]
unmatched.sp <- unmatched.sp[-which(is.na(unmatched.sp$Taxon)),]
write.csv(unmatched.sp, "sp_fix.csv")


# cleaning unmatched species

# 1. change spp to sp
unmatched.sp$species_matched <- gsub("spp", "sp", unmatched.sp$species_matched)
# 2. change species to sp
unmatched.sp$species_matched <- gsub("species", "sp", unmatched.sp$species_matched)
# 3. Change NA to sp
unmatched.sp$species_matched <- gsub(" NA", " sp", unmatched.sp$species_matched)
# 4. Change family to sp
unmatched.sp$species_matched <- gsub(" family", " sp", unmatched.sp$species_matched)
# 4. Fix individual species
unmatched.sp$species_matched <- gsub("salsolacollina collina", "Salsola collina", unmatched.sp$species_matched)
unmatched.sp$species_matched <- gsub("aristida_caput medusae", "Aristida caput-medusae", unmatched.sp$species_matched)
unmatched.sp$species_matched[unmatched.sp$species_matched == "artemisia (caudata)"] <- "Artemisia campestris"
unmatched.sp$species_matched <- gsub("dichanthelium oliganthes", "Dichanthelium oligosanthes", unmatched.sp$species_matched)
unmatched.sp$species_matched <- gsub("euphorbia (supina)", "Euphorbia maculata", unmatched.sp$species_matched)
unmatched.sp$species_matched[unmatched.sp$species_matched == "euphorbia (supina)"] <- "Euphorbia maculata"
unmatched.sp$species_matched <- gsub("forb hibiscus", "hibiscus sp", unmatched.sp$species_matched)
unmatched.sp$species_matched <- gsub("gypshophyla sthrutium", "gypsophila struthium", unmatched.sp$species_matched)
unmatched.sp$species_matched <- gsub("hyacinthoides non", "Hyacinthoides non-scripta", unmatched.sp$species_matched)
unmatched.sp$species_matched <- gsub("knotweed sp", "Reynoutria japonica", unmatched.sp$species_matched)
unmatched.sp$species_matched <- gsub("linnea borealis", "Linnaea borealis", unmatched.sp$species_matched)
unmatched.sp$species_matched <- gsub("lychnis latifolia", "Silene latifolia", unmatched.sp$species_matched)
unmatched.sp$species_matched <- gsub("oxytropis caerulea", "Oxytropis coerulea", unmatched.sp$species_matched)
unmatched.sp$species_matched <- gsub("petalostemum villosum", "Dalea villosa", unmatched.sp$species_matched)
unmatched.sp$species_matched[unmatched.sp$species_matched == "plantago (purshii)"] <- "Plantago patagonica"
unmatched.sp$species_matched <- gsub("ranoncolos rhombiodeu", "Ranunculus rhomboideus", unmatched.sp$species_matched)
unmatched.sp$species_matched <- gsub("robinia pseudo acacia", "Robinia pseudoacacia", unmatched.sp$species_matched)
unmatched.sp$species_matched <- gsub("rubus pennsylvanicus", "Rubus pensilvanicus", unmatched.sp$species_matched)
unmatched.sp$species_matched <- gsub("silene jenisseensis", "Silene jenisseensis", unmatched.sp$species_matched)
unmatched.sp$species_matched <- gsub("symphyotrichum novae", "Symphyotrichum novae-angliae", unmatched.sp$species_matched)
unmatched.sp$species_matched[unmatched.sp$species_matched == "pycnan vir"] <- "Pycnanthemum virginianum"
# change column names
names(unmatched.sp)[1] <- "genus_species"
# combine with matched species
newsp.match <- rbind(matched.sp, unmatched.sp)

unique.newsp <- data.frame(species_matched = (unique(newsp.match$species_matched)), new.sp = 1)
unique.oldsp <- data.frame(species_matched = unique(oldsp$species_matched), old.sp = 1)
all.sp<-merge(unique.newsp, unique.oldsp, all = TRUE)
all.sp$old.sp[which(is.na(all.sp$old.sp))] <- 0
all.sp$new.sp[which(is.na(all.sp$new.sp))] <- 0

need.traits <- all.sp[which(all.sp$new.sp ==1 & all.sp$old.sp ==0),]
need.traits <- need.traits[!grepl(" sp",fixed = TRUE, need.traits$species_matched),]
need.traits <- need.traits[!grepl("forb",fixed = TRUE, need.traits$species_matched),]
need.traits <- need.traits[!grepl("unknown",fixed = TRUE, need.traits$species_matched),]
need.traits <- need.traits[!grepl("unidentified",fixed = TRUE, need.traits$species_matched),]
need.traits <- need.traits[!grepl("unk",fixed = TRUE, need.traits$species_matched),]
need.traits <- need.traits[!grepl("grass",fixed = TRUE, need.traits$species_matched),]
need.traits <- need.traits[!grepl("legume",fixed = TRUE, need.traits$species_matched),]
need.traits <- need.traits[!grepl("dicot",fixed = TRUE, need.traits$species_matched),]
need.traits <- need.traits[!grepl("lichen", need.traits$species_matched),]
need.traits <- need.traits[!grepl("rush",fixed = TRUE, need.traits$species_matched),]
need.traits <- need.traits[!grepl("un ",fixed = TRUE, need.traits$species_matched),]

### Notes: lespedeza discola???, marsilea cocinea??? solanum grascilens bonariensis blanco???
## dichelachne squamulosum, gliseria peruviana

### herbdiv na values, gap 	pr_vi



