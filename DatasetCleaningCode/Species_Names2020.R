###########################
### Species cleaning ####
########################

## In this script the species names are cleaned so that they are standardized
## between experiments and to be merged with TRY.
## Two files are read in - the full species list created in the Abundance_merge.R
## file and the list of the corrected names from version 1.0 (note: no code for this
## because a lot of the cleaning for this list was done by hand)
## the full species list is run through the TPL() function from Taxonstand
## to save time so the species list does not always have to be run through TPL()
## intermediate stages are saved to run the rest of the code

# Set working directory
setwd("/Users/kaitlinkimmel/Dropbox/CoRRE_database/")

# load libraries
library(Taxonstand)

# import data
full.list <- read.csv ("Data/CompiledData/Species_lists/SpeciesList.csv", row.names = 1)
oldsp <- read.csv("Data/CompiledData/CoRRE_TRY_species_list.csv", row.names = 1)
moss <- read.delim("Data/OriginalData/Traits/Bryophytes.txt", sep = ",", header = FALSE) #from: http://www.mobot.org/mobot/tropicos/most/bryolist.shtml
tree.spold <- read.csv("Data/TRYCoRREMerge/species_families_trees_compelete.csv")
# Manipulating oldsp to prepare to merge with the new species list
oldsp$oldsp <- 1 # create a column to indicate name was in old species list
oldsp1 <- oldsp[,c(1,4)] # get rid of extra columns

# Merge new list with old list
merged.list<- merge(full.list, oldsp1, all.x = TRUE) 

# get species that are not in old list 
unidentified.sp <- data.frame(genus_species=merged.list[which(is.na(merged.list$oldsp)),1])

# Run species which were not identified in the old list through the TPL function to match with accepted species names
clean.list <- TPL(unidentified.sp$genus_species)

# Pull out matched species (i.e. those that had a match in The Plant List) -
# These do not need to nbe checked further. 
matched.sp <- clean.list[clean.list$Plant.Name.Index == TRUE | clean.list$Taxonomic.status == "Accepted",]
matched.sp$species_matched <- paste(matched.sp$New.Genus, matched.sp$New.Species, sep = " ") # creating column with cleaned name
matched.sp <- matched.sp[,c(1,27,13,26)] # getting two relevant columns
names(matched.sp)[1] <- "genus_species" #renaming column
matched.sp <- matched.sp[-which(is.na(matched.sp$genus_species)),] # one na value.. not sure why
matched.sp$species_matched[matched.sp$species_matched == "unkown big"] <- "Unknown"
write.csv(matched.sp, "Data/CompiledData/Species_lists/good_sp.csv") # saving to load back in later

## Pull out species with no match in TPL to hand fix names
unmatched.sp <- clean.list[clean.list$Plant.Name.Index == FALSE & clean.list$Taxonomic.status != "Accepted",]
unmatched.sp$species_matched <- paste(unmatched.sp$New.Genus, unmatched.sp$New.Species, sep = " ") # creating column with "cleaned" name
unmatched.sp <- unmatched.sp[,c(1,27,13,26)] # getting necessary columns
unmatched.sp <- unmatched.sp[-which(is.na(unmatched.sp$Taxon)),]
write.csv(unmatched.sp, "Data/CompiledData/Species_lists/sp_fix.csv") # save intermediate file 


# cleaning unmatched species
# Read ing unmatched.sp
unmatched.sp <- read.csv("Data/CompiledData/Species_lists/sp_fix.csv", row.names = 1)
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
unmatched.sp$species_matched <- gsub("ranoncolos rhombiodeus", "Ranunculus rhomboideus", unmatched.sp$species_matched)
unmatched.sp$species_matched <- gsub("robinia pseudo acacia", "Robinia pseudoacacia", unmatched.sp$species_matched)
unmatched.sp$species_matched <- gsub("rubus pennsylvanicus", "Rubus pensilvanicus", unmatched.sp$species_matched)
unmatched.sp$species_matched <- gsub("silene jenisseensis", "Silene jenisseensis", unmatched.sp$species_matched)
unmatched.sp$species_matched <- gsub("symphyotrichum novae", "Symphyotrichum novae-angliae", unmatched.sp$species_matched)
unmatched.sp$species_matched[unmatched.sp$species_matched == "pycnan vir"] <- "Pycnanthemum virginianum"
unmatched.sp$species_matched[unmatched.sp$species_matched == "solanum grascilens"] <- "Solanum chenopodioides"
unmatched.sp$species_matched[unmatched.sp$species_matched == "marsilea cocinea"] <- "Marsilea ancylopoda"
unmatched.sp$species_matched[unmatched.sp$species_matched == "gliseria peruviana"] <- "Glyceria multiflora"
unmatched.sp$species_matched[unmatched.sp$species_matched == "unkown big leaf"] <- "Unknown"
unmatched.sp$species_matched[grepl("forb",fixed = TRUE, unmatched.sp$species_matched)] <- "Unknown"
unmatched.sp$species_matched[grepl("unknown",fixed = TRUE, unmatched.sp$species_matched)]<- "Unknown"
unmatched.sp$species_matched[grepl("unidentified",fixed = TRUE, unmatched.sp$species_matched)]<- "Unknown"
unmatched.sp$species_matched[grepl("unk",fixed = TRUE, unmatched.sp$species_matched)]<- "Unknown"
unmatched.sp$species_matched[grepl("grass",fixed = TRUE, unmatched.sp$species_matched)]<- "Unknown"
unmatched.sp$species_matched[grepl("legume",fixed = TRUE, unmatched.sp$species_matched)]<- "Unknown"
unmatched.sp$species_matched[grepl("dicot",fixed = TRUE, unmatched.sp$species_matched)]<- "Unknown"
unmatched.sp$species_matched[grepl("lichen", unmatched.sp$species_matched)]<- "Unknown"
unmatched.sp$species_matched[grepl("rush",fixed = TRUE, unmatched.sp$species_matched)]<- "Unknown"
unmatched.sp$species_matched[grepl("un ",fixed = TRUE, unmatched.sp$species_matched)]<- "Unknown"

# change column names
names(unmatched.sp)[1] <- "genus_species"

## Run unmatched thru TPL again

unmatched.sp.new <- TPL(unmatched.sp$species_matched)
unmatched.sp.new$species_matched <- paste(unmatched.sp.new$New.Genus, unmatched.sp.new$New.Species, sep = " ")
corrected.names <- unmatched.sp.new[which(unmatched.sp.new$New.Species != "sp"),]
corrected.names <- corrected.names[,c(1,27,13,26)]
names(corrected.names) <- c("genus_species","species_matched","Family","Tax_res")
# combine with matched species
matched.sp <- read.csv("Data/CompiledData/Species_lists/good_sp.csv", row.names = 1)
newsp.match <- rbind(matched.sp, corrected.names)


unique.newsp <- data.frame(species_matched = (unique(newsp.match$species_matched)), new.sp = 1)
unique.oldsp <- data.frame(species_matched = unique(oldsp$species_matched), old.sp = 1)
all.sp<-merge(unique.newsp, unique.oldsp, all = TRUE)
all.sp$old.sp[which(is.na(all.sp$old.sp))] <- 0
all.sp$new.sp[which(is.na(all.sp$new.sp))] <- 0

need.traits <- all.sp[which(all.sp$new.sp ==1 & all.sp$old.sp ==0),]
need.traits <- need.traits[!grepl("Unknown",fixed = TRUE, need.traits$species_matched),]
need.traits <- merge(need.traits, unique(newsp.match[,c(2,3)]), all.x = TRUE)
need.traits <- need.traits[-which(need.traits$species_matched == "pioneer mosses"),]

# caplitalize first letter of species names function modified from https://rstudio-pubs-static.s3.amazonaws.com/408658_512da947714740b99253228f084a08a9.html
CapStr <- function(y) { 
  c <- strsplit(y, " ")[[1]][1]
  d <- strsplit(y, " ")[[1]][2]
  paste(paste(toupper(substring(c, 1,1)), substring(c, 2),
              sep="", collapse=" "), d, sep = " ")
}
need.traits$species_matched <- sapply(need.traits$species_matched, CapStr)

moss$Family <- sapply(strsplit(moss$V1, split = " "),`[`, 1, simplify=FALSE)
moss <- moss[which(moss$Family != ""),]
need.traits <- need.traits[-which(need.traits$Family %in% moss$Family),]
need.traits <- need.traits[-which(need.traits$Family == "Anastrophyllaceae"),]
need.traits <- need.traits[-which(need.traits$species_matched == "Nephroma arcticum"),]

need.traits$Family[need.traits$species_matched == "Dichelachne squamulosum"] <- "Poaceae"
need.traits$Family[need.traits$species_matched == "Lespedeza discola"] <- "Fabaceae"
need.traits$Family[need.traits$species_matched == "Robinia pseudo"] <- "Fabaceae"
need.traits$Family[need.traits$species_matched == "Silene jenisseensis"] <- "Caryophyllaceae"

write.csv(need.traits, "Data/CompiledData/Species_lists/newsp2020.csv")


### Create file to indicate if the species is a tree or not
tree.sp <- need.traits
tree.sp$tree.non.tree <- "non-tree"
tree.sp$tree.non.tree[tree.sp$species_matched %in% c("Asimina triloba", "Juniperus communis", "Diospyros virginiana", 
                                       "Tilia europaea", "Morus rubra", "Fraxinus excelsior", "Crataegus secta",
                                       "Prunus avium", "Prunus padus","Pyrus pyraster", "Populus tremula",
                                       "Salix atrocinerea", "	Acer pseudoplatanus")| 
          tree.sp$Family %in% c("Betulaceae", "Fagaceae", "Juglandaceae", "Pinaceae")] <- "tree"

# merge with previous version
tree.sp <- tree.sp[,c(1,4,5)]
names(tree.sp)[2]<- "family"
tree.spnew <- rbind(tree.sp, tree.spold)
write.csv(tree.spnew,"Data/TRYCoRREMerge/species_families_trees_compelete_2020.csv", row.names = FALSE)

#### Create a dataframe with full species list by combining old sp with new sp. 
names(newsp.match)[4] <- "type"
newsp.match$type[newsp.match$Family %in% moss$Family] <- "moss/lichen"
newsp.match$type[newsp.match$Family == "Anastrophyllaceae"] <- "moss/lichen"



full.splist <- rbind(newsp.match[,c(1,2,4)], oldsp[,c(1:3)])
fix <- which(!grepl( " sp\\.", full.splist$species_matched) & grepl(" species", full.splist$genus_species))
full.splist$species_matched[fix]<- full.splist$genus_species[fix]
full.splist$species_matched <- gsub("species", "sp.", full.splist$species_matched)
# caplitalize first letter of species names function modified from https://rstudio-pubs-static.s3.amazonaws.com/408658_512da947714740b99253228f084a08a9.html

full.splist$species_matched <- sapply(full.splist$species_matched, CapStr)

full.splist$type[full.splist$type == "Species"] <- 'identified species'
full.splist$type[full.splist$type == "Genus"] <- 'identified genus'
full.splist$type[fix] <- "identified genus"
names(full.splist)[1] <- "species"


## create a "remove" column where unknowns, etc can be removed easily

full.splist$remove <- 0
full.splist$remove[full.splist$species_matched == "Unknown NA"] <- 3
full.splist$remove[full.splist$type == "identified genus"] <- 1
full.splist$remove[full.splist$type == "moss/lichen"] <- 2
full.splist$remove[full.splist$species_matched == "Pioneer mosses"] <- 2


write.csv(full.splist, "Data/CompiledData/Species_lists/fullsp_list2020.csv", row.names = FALSE)
### Notes: lespedeza discola???
## dichelachne squamulosum???

### herbdiv na values, gap 	pr_vi



