##################
#### KNZ_IRG ####
################
#setwd("~/Dropbox/CoRRE_database")
setwd("C://Users/mavolio2/Dropbox/CoRRE_database")
library(tidyverse)

file <- "https://portal.edirepository.org/nis/dataviewer?packageid=knb-lter-knz.72.6&entityid=2798ee2d63b042202cdb97fa25fe3a30"
df <- read.csv(file, header = TRUE)
sp_list <- read.csv("Data/OriginalData/Sites/knz/sp_list.csv") %>% 
  rename(SpeciesNumber=code) %>% 
  select(SpeciesNumber, genus, species)


# convert cover classes to midpoints
df$Cover[df$Cover == 1] <- 0.5
df$Cover[df$Cover == 2] <- 3
df$Cover[df$Cover == 3] <- 15
df$Cover[df$Cover == 4] <- 37.5
df$Cover[df$Cover == 5] <- 62.5
df$Cover[df$Cover == 6] <- 85
df$Cover[df$Cover == 7] <- 97.5

df <- merge(df, sp_list)
df$genus_species <- paste(df$genus, df$species, sep = ".")

df2<-df %>% 
  rename(calendar_year=RecYear, treatment=Transect, abundance=Cover) %>% 
  mutate(plot_id=ifelse(treatment=="c", Plot, ifelse(treatment=="i", Plot+100, 999)),
         community_type=ifelse(Plot %in% c(1:6, 16:21), "l", ifelse(Plot %in% c(10:15, 26:31), "u", "drop"))) %>% 
  select(calendar_year, treatment, abundance, genus_species, plot_id, community_type) %>% 
  filter(community_type!="drop", (plot_id %in% c(1:5, 10:13, 101:105, 110:113))) %>% 
  mutate(site_code="KNZ",
         project_name="IRG",
         data_type="cover",
         treatment_year=calendar_year-1990)

#write.csv(df2, "Data/CleanedData/Sites/Species csv/KNZ_IRG.csv", row.names = FALSE)

file1 <- "https://pasta.lternet.edu/package/data/eml/knb-lter-knz/72/13/d7d500227665f76533332ebade88deeb"
df1 <- read.csv(file1, header = TRUE)
