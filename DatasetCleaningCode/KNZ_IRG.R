##################
#### KNZ_IRG ####
################
#setwd("~/Dropbox/CoRRE_database")
#setwd("C:/Users/K_WILCOX/OneDrive - UNCG/Working groups/sDiv/CoRRE data")
library(tidyverse)

file <- "https://pasta.lternet.edu/package/data/eml/knb-lter-knz/72/13/2798ee2d63b042202cdb97fa25fe3a30"
df_raw <- read.csv(file, header = TRUE)
sp_list_raw <- read.csv("Data/OriginalData/Sites/knz/sp_list.csv")
#sp_list_raw <- read.csv("sp_list.csv") #krw local copy
sp_list <- sp_list_raw %>%
  dplyr::select(code, genus, species) %>%
  rename("Spcode"="code")

# Set first year of experiment
irg_exp_year_1 <- 1991

# convert cover classes to midpoints
df_raw$Cover[df_raw$Cover == 1] <- 0.5
df_raw$Cover[df_raw$Cover == 2] <- 3
df_raw$Cover[df_raw$Cover == 3] <- 15
df_raw$Cover[df_raw$Cover == 4] <- 37.5
df_raw$Cover[df_raw$Cover == 5] <- 62.5
df_raw$Cover[df_raw$Cover == 6] <- 85
df_raw$Cover[df_raw$Cover == 7] <- 97.5

### Create tables and vectors for community designation and for removal of B. bladhii invaded plots
upland_plot_vec <- c(10:15, 27:31)
lowland_plot_vec <- c(1:5, 16:20)
slope_plot_vec <- c(6:8, 21:26) # there is no plot 9 in dataset (the plot was removed from the experiment in the 1990s)

slope_table <- data.frame(
  Plot = c(upland_plot_vec, lowland_plot_vec, slope_plot_vec),
  community = c(rep("u",length(upland_plot_vec)),
                rep("l",length(lowland_plot_vec)),
                rep("s",length(slope_plot_vec))
                )
)

invaded_plot_vec <- c(2, 6, 7, 8, 14)

### Clean and format data
df <- df_raw %>%
  left_join(sp_list, by="Spcode") %>%
  mutate(genus_species=paste(genus, species, sep=" ")) %>%
  mutate(Trt = replace(Trt, Trt=="a", "c")) %>% # Create uniform treatment designations
  full_join(slope_table, by="Plot") %>%
  filter(community %in% c("l", "u")) %>% # Remove plots on slope
  filter(!Plot %in% invaded_plot_vec) %>% # Remove plots invaded by B. bladhii
  rename(calendar_year=RecYear, treatment=Trt, plot_id=Plot, abundance=Cover, community_type=community, block=Transect) %>% # Question -- do you want transect set as block in the data?
  mutate(site_code = "KNZ",
         project_name = "IRG",
         data_type = "cover",
         treatment_year = calendar_year-(irg_exp_year_1-1),
         version = 1) %>% # Not sure if this version number is correct
  dplyr::select(site_code,
                project_name,
                community_type,
                calendar_year,
                treatment_year,
                treatment,
                block,
                plot_id,
                data_type, 
                version,
                genus_species,
                abundance)
  
### Check data frame to make sure it looks right
with(filter(df, calendar_year==1995 & plot_id == 3),
     table(genus_species, treatment))
with(filter(df, treatment == "c"),
     table(genus_species, calendar_year))
missing_species_names <- filter(df, genus_species==". .")
unique(missing_species_names$Spcode)

## There are three species numbers that have no names within the most updated (May 2023) sp_list from Konza
## species numbers are 188, 249, 251. 188 is found in Plot 7, 249 in plot 4, 251 is found in plot 10-13,15,26-28
## I have emailed Yang and Jeff about this (May 23, 2023) -- will update when I hear back

#write.csv(df, "Data/CleanedData/Sites/Species csv/KNZ_IRG.csv", row.names = FALSE)

# file1 <- "https://pasta.lternet.edu/package/data/eml/knb-lter-knz/72/13/d7d500227665f76533332ebade88deeb"
# df1 <- read.csv(file1, header = TRUE)
