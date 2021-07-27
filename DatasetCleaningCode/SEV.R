#############
#### SEV ###
###########

# Data sent as one large datafile, so will have all SEV experiments cleaned in the same R script
# all data are percent cover
setwd("~/Dropbox/CoRRE_database")

sev.dat <- read.csv("Data/OriginalData/Sites/SEV/npp_quadcover_for_corre.csv")
sev.sp <- read.csv("Data/OriginalData/Sites/SEV/SEV_species names.csv")
sev.sp$genus_species <- paste(sev.sp$genus, sev.sp$species, sep = ".")
sev.sp <- sev.sp[,c(5,11)]
names(sev.sp)[1] <- "species"
sev.dat <- merge(sev.dat, sev.sp, all.x = TRUE)

NFert <- sev.dat[sev.dat$site == "fertilizer",]
WENNDEx <- sev.dat[sev.dat$site == "warming",]
MRME <- sev.dat[sev.dat$site == "MRME",] 
black <- sev.dat[sev.dat$site == "EDGE_black",]
blue <- sev.dat[sev.dat$site == "EDGE_blue",]


#### NFert ######
# Get max cover over the three seasons
NFert <- aggregate(NFert$quadcover, by = list(calendar_year = NFert$year, treatment =NFert$treatment, 
                                             plot_id = NFert$plot, quad = NFert$quad, genus_species = NFert$genus_species),
                  FUN = max)
# Mean cover over the 4 quadrats
NFert <- aggregate(NFert$x, by = list(calendar_year = NFert$calendar_year, treatment =NFert$treatment, 
                                      plot_id = NFert$plot_id, genus_species = NFert$genus_species),
                   FUN = mean)

NFert$site_code <- "SEV"
NFert$project_name <- "NFert"
NFert$data_type <- "cover"
NFert$treatment_year <- NFert$calendar_year - 1994

NFert <- NFert[,c(1,2,3,8,9,6,7,4,5)]
names(NFert)[9] <- "abundance"

write.csv(NFert, "Data/CleanedData/Sites/Species csv/SEV_NFert20.csv", row.names = FALSE)

#### WENNDEx ####

WENNDEx <- aggregate(WENNDEx$quadcover, by = list(calendar_year = WENNDEx$year, treatment =WENNDEx$treatment, 
                                              plot_id = WENNDEx$plot, quad = WENNDEx$quad, genus_species = WENNDEx$genus_species),
                   FUN = max)
# Mean cover over the 4 quadrats
WENNDEx <- aggregate(WENNDEx$x, by = list(calendar_year = WENNDEx$calendar_year, treatment =WENNDEx$treatment, 
                                      plot_id = WENNDEx$plot_id, genus_species = WENNDEx$genus_species),
                   FUN = mean)
WENNDEx$site_code <- "SEV"
WENNDEx$project_name <- "WENNDEx"
WENNDEx$treatment_year <- WENNDEx$calendar_year - 2005
WENNDEx$data_type <- "cover"


WENNDEx <- WENNDEx[,c(1,2,3,9,8,6,7,4,5)]
names(WENNDEx)[9] <- "abundance"

write.csv(WENNDEx, "Data/CleanedData/Sites/Species csv/SEV_WENNDEx20.csv", row.names = FALSE)

#### MRME #### #began 2006 

MRME <- aggregate(MRME$quadcover, by = list(calendar_year = MRME$year, treatment =MRME$treatment, 
                                                  plot_id = MRME$plot, subplot = MRME$subplot,
                                            quad = MRME$quad, genus_species = MRME$genus_species),
                     FUN = max)
# Mean cover over the 4 quadrats
MRME <- aggregate(MRME$x, by = list(calendar_year = MRME$calendar_year, treatment =MRME$treatment, 
                                          plot_id = MRME$plot_id, subplot = MRME$subplot, 
                                    genus_species = MRME$genus_species),
                     FUN = mean)
MRME$site_code <- "SEV"
MRME$project_name <- "MRME"
MRME$data_type <- "cover"
MRME$treatment_year <- MRME$calendar_year - 2005

MRME <- MRME[,c(1,2,3,4,9,10,7,8,5,6)]
names(MRME)[10] <- "abundance"
write.csv(MRME, "Data/CleanedData/Sites/Species csv/SEV_MRME.csv", row.names = FALSE)

#### EDGE ####

black <- aggregate(black$quadcover, by = list(calendar_year = black$year, treatment =black$treatment, 
                                                  plot_id = black$plot, block = black$block, 
                                              quad = black$quad, genus_species = black$genus_species),
                     FUN = max)
# Mean cover over the 4 quadrats
black <- aggregate(black$x, by = list(calendar_year = black$calendar_year, treatment =black$treatment, 
                                          plot_id = black$plot_id, block = black$block, genus_species = black$genus_species),
                     FUN = mean)
black$community_type <- "black_gramma"
black$site_code <- "SEV"
black$project_name <- "EDGE"
black$data_type <- "cover"
black$treatment_year <- black$calendar_year - 2011

black <- black[,c(1,2,3,4,7,10,11,8,9,5,6)]
names(black)[11] <- "abundance"

blue <- aggregate(blue$quadcover, by = list(calendar_year = blue$year, treatment =blue$treatment, 
                                              plot_id = blue$plot, block = blue$block, 
                                              quad = blue$quad, genus_species = blue$genus_species),
                   FUN = max)
# Mean cover over the 4 quadrats
blue <- aggregate(blue$x, by = list(calendar_year = blue$calendar_year, treatment =blue$treatment, 
                                      plot_id = blue$plot_id, block = blue$block, genus_species = blue$genus_species),
                   FUN = mean)
blue$community_type <- "blue_gramma"
blue$site_code <- "SEV"
blue$project_name <- "EDGE"
blue$data_type <- "cover"
blue$treatment_year <- blue$calendar_year - 2011

blue <- blue[,c(1,2,3,4,7,10,11,8,9,5,6)]
names(blue)[11] <- "abundance"

EDGE <- rbind(black, blue)
write.csv(EDGE, "Data/CleanedData/Sites/Species csv/SEV_EDGE20.csv", row.names = FALSE)
