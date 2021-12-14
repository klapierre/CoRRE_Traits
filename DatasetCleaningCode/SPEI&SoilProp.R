### CODE TO EXTRACT SPEI DATA FOR CORRE SITES
## ADAPTED FROM CODE PROVIDED BY J. COWLES, C. KAZANSKI, & E.LIND
## UPDATED NOV. 27, 2019 BY K.KIMMEL

## PACKAGES
library(here)
library(ncdf4)
library(GSIF)
library(rjson)
library(sp)
library(chron)
library(raster)
require(jsonlite)

# DATA
sites <- read.csv("C:/Users/ohler/Dropbox/CoRRE_database/Data/CompiledData/siteLocationClimate.csv")


## need local copy of spei0X.nc (X-month interpolation of drought index)
## http://digital.csic.es/handle/10261/128892
## ~316 MB files

spei_01.nc <- nc_open("C:/Users/ohler/Dropbox/CoRRE_database/Data/CleanedData/spei01.nc")
spei_04.nc <- nc_open("C:/Users/ohler/Dropbox/CoRRE_database/Data/CleanedData/spei04.nc")
spei_12.nc <- nc_open("C:/Users/ohler/Dropbox/CoRRE_database/Data/CleanedData/spei12.nc")

t <- ncvar_get(spei_04.nc,'time') # extract information about the time series
nt <- dim(t) #length of time series
tunits <- ncatt_get(spei_04.nc,'time','units')
tunits
nc_close(spei_04.nc) # close connection (keep tidy)

# following depends on form of attribute information
# for SPEI, time attribute is in form 'days since 1900-1-1'
tustr <- strsplit(tunits$value, " ")
tdstr <- strsplit(unlist(tustr)[3],"-")
tmonth <- as.integer(unlist(tdstr)[2])
tday <- as.integer(unlist(tdstr)[3])
tyear <- as.integer(unlist(tdstr)[1])
realt <- chron(dates.=t,origin=c(tmonth,tday,tyear),format=c(dates="m/dd/yyyy"))


# a 'brick' is a 3D compilation of raster layers. think of phyllo dough. mmmm.
spei01 <- brick("C:/Users/ohler/Dropbox/CoRRE_database/Data/CleanedData/spei01.nc")
spei04 <- brick("C:/Users/ohler/Dropbox/CoRRE_database/Data/CleanedData/spei04.nc")
spei12 <- brick("C:/Users/ohler/Dropbox/CoRRE_database/Data/CleanedData/spei12.nc")

dat<- data.frame(month= as.character(),year=as.numeric(),spei_01=as.numeric(),
                 spei_04 = as.numeric(), spei_12 = as.numeric(),
                 id= as.character())
## extract single site
for (i in 1:nrow(sites)) {
  sp <- SpatialPoints(data.frame(longitude=sites[i,7],latitude=sites[i,6]))
  x <- as.vector(extract(spei01,sp))
  y <- as.vector(extract(spei04,sp))
  z <- as.vector(extract(spei12,sp))
  tmp <- data.frame(month=months(as.POSIXct(realt)),
                   year=years(as.POSIXct(realt)), 
                   spei_04 = y, spei_12 = z, id = sites[i,1])
  tmp <- tmp[tmp$year > 1980,]
  dat  <- rbind(dat, tmp)
}

SPEIDat <- merge(sites, dat)
write.csv(SPEIDat, file = "C:/Users/ohler/Dropbox/CoRRE_database/Data/CompiledData/SPEI_CORRE.csv")



### SOIL PROPERTIES
soldf <- data.frame(BLDIFE_sl1= as.numeric(), BLDIFE_sl2= as.numeric(), BLDIFE_sl3= as.numeric(), BLDIFE_sl4= as.numeric(), BLDIFE_sl5= as.numeric(),
                    BLDIFE_sl6= as.numeric(), BLDIFE_sl7= as.numeric(), CECSOL_sl1= as.numeric(), CECSOL_sl2= as.numeric(), CECSOL_sl3= as.numeric(),
                    CECSOL_sl4= as.numeric(), CECSOL_sl5= as.numeric(), CECSOL_sl6= as.numeric(), CECSOL_sl7= as.numeric(), PHIHOX_sl1= as.numeric(),
                    PHIHOX_sl2= as.numeric(), PHIHOX_sl3= as.numeric(), PHIHOX_sl4= as.numeric(), PHIHOX_sl5= as.numeric(), PHIHOX_sl6= as.numeric(),
                    PHIHOX_sl7= as.numeric(), TEXMHT_sl1= as.numeric(), TEXMHT_sl2= as.numeric(), TEXMHT_sl3= as.numeric(), TEXMHT_sl4= as.numeric(),
                    TEXMHT_sl5= as.numeric(), TEXMHT_sl6= as.numeric(), TEXMHT_sl7= as.numeric(), SLTPPT_sl1= as.numeric(), SLTPPT_sl2= as.numeric(),
                    SLTPPT_sl3= as.numeric(), SLTPPT_sl4= as.numeric(), SLTPPT_sl5= as.numeric(), SLTPPT_sl6= as.numeric(), SLTPPT_sl7= as.numeric(),
                    SNDPPT_sl1= as.numeric(), SNDPPT_sl2= as.numeric(), SNDPPT_sl3= as.numeric(), SNDPPT_sl4= as.numeric(), SNDPPT_sl5= as.numeric(),
                    SNDPPT_sl6= as.numeric(), SNDPPT_sl7= as.numeric(), CLYPPT_sl1= as.numeric(), CLYPPT_sl2= as.numeric(), CLYPPT_sl3= as.numeric(),
                    CLYPPT_sl4= as.numeric(), CLYPPT_sl5= as.numeric(), CLYPPT_sl6= as.numeric(), CLYPPT_sl7= as.numeric(), ORCDRC_sl1= as.numeric(),
                    ORCDRC_sl2= as.numeric(), ORCDRC_sl3= as.numeric(), ORCDRC_sl4= as.numeric(), ORCDRC_sl5= as.numeric(), ORCDRC_sl6= as.numeric(),
                    ORCDRC_sl7= as.numeric(), AWCh1_sl1= as.numeric(), AWCh1_sl2= as.numeric(), AWCh1_sl3= as.numeric(), AWCh1_sl4= as.numeric(), 
                    AWCh1_sl5= as.numeric(), AWCh1_sl6= as.numeric(), AWCh1_sl7= as.numeric(), id= as.character())
for(i in 1:nrow(sites)){
  x <- fromJSON(paste0('https://rest.soilgrids.org/query?lon=',sites[i,7],'&lat=',sites[i,6],',&attributes=CECSOL,TEXMHT,ORCDRC,PHIHOX,BLDFIE,SLTPPT,SNDPPT,CLYPPT,AWCh1'), flatten = TRUE)
  if(x$properties$soilmask == 'nodata'){
    soldf[i,c(1:63)] <- NA
    soldf[i,64] <- sites[i,1]
  } else {
  bulkden <- as.data.frame(t(unlist(x$properties$BLDFIE$M)))
  names(bulkden) <- paste("BLDIFE", c("sl1", "sl2", "sl3", "sl4", "sl5", "sl6", "sl7"), sep = "_")
  catex <- as.data.frame(t(unlist(x$properties$CECSOL$M)))
  names(catex) <- paste("CECSOL", c("sl1", "sl2", "sl3", "sl4", "sl5", "sl6", "sl7"), sep = "_")
  pH <- as.data.frame(t(unlist(x$properties$PHIHOX$M)))
  names(pH) <- paste("PHIHOX", c("sl1", "sl2", "sl3", "sl4", "sl5", "sl6", "sl7"), sep = "_")
  tex <- as.data.frame(t(unlist(x$properties$TEXMHT$M)))
  names(tex) <- paste("TEXMHT", c("sl1", "sl2", "sl3", "sl4", "sl5", "sl6", "sl7"), sep = "_")
  silt <- as.data.frame(t(unlist(x$properties$SLTPPT$M)))
  names(silt) <- paste("SLTPPT", c("sl1", "sl2", "sl3", "sl4", "sl5", "sl6", "sl7"), sep = "_")
  sand <- as.data.frame(t(unlist(x$properties$SNDPPT$M)))
  names(sand) <- paste("SNDPPT", c("sl1", "sl2", "sl3", "sl4", "sl5", "sl6", "sl7"), sep = "_")
  clay <- as.data.frame(t(unlist(x$properties$CLYPPT$M)))
  names(clay) <- paste("CLYPPT", c("sl1", "sl2", "sl3", "sl4", "sl5", "sl6", "sl7"), sep = "_")
  SOM <- as.data.frame(t(unlist(x$properties$ORCDRC$M)))
  names(SOM) <- paste("ORCDRC", c("sl1", "sl2", "sl3", "sl4", "sl5", "sl6", "sl7"), sep = "_")
  wat <- as.data.frame(t(unlist(x$properties$AWCh1$M)))
  names(wat) <- paste("AWCh1", c("sl1", "sl2", "sl3", "sl4", "sl5", "sl6", "sl7"), sep = "_")
  tmp <- cbind(bulkden, catex, pH, tex, silt, sand, clay, SOM, wat)
  tmp$id <- sites[i,1]
  soldf <- rbind.data.frame(soldf, tmp, stringsAsFactors = FALSE)
  }
}

soldf <- merge(sites, soldf)

write.csv(soldf, here("Data", "Soil_CORRE.csv"))
