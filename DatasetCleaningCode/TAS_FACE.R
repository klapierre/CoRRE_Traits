setwd("~/Dropbox/CoRRE_database/Data/")

library(dplyr)
library(tidyr)

spcount<-read.csv("OriginalData/Sites/TAS_FACE/Tasface_abund_trt.csv")%>%
  gather(genus_species, abundance, Acaena.echinata:Wurmbea.dioica)

treatment_year<-spcount%>%
  select(calendar_year)%>%
  unique()%>%
  mutate(treatment_year=seq(2,8, by=1))

spdata<-merge(spcount, treatment_year, by="calendar_year")%>%
  mutate(site_code="TAS",
         project_name="FACE",
         data_type = "count")

write.csv(spdata, "CleanedData/Sites/Species csv/TAS_FACE.csv")

# Cover data - do not use
#Load in data
# # Get names from all of the tabs in excel sheet
# tab_names <- excel_sheets(path = "OriginalData/Sites/TAS_FACE/TasFACE_cover.xlsx")
# # import all tabs so that each is a separate element in a list
# myfiles <- lapply(tab_names, function(x) read_excel(path = "OriginalData/Sites/TAS_FACE/TasFACE_cover.xlsx", sheet = x))
# 
# for(i in 2:length(myfiles)){
#   temp <- as.data.frame(t(myfiles[[2]]))
#   colnames(temp) <- temp[1,]
#   temp <- temp[-1,]
#   temp$sample <- rownames(temp)
#   temp$sample[temp$sample == "40179"] <- "Jan-10"
#   temp$sample[temp$sample == "40269"] <- "Apr-10"
#   row.names(temp) <- NULL
#   temp <- temp %>% gather(genus_species, abundance, "Acaena echinata":"Wurmbea dioica")
#   temp <- temp[-which(is.na(temp$abundance)),]
#   temp$year <- str_sub(temp$sample, start= -2)
#   temp$abundance <- as.numeric(temp$abundance)
#   temp$year <- as.numeric(temp$year)
#   temp <- aggregate(temp$abundance, by = list(genus_species = temp$genus_species, calendar_year = temp$year), FUN = max)
#   names(temp)[3] <- "abundance"
#   temp$plot_id <- i-1
#   myfiles[[i]] <- temp
# }




