setwd("~/Dropbox/CoRRE_database")
setwd("C:\\Users\\mavolio2\\Dropbox\\CoRRE_database")
setwd('C:\\Users\\komatsuk\\Dropbox (Smithsonian)\\working groups\\CoRRE\\CoRRE_database') #kim's desktop
setwd('C:\\Users\\lapie\\Dropbox (Smithsonian)\\working groups\\CoRRE\\CoRRE_database') #kim's laptop


library(tidyverse)

anpp<-read.csv("Data/OriginalData/Sites/SERC_TMECE/SERC1 1997-2019 stacked.csv")%>%
  rename(treatment=Treatment,
         community_type=Community,
         abundance=abundance.2)%>%
  group_by(site_code, project_name, community_type, block, plot_id, calendar_year, treatment_year, treatment) %>% 
  summarize(anpp=sum(abundance))%>%
  ungroup()


# write.csv(anpp, "Data/CleanedData/Sites/ANPP csv/SERC_TMECE_anpp.csv")

###NOTE: species data moved directly from clean file into clean folder, no changes made in R

#control plot ANPP (on avg): different from the anpp plots which had chambers on them, put this number into the ANPP spreadsheet

anppCtl <- anpp%>%
  filter(treatment=='C')%>%
  na.omit()%>%
  group_by(treatment_year, community_type)%>%
  summarise(anpp_mean=mean(anpp))%>%
  ungroup()%>%
  group_by(community_type)%>%
  summarise(anpp_mean2=mean(anpp_mean))%>%
  ungroup()
