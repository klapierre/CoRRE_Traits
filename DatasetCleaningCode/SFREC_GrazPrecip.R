setwd("~/Dropbox/converge_diverge/datasets/ORIGINAL_DATASETS/SFREC_HallettDissertation")

library(tidyr)
library(dplyr)

data<-read.csv("data.csv")

species<-read.csv("speciescode.csv")

treatment_year<-data%>%
  select(calendar_year)%>%
  unique()%>%
  mutate(treatment_year=seq(1,3, by=1))

data1<-merge(data, species, by="species")%>%
  select(-X, -X.1, -X.2, -X.3, -X.4, -species)

data2<-merge(data1, treatment_year, by="calendar_year")%>%
  filter(wdpair!="D")%>%
  select(-wdpair)%>%
  mutate(site_code="SFREC", project_name="HallettDissertation")

write.csv(data2, "SFREC_HallettDissertation.csv")


test<-read.csv("SFREC_HallettDissertation.csv")

check<-test%>%
  tbl_df%>%
  group_by(community_type, treatment, plot_id)%>%
  summarize(abundance=length(abundance))%>%
  tbl_df%>%
  group_by(community_type, treatment)%>%
  summarize(rep=length(abundance))