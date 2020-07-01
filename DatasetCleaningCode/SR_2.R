setwd("~/Dropbox/converge_diverge/datasets/ORIGINAL_DATASETS/seabloom/exp2/seabloom-ca-data")

library(dplyr)
library(tidyr)

dat<-read.csv("seed-add-data-output-full-plant-cover.csv")

dat2<-dat%>%
  filter(seed.total==999|seed.total==0)%>%
  mutate(treatment=paste(seed.total, ntrt, dist, sep="_"))%>%
  select(site, block, plot, treatment)%>%
  unique()

#this data is not useable b/c there are only 3 reps/treatment.
