setwd("~/Documents/Dropbox/converge_diverge/datasets/checked data_04292014/not_perfect")
dir()
gap<-read.delim("DCGS_gap.txt")
library(reshape2)
gap2<-gap[-5]

melt<-melt(gap, id="replicate", "calendar_year")

means<-aggregate(gap2[,6:130], gap2[,1:5], mean)
write.table(means, "gap_means.txt")
