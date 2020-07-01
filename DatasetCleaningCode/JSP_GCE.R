setwd("~/Documents/Dropbox/converge_diverge/datasets/ORIGINAL_DATASETS/jasper ridge")
jsp<-read.delim("JPS_GCE2.txt")
dir()
library(reshape2)

all<-melt(jsp, id=c("Plot_id", "treatment", "SPECIES"))
all4<-dcast(all, Plot_id+treatment+variable~SPECIES)
all4[is.na(all4)]<-0

write.table(all4, "JSP_GCE3.txt")

bio<-read.delim("JRGCE_ANPP.txt")

sum<-aggregate(bio[,10:13], bio[,1:8], sum)
