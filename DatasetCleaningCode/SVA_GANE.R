setwd("C:/Users/Meghan/Dropbox/converge_diverge/datasets/sept 2013 datasets/clare robinson")
sp<-read.delim("CHR_sp.txt", header=T)
melt<-melt(sp, id=c("Calendar_Year", "Species"))
cast<-cast(melt, Calendar_Year+variable~Species)
cast[is.na(cast)]<-0
write.table(cast, "CHR_SPECIES.txt")