setwd("~/Documents/Dropbox/converge_diverge/datasets/sept 2013 datasets/Russian Study")
sp<-read.delim("Rus_sp.txt", header=T)
sp2<-sp[,-20]

means<-aggregate(sp2[,22:103], sp2[,2:20], mean)
write.table(means, "rus_sp_means.txt")