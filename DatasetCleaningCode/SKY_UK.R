setwd("C:/Users/Meghan/Dropbox/converge_diverge/datasets/sept 2013 datasets/Kentucky/corrected")
ky<-read.delim("SKY_UK.txt")
ky2=ky[, c(3:4,6,8:33)]
#melted<-melt(ky2, id=c("Year", "sample", "Plot_ID"))
#Casted<-dcast(melted, Year+Plot_ID~sample)

max<-aggregate(ky2[,4:29], ky2[,2:3], max)
sort<-ky2[order(ky2$Year, ky2$Plot_ID),]
write.table(max,"max.txt")