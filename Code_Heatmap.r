library(gplots)
path <- getwd()
temp <- read.csv(paste(path,'klp_f.csv',sep='/'))
temp <- temp
y <- data.matrix(temp)
print(y)

temp_heatmap <- heatmap(y,Rowv=TRUE,Colv=NA,col=cm.colors(256),margins=c(5,10),labRow=paste('L',c(2:10)),cexRow=0.5,cexCol=0.5)
print(temp_heatmap)
