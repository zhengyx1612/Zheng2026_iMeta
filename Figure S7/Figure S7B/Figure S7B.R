library(bipartite)

df <- read.csv('food web data.csv',row.names=1)
data <- read.csv('Sort file.csv')
df <- t(df)
df <- as.data.frame(df)

#先重排幼虫
colnames(df)
insect <- as.character(data[,2])
plant <- as.character(data[1:69,1])
df2 <- df[,order(factor(colnames(df),levels=c(insect)))]
df2 <- df2[order(factor(rownames(df2),levels=c(plant))),]
visweb(df2,type="none",pred.lablength = 50)
