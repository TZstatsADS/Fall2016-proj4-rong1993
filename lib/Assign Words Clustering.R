#Cluster Songs 
library(stats)
library(dplyr)
library(DescTools)
setwd("~/Desktop/Project4_data")
load("lyr.RData")
lyr = lyr[,-1]
count = apply(lyr,1,sum)
for( i in 1:dim(lyr)[1]){
  print(i)
  lyr[i,] = lyr[i,]/count[i]
}
save(lyr,file = "frequency.RData")

Counts = numeric(30)


#-----Need to find out the way to select best K-----#
for(n in 2:30){
  print(n)
  fit = kmeans(lyr,n)
  Counts[n-1] = fit$tot.withinss
}
which.min(Counts)


#----For now just use n = 10---#

n = 10
fit = kmeans(lyr,n)
Data = data.frame(lyr,Group = fit$cluster)
Label = fit$cluster
save(Label,file="Cluster.RData")



