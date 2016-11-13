#Analysis Words Frequencies
setwd("~/Desktop/Project4_data")
load("lyr.RData")
Voc = names(lyr)[-1]
lyr = lyr[,-1]#Delete track_id

#Train and Test 
ind = 1:2350
Train = sample(ind,2250)
Test = ind[-Train]

#naive ranks
temp = lyr[Train,]
temp[temp!=0] = 1
colnames(temp) = 1:dim(temp)[2]
High.Freq.Words<-as.numeric(names(sort(apply(temp[,-1],2,sum),decreasing = TRUE)))

K = cor(temp)


#Test
Test.Data = lyr[Test,] 
rank = High.Freq.Words
#rank = rep(1,dim(Test.Data)[2])
mean(apply(Test.Data,1,Score))



Score<-function(words){
  Appear = Voc[which(words!=0)]
  m = length(Appear)
  R = which(Voc%in%Appear)
  w = rank[R]
  rmean = mean(rank)
  Score = 1/(m*rmean)*sum(w)
  return(Score)
}


