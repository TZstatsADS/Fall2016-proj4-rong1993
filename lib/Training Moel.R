#----Train Model---#
load("Features.RData")
load("vocab.RData")
load("doc.RData")
load("lyr.RData")
lyr = lyr[,-1]
#lyr = lyr[,-c(2,3,6:30)]
load("New.Features.RData")

A = New.F[[1]]
B = New.F[[2]]
Data = A[-B,]
lyr = lyr[-B,]


KK = matrix(0,ncol = 5, nrow = 6)
t1 = Sys.time()
for(treshold in seq(from = 0,to=1,by = 0.1)){
  for( K in 6:10){
  n = dim(Data)[1]
  PCA = 0.99
  train.index = sample(1:n,2300)
  Train.Fea = Data[train.index,]
  Test.Fea = Data[-train.index,]
  alpha <- 0.02
  eta <- 0.02
  documents = doc[train.index]
  A = Topic.Modeling(documents,K,5000)
  print("Get Cluster")
  Topic.Dist = A$Topic.Dist
  Words.Topic = A$Words.Topic
  T.Lyr = lyr[-train.index,]
  Ranking = Predict(Train.Fea,Test.Fea,Topic.Dist,Words.Topic,PCA,treshold=0)
  print("Get Ranking")
  Scores = Top.Rank(Ranking,T.Lyr)
  KK[K-4,(treshold*10+1)] = mean(Scores)
  }  
}
t2 = Sys.time()

KK
