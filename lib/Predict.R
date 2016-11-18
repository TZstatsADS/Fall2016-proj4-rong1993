#Use this file to predict!


load("Data/doc.RData")
load("Data/lyr.RData")
load("Data/New.Features.RData")
load("Data/Topic.Modeling.RData")
load("Data/vocab.RData")
source("lib/Main.R")
Predict.Rank<-function(Train.Fea,Test,Topic.Modeling,vocab){#Test is a list with all features
  Topic.Dist = TP$Topic.Dist
  Words.Topic = TP$Words.Topic
  Test.Fea = NULL
  for( i in 1:length(Test)){
    x = Test[[i]]
    temp = Get.Features(x)
    Test.Fea = rbind(Test.Fea,temp)
  }
  Ranking = Predict(Train.Fea,Test.Fea,Topic.Dist,Words.Topic,PCA = 0.99,treshold=0.1)
  colnames(Ranking) = vocab[-length(vocab)]
  return(Ranking)
}


# TP = Topic.Modeling(doc,8,5000)
# save(TP,file = "Topic.Modeling.RData")

# load("All.Features.RData")
# Test = Features[1:100]

Ranking = Predict.Rank(Train.Fea = Train.Fea,Test=Test,Topic.Modeling = TP,vocab = vocab)


