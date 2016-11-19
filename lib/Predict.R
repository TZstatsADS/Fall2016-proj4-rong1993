load("doc.RData")
load("lyr.RData")
load("New.Features.RData")
load("Topic.Modeling.RData")
load("vocab.RData")
source("Code/Main.R")
Predict.Rank<-function(Train.Fea,Test.Fea,Topic.Modeling,vocab){#Test is a list with all features
  Topic.Dist = TP$Topic.Dist
  Words.Topic = TP$Words.Topic
  Ranking = Predict(Train.Fea,Test.Fea,Topic.Dist,Words.Topic,PCA = 0.99,treshold=0.1)
  colnames(Ranking) = vocab[-length(vocab)]
  return(Ranking)
}


# TP = Topic.Modeling(doc,8,5000)
# save(TP,file = "Topic.Modeling.RData")

# load("All.Features.RData")
# Test = Features[1:100]
load("Test.Fea.RData")

# 
# TP = Topic.Modeling(doc,8,5000)
# save(TP,file = "Topic.Modeling.RData")

Ranking = Predict.Rank(Train.Fea = Train.Fea,Test.Fea,Topic.Modeling = TP,vocab = vocab)
write.csv(Ranking,"Ranking.csv")
