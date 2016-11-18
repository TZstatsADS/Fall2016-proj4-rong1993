#------Main Code--------#

Topic.Modeling<-function(lyr.doc,K,G){
  library(lda)
  alpha <- 0.02
  eta <- 0.02
  t1 <- Sys.time()
  fit <- lda.collapsed.gibbs.sampler(documents = lyr.doc, K = K, vocab = vocab, 
                                     num.iterations = G, alpha = alpha, 
                                     eta = eta, initial = NULL, burnin = 0,
                                     compute.log.likelihood = TRUE)
  t2<- Sys.time()
  Topic.Dist = t(fit$document_sums) #Topic Distribution
  for(i in 1:dim(Topic.Dist)[1]){
    Topic.Dist[i,] = Topic.Dist[i,]/sum(Topic.Dist[i,])
  }
  return(list(Topic.Dist = Topic.Dist, Words.Topic = fit$topics))
}





Predict<-function(Train.Fea,Test.Fea,Topic.Dist,Words.Topic,PCA,treshold){ 
  #Do clustering using Train.Fea and One Test.Fea at a time
  #Loop to Get the Prediciton
  ntest = dim(Test.Fea)[1]
  ntrain = dim(Train.Fea)[1]
  ncluster = dim(Topic.Dist)[2]
  Test.Dist = matrix(0,nrow = ntest, ncol = ncluster)
  print(paste0("testing"))
  Features = rbind(Train.Fea,Test.Fea) #ntrain,ntest
  PCA.F = prcomp(t(Features),scale. = TRUE,center = TRUE)
  print("Get PCA")
  PCA.S = summary(PCA.F)
  PCA.index = which(PCA.S$importance[3,]>PCA)[1] #PCA - explanation
  Data =  as.data.frame(PCA.S$rotation[,1:PCA.index])
  Prediction = matrix(0,nrow = ntest,ncol = ncluster)
    # for(K in 1:ncluster){
    #   print(K)
    #   Group.Data = data.frame(Data[1:ntrain,],y = Topic.Dist[,K])
    #   model <- glm (y~ ., data = Group.Data)
    #   Temp = Data[(ntrain+1):(ntrain+ntest),]
    #   Prediction[,K] = predict(model,as.data.frame(Temp),type="response")
    # }#cluster
  #------------Convert Distribution into Categories-------#
    #----Using Logistic Regression----------#
  Topic.Dist[Topic.Dist>treshold]=1
  Topic.Dist[Topic.Dist<=treshold]=0
  for(K in 1:ncluster){
      print(K)
      Group.Data = data.frame(Data[1:ntrain,],y = Topic.Dist[,K])
      model <- glm (y~ ., data = Group.Data,family = binomial,control = list(maxit = 500))
      Temp = Data[(ntrain+1):(ntrain+ntest),]
      Prediction[,K] = predict(model,as.data.frame(Temp),type="response")
    #-----Using SVM-------#
      library(e1071)
      #tune<-tune.svm(y~.,data=Group.Data,gamma=10^(-3:-1),cost=10^(-1:4))  
      #a = summary(tune)$best.parameters
      # model <- svm(y~., data=Group.Data, method="C-classification", kernel="radial", probability=T, 
      #              gamma=0.01, cost = 1000)
      # Temp = Data[(ntrain+1):(ntrain+ntest),]
      # Prediction[,K] <- predict(model, Temp, probability=T)
    }
  for(i in 1:ntest){
  Test.Dist[i,] = Prediction[i,]/sum(Prediction[i,])
  } 
  #--Ranking
  Ranking = matrix(0,nrow = ntest,ncol = (length(vocab)-1))
  for(i in 1:dim(Test.Dist)[1]){
    x = Test.Dist[i,]
    Frequency = x%*%Words.Topic
    Frequency = Frequency[,-1]
    Ranking[i,] =  order(Frequency,decreasing = TRUE )
  }
  for(i in 1:dim(Ranking)[1]){
  }
  return(Ranking)
} #Function




Score<-function(Ranking,T.Lyr){
  Scores = numeric(dim(Ranking)[1])
  Top = Scores
  for(i in 1:dim(Ranking)[1]){
  x = T.Lyr[i,]
  Appear = which(x!=0)
  Total.Ranking  = sum(Ranking[i,Appear])
  m = length(Appear)
  rmean = mean(Ranking[i,])
  Scores[i] = 1/(m*rmean)*Total.Ranking
  Top[i] = Total.Ranking/m
  }
  return(Top)
}


Top.Rank<-function(Ranking,T.Lyr){
  rate = NULL
  for(i in 1:dim(Ranking)[1]){
  x = T.Lyr[i,]
  Appear = which(x!=0)
  t = min(length(Appear),100)
  y = Ranking[i,1:t]
  rate[i] = length(which(Appear%in%y))/t
  }
  return(rate)
}



Get.Features<-function(x){
bar.con.median = median(x$bars_confidence)
beat.con.median = median(x$beats_confidence)
duration = x$songs$duration
loudness = median(x$songs$loudness)
sec.con = median(x$sections_confidence)
seg.loud.max = c(median(x$segments_loudness_max_time),sd(x$segments_loudness_max_time))
seg.pitch = c(apply(x$segments_pitches,1,median),c(apply(x$segments_pitches,1,sd)))
seg.tim = c(apply(x$segments_timbre,1,median),apply(x$segments_timbre,1,sd))
temp = c(bar.con.median,beat.con.median,duration,loudness,sec.con,seg.loud.max,seg.pitch,seg.tim)
return(temp)
}



# 
# A = Topic.Modeling(doc,5,50)
# load("Cluster = 5.RData")
# load("Features.RData")
# load("vocab.RData")
# load("lyr.RData")
# lyr = lyr[,-1]
# lyr = lyr[,-c(2,3,6:30)]
# #save(A,file = "Cluster = 5.RData")
# Topic.Dist = A$Topic.Dist
# Words.Topic = A$Words.Topic
# 
# 
# Data = cbind(Beat.Con,Beat.Start,Bar.Con,Bar.Start)#,Seg.Loud.Time,Seg.Pit,Seg.Tim)
# n = dim(Data)[1]
# train.index = sample(1:n,2300)
# Train.Fea = Data[train.index,]
# Test.Fea = Data[-train.index,]
# Topic.Dist = Topic.Dist[train.index,]
# T.Lyr = lyr[-train.index,]
# PCA = 0.85
# Ranking = Predict(Train.Fea,Test.Fea,Topic.Dist,Words.Topic,PCA)
# Scores = Score(Ranking,T.Lyr)
# mean(Scores)
