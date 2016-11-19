setwd("~/Desktop/Project4_data")
#PCA



Data = cbind(Bar.Con,Bar.Start,Beat.Con,Beat.Start)


#---Use PCA to reduce dimensions---#
PCA = prcomp(t(Data))
A = summary(PCA)
index = which(A$importance[3,]>0.9)[1]
New.Data = as.data.frame(A$rotation[,1:index])

ngroup = dim(Topic.Dist)[2]
Prob = numeric(ngroup)
for(i in 1:ngroup){
   Group.Data = data.frame(New.Data,y = Topic.Dist[,i])
   model <- glm (y~ ., data = Group.Data)
   Prob[i] = predict(model,newdata,type="response")
}


