setwd("~/Desktop/Project4_data")
#----All Features --- #
#Songs
#Seg.Con
#Seg.Loud.Max
#Seg.Loud.Time
#Seg.Loud.Start
#Seg.Start
#Seg.Pit
#Seg.Tim
#Beat.Con
#Beat.Start
#Bar.Con
#Bar.Start
Songs = NULL
for( i in 1 : length(Features)){
  Songs = rbind(Songs,as.vector(Features[[i]]$songs))
}
Songs = Songs[,c(-2,-31)]

Trim<-function(x,n){
  if(length(x)==0){
    d = rep(0,n)
    return(d)
  }
  if(length(x)<n){
    k = n-length(x)
    if(k<length(x)){
    d = c(x,x[1:k])
    }else{
      t = floor(k/length(x))
      temp = x
      for(ii in 1:t){
        temp = c(temp,x)
      }
      if((k - t*length(x))!=0) temp = c(temp,x[1:(k-t*length(x))])
      d = temp
    }
  }else{
    d = x[1:n]
  }
  return(d)
}

Trim.Matrix<-function(M,row,col){
  ro = dim(M)[1]
  co = dim(M)[2]
  if(ro<row){
    dr = row - ro
    if(dr<ro){
      M = rbind(M,M[1:dr,])
    }else{
      t = floor(dr/ro)
      temp = M
      for(l in 1:t){
        print(l)
        temp = rbind(temp,M)
      }
      if((dr-t*ro)>0) temp = rbind(temp,M[1:(dr-t*ro),])
    }
    M = temp
  }else{
    M = M[1:row,]
  }
  if(co<col){
    dc = col - co
    if(dc<co){
      M = cbind(M,M[,1:dc])
    }else{
      t = floor(dc/co)
      temp = M
      for(l in 1:t){
        print(paste0("l = ",l))
        temp = cbind(temp,M)
      }
      if((dc-t*co)>0) temp = cbind(temp,M[,1:(dc-t*co)])
      M = temp
    }
  }else{
    M = M[,1:col]
  }
  return(M)
}
Seg.Con = NULL
for( i in 1 : length(Features)){
  print(i)
  temp = Features[[i]]$segments_confidence
  #Seg.Con[i] = length(temp)
  Seg.Con = rbind(Seg.Con,Trim(temp,n=1000))
}
Seg.Loud.Max = NULL
for( i in 1: length(Features)){
  print(i)
  temp = Features[[i]]$segments_loudness_max
  Seg.Loud.Max = rbind(Seg.Loud.Max,Trim(temp,n=1000))
}
Seg.Loud.Time = NULL
for( i in 1 : length(Features)){
  temp = Features[[i]]$segments_loudness_max_time
  #Seg.Con[i] = length(temp)
  Seg.Loud.Time = rbind(Seg.Loud.Time,Trim(temp,n=1000))
}
Seg.Loud.Start = NULL
for( i in 1 : length(Features)){
  temp = Features[[i]]$segments_loudness_start
  #Seg.Con[i] = length(temp)
  Seg.Loud.Start = rbind(Seg.Loud.Start,Trim(temp,n=1000))
}
Seg.Start = NULL
for( i in 1 : length(Features)){
  temp = Features[[i]]$segments_start
  #Seg.Con[i] = length(temp)
  Seg.Start = rbind(Seg.Start,Trim(temp,n=1000))
}
Seg.Pit = NULL
for( i in 1 : length(Features)){
  print(i)
  temp = Trim.Matrix(Features[[i]]$segments_pitches,row=12,col=1000)
  Seg.Pit = rbind(Seg.Pit,as.vector(temp))
}
Seg.Tim = NULL
for( i in 1 : length(Features)){
  print(i)
  temp = Trim.Matrix(Features[[i]]$segments_timbre,row=12,col=1000)
  Seg.Tim = rbind(Seg.Tim,as.vector(temp))
}
Bar.Con = NULL
for( i in 1 : length(Features)){
  print(i)
  temp = Features[[i]]$bars_confidence
  #Bar.Con[i] = length(temp)
  Bar.Con = rbind(Bar.Con,Trim(temp,n=200))
}
Bar.Start = NULL
for( i in 1 : length(Features)){
  temp = Features[[i]]$bars_start
  #Bar.Start[i] = length(temp)
  Bar.Start = rbind(Bar.Start,Trim(temp,n=200))
}
Beat.Con = NULL
for( i in 1 : length(Features)){
  temp = Features[[i]]$beats_confidence
  #Beat.Con[i] = length(temp)
  Beat.Con = rbind(Beat.Con,Trim(temp,n=500))
}
Beat.Start = NULL
for( i in 1 : length(Features)){
  temp = Features[[i]]$beats_start
  #Beat.Start[i] = length(temp)
  Beat.Start = rbind(Beat.Start,Trim(temp,n=500))
}









# All.Features = data.frame(Songs,Seg.Con,Seg.Loud.Max,Seg.Loud.Time,Seg.Loud.Start,
#                       Seg.Start,Seg.Pit,Seg.Tim,Beat.Con,Beat.Start,Bar.Con,Bar.Start)
# 
# save(All.Features,file = "All.Features.RData")
# 
# k = 1
# na.index = NULL
# for(i in 1:dim(All.Features)[2]){
#   print(i)
#   a = All.Features[,i]
#   t = sum(is.na(a))
#   if(t>0){
#     na.index[k] = i
#     k = k+1
#   }
# }
# Na.Omit.Features = All.Features[,-na.index]
# save(Na.Omit.Features,file = "Na.Omit.Features.RData")

# #---Random Forest-----#
# library(randomForest)
# model<-randomForest(Group~.,data=Train.Data,nTree= 500)
# prediction<-predict(model,newdata = Test.Data,type='class')
# pre = round(prediction)
# table(pre,Test.Data$Group)
# importance(model)
# 
# #---Clustering---#
# 
# as.numeric(runif(20)>0.5)


