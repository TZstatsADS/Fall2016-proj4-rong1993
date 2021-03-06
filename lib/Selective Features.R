#Select Features
load("All.Features.RData")
#load("Test.Features.RData")

New.F = matrix(0,nrow = length(Features),ncol = 53)
for(i in 1:length(Features)){
  print(i)
  x = Features[[i]]
  bar.con.median = median(x$bars_confidence)
  beat.con.median = median(x$beats_confidence)
  #duration = x$songs$duration
  #loudness = median(x$songs$loudness)
  sec.con = median(x$sections_confidence)
  seg.loud.max = c(median(x$segments_loudness_max_time),sd(x$segments_loudness_max_time))
  seg.pitch = c(apply(x$segments_pitches,1,median),c(apply(x$segments_pitches,1,sd)))
  seg.tim = c(apply(x$segments_timbre,1,median),apply(x$segments_timbre,1,sd))
  temp = c(bar.con.median,beat.con.median,sec.con,seg.loud.max,seg.pitch,seg.tim)
  New.F[i,] = temp
}

# Test.Fea = New.F
# save(Test.Fea,file = "Test.Fea.RData")

Train.Fea = New.F
A = New.F

a1 = which(is.na(A[,1]))
a2 = which(is.na(A[,2]))
a3  = which(is.na(A[,3]))
outlier = union(union(a1,a2),a3)

load("doc.RData")
doc = doc[-outlier]
length(doc)
save(doc,file = "doc.Rdata")

# New.F = list(New.F,outlier)
# save(New.F,file = "New.Features.RData")
# load("New.Features.RData")
# A = New.F[[1]]
# outlier = New.F[[2]]
# A = A[-outlier,]
# sum(is.na(A))

# Train.Fea = Train.Fea[,c(-3,-4)]
# save(Train.Fea,file = "New.Features.RData")
# dim(Train.Fea)
