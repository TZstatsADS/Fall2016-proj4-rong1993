#Select Features
load("All.Features.RData")

New.F = matrix(0,nrow = length(Features),ncol = 55)
for(i in 1:length(Features)){
  print(i)
  x = Features[[i]]
  bar.con.median = median(x$bars_confidence)
  beat.con.median = median(x$beats_confidence)
  duration = x$songs$duration
  loudness = median(x$songs$loudness)
  sec.con = median(x$sections_confidence)
  seg.loud.max = c(median(x$segments_loudness_max_time),sd(x$segments_loudness_max_time))
  seg.pitch = c(apply(x$segments_pitches,1,median),c(apply(x$segments_pitches,1,sd)))
  seg.tim = c(apply(x$segments_timbre,1,median),apply(x$segments_timbre,1,sd))
  temp = c(bar.con.median,beat.con.median,duration,loudness,sec.con,seg.loud.max,seg.pitch,seg.tim)
  New.F[i,] = temp
}

a1 = which(is.na(A[,1]))
a2 = which(is.na(A[,2]))
a3  = which(is.na(A[,5]))
outlier = union(union(a1,a2),a3)

New.F = list(New.F,outlier)
save(New.F,file = "New.Features.RData")
load("New.Features.RData")
A = New.F[[1]]
outlier = New.F[[2]]
A = A[-outlier,]
sum(is.na(A))
