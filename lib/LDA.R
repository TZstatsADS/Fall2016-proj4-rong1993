#LDA
setwd("~/Desktop/Project4_data")
load("lyr.Rdata")
library(NLP)
library(tm)
library(lda)
library(LDAvis)
lyr = lyr[,-1]
lyr = lyr[,-c(2,3,6:30)]
vocab = c(names(lyr),"ITSEND")
word.list = NULL
for(i in 1:dim(lyr)[1]){
  print(i)
  word.list[[i]]= rep(colnames(lyr), lyr[i,])
}
get.terms <- function(x) {
  index <- match(x, vocab)
  index <- index[!is.na(index)]
  rbind(as.integer(index), as.integer(rep(1, length(index))))
}

get.counts.matrix<-function(x){
  index <- match(x, vocab)
  index <- index[!is.na(index)]
  temp = rbind(as.integer(index), as.integer(rep(1, length(index))))
  K = table(temp)
  r1 = as.integer(names(K))
  r2 = as.integer(K)
  m = rbind(r1,r2)
  return(m)
}

doc <- lapply(word.list, get.counts.matrix)
save(doc,file = "doc.RData")
save(vocab,file = "vocab.RData")



K <- 5
G <- 10
alpha <- 0.02
eta <- 0.02
library(lda)
set.seed(357)
t1 <- Sys.time()

fit <- lda.collapsed.gibbs.sampler(documents = doc, K = K, vocab = vocab, 
                                   num.iterations = G, alpha = alpha, 
                                   eta = eta, initial = NULL, burnin = 0,
                                   compute.log.likelihood = TRUE)

t2 <- Sys.time()
Topic.Dist = t(fit$document_sums) #Topic Distribution
for(i in 1:dim(Topic.Dist)[1]){
  Topic.Dist[i,] = Topic.Dist[i,]/sum(Topic.Dist[i,])
}
Words.Assign = fit$topics
