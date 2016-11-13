#source("http://bioconductor.org/biocLite.R")
#biocLite("rhdf5")
library(rhdf5)
H5close()
setwd("~/Desktop/Project4_data/data")
Filenames = list.files(path = "~/Desktop/Project4_data/data",all.files = TRUE, recursive = TRUE )

for( i in 1:length(Filenames)){
  
}
sound1<-h5read("TRAAABD128F429CF47.h5", "/analysis")
sound2<-h5read("TRAAAEF128F4273421.h5", "/analysis")



install.packages("DescTools")
