#source("http://bioconductor.org/biocLite.R")
#biocLite("rhdf5")
library(rhdf5)
library(stringr)
H5close()
setwd("~/Desktop/Project4_data")
load("lyr.RData")
setwd("~/Desktop/Project4_data/data")
Names = lyr[,1]
Filenames = list.files(path = "~/Desktop/Project4_data/data",all.files = TRUE, recursive = TRUE )
Features = list(0)


for( i in 1:length(Names)){
    index = grep(Names[i],Filenames)
    Features[[i]]<-h5read(Filenames[index],"/analysis")
    print(i)
}

save(Features,file= "All.Features.RData")




