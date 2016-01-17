ActualDataPlot <- function(){
library('rgl')
mydata <- read.csv(file="/Users/KarthikMaharajan/Documents/Fall 2015/Introduction to Data Mining/Project 3/Datasets/dataset1.csv", header=TRUE, sep=",")
plot3d(mydata,col=mydata$cluster)
}

KmeansPlot <- function(){
  mydata <- read.csv(file="/Users/KarthikMaharajan/Documents/Fall 2015/Introduction to Data Mining/Project 3/Datasets/dataset1.csv", header=TRUE, sep=",")
  mydata$cluster=NULL
  summary(mydata)
  fit <- kmeans(mydata, 8)
  aggregate(mydata,by=list(fit$cluster),FUN=mean)
  library('rgl')
  plot3d(mydata,col=fit$cluster)
}

DBSCANplot <- function(){
  mydata1 <- read.csv(file="/Users/KarthikMaharajan/Documents/Fall 2015/Introduction to Data Mining/Project 3/Datasets/dataset1.csv", header=TRUE, sep=",")
  mydata1$cluster=NULL
  library('fpc')
  library('rgl')
  d <- dbscan(mydata1,5,MinPts=100,showplot = 1)
  plot3d(mydata1,col=d$cluster+1)
  
}

Graphplot <- function(){
  myData <- read.csv(file="/Users/KarthikMaharajan/Documents/Fall 2015/Introduction to Data Mining/Project 3/Datasets/dataset1.csv", header=TRUE, sep=",")
  library('MCL')
  library('rgl')
  library('fields')
  m <- as.matrix(myData)
  distances <- rdist(m) 
  distancesLogical <- distances < 2
  for(i in 1:nrow(distancesLogical))
  {
    distancesLogical[i,i] <- 0
  }
  distancesLogical <- matrix(as.numeric(distancesLogical), nrow = 1000, ncol = 1000) 
  mcl1 <- mcl(distancesLogical, addLoops=TRUE, ESM=TRUE) 
  plot3d(myData, col=(mcl1$Cluster+1))
}

