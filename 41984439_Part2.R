WeightedAlgorithm <- function(){
mydata <- read.csv(file="/Users/KarthikMaharajan/Documents/Fall 2015/Introduction to Data Mining/Project 3/Datasets/dataset1.csv", header=TRUE, sep=",")
library(flexclust)
mydata <- mydata[,1:3]
library(rgl)
a1 <- function (x, centers) 
{
  if (ncol(x) != ncol(centers)) 
    stop(sQuote("x"), " and ", sQuote("centers"), " must have the same number of columns")
  z <- matrix(0, nrow = nrow(x), ncol = nrow(centers))
  for (k in 1:nrow(centers)) {
    p <- (t(x) - centers[k, ])^2
    p["x",] <- p["x",]*16
    p["y",] <- p["y",]*4
    z[, k] <- sqrt(colSums(p))
  }
  z
}

kc <- kmeans(mydata,8)
kc2 <- kcca(mydata, 8, family=kccaFamily(dist=a1))
plot3d(mydata$x,mydata$y,mydata$z,col=clusters(kc2))
}

KmeansAlgorithm <- function(){
  mydata <- read.csv(file="/Users/KarthikMaharajan/Documents/Fall 2015/Introduction to Data Mining/Project 3/Datasets/dataset1.csv", header=TRUE, sep=",")
  mydata$cluster=NULL
  summary(mydata)
  fit <- kmeans(mydata, 8)
  aggregate(mydata,by=list(fit$cluster),FUN=mean)
  library('rgl')
  plot3d(mydata,col=fit$cluster)
}