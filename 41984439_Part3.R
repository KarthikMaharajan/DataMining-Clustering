kmeansdatatwo <- function(){
    mydata <- read.csv(file="/Users/KarthikMaharajan/Documents/Fall 2015/Introduction to Data Mining/Project 3/Datasets/dataset2.csv", header=TRUE, sep=",")
    data <- mydata[,2:5]
    
    # Calculating the ideal number of clusters
    wss <- (nrow(data)-1)*sum(apply(data,2,var))
    for (i in 2:15) wss[i] <- sum(kmeans(data,
    centers=i)$withinss)
    plot(1:15, wss, type="b", xlab="Number of Clusters",
    ylab="Within groups sum of squares")
    
    kc <- kmeans(data,2)
    
    purity <- function(clusters, classes) {
        sum(apply(table(classes, clusters), 2, max)) / length(clusters)
    }
    library('rgl')
    
    print(paste("Accuracy of the clustering is",purity(kc$cluster, mydata[,6])))
    plot3d(mydata,col=kc$cluster)
}
