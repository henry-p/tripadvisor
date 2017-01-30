d <- reductionResult$x[,1:25] # Remove feature which could downgrade results; 25 -> >98%
e <- as.factor(reductionData)

### K-Means ###
wss <- 0
for (i in 1:20) wss[i] <- sum(kmeans(d,centers=i)$withinss)
plot(1:20, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")

### HClust (only ward.d and ward.d2) ###
clustersD <- hclust(dist(d), method = "ward.D")
clustersD2 <- hclust(dist(d), method = "ward.D2")

plot(clustersD)
abline(h=25, col="red")
abline(h=17, col="red")
plot(clustersD2)
abline(h=17, col="red")

### Add clusters to hotels (only four clusters) ###
groupsD4 = cutree(clustersD, k=4)
hotels.clustered.hier = cbind(hotels.total[-c(15:59)], groupsD4)
hotels.clustered = cbind(hotels.clustered, groupsD4)

