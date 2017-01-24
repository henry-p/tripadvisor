#reductionData <- get Features from CleanData
reductionData = hotels[15:54]
reductionResult = prcomp(reductionData, scale = TRUE)
plot(reductionResult, type = "l")
summary(reductionResult)

d <- reductionResult$x[,1:21] # Remove feature which could downgrade results; 15 -> >90%
e <- as.factors(reductionData)
### K-Means ###
wss <- 0
  for (i in 1:20) wss[i] <- sum(kmeans(d,centers=i)$withinss)
plot(1:20, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")

### KK-Means ###
#library("kernlab")
#wkk <- 0
#  for (i in 2:8) wkk[i] <- sum(withinss(kkmeans(e,centers=i,kernel = "rbfdot")))
#plot(1:8, wkk, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")

### HClust (only ward.d and ward.d2) ###
clustersD <- hclust(dist(d), method = "ward.D")
clustersD2 <- hclust(dist(d), method = "ward.D2")

plot(clustersD)
abline(h=25, col="red")
abline(h=17, col="red")
plot(clustersD2)
abline(h=17, col="red")

### Add clusters to hotels (only fife clusters) ###
groupsD4 = cutree(clustersD, k=4)
hotels = cbind(hotels, groupsD4)