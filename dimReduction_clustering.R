#reductionData <- get Features from CleanData
reductionData = hotels[15:54]
reductionResult = prcomp(reductionData, scale = TRUE)
plot(reductionResult, type = "l")
summary(reductionResult)

d <- reductionResult$x[,1:15] # Remove feature which could downgrade results; 15 -> >98%

### K-Means ###
wss <- (nrow(d)-1)*sum(apply(d,2,var))
  for (i in 2:20) wss[i] <- sum(kmeans(d,centers=i)$withinss)
plot(1:20, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")

### KK-Means ###
library("kernlab")
wkk <- (nrow(d)-1)*sum(apply(d,2,var))
  for (i in 2:8) wkk[i] <- sum(withinss(kkmeans(d,centers=i,kernel = "besseldot")))
plot(1:8, wkk, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")

### HClust (only ward.d and ward.d2) ###
clustersD <- hclust(dist(d), method = "ward.D")
clustersD2 <- hclust(dist(d), method = "ward.D2")

plot(clustersD)
abline(h=25, col="red")
abline(h=17, col="red")
plot(clustersD2)
abline(h=17, col="red")


### Add clusters to hotels (only fife clusters) ###
groupsD5 = cutree(clustersD, k=5)
#groupsD3 = cutree(clustersD, k=3)
#groupsD23 = cutree(clustersD2, k=3)
hotels = cbind(hotels, groupsD5)
#hotels = cbind(hotels, groupsD3)
#hotels = cbind(hotels, groupsD23)
