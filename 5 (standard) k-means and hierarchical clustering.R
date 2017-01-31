##### ---------- (STANDARD) K-MEANS AND HIERARCHICAL CLUSTERING ---------- #####
## K-Means
wss <- 0
for (i in 1:20) wss[i] <- sum(kmeans(PCs,centers=i)$withinss)
plot(1:20, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")


## HClust (only ward.d and ward.d2)
clustersD <- hclust(dist(PCs), method = "ward.D")
clustersD2 <- hclust(dist(PCs), method = "ward.D2")

plot(clustersD)
abline(h=25, col="red")
abline(h=17, col="red")
plot(clustersD2)
abline(h=17, col="red")

# Add clusters to hotels (only four clusters)
groupsD4 = cutree(clustersD, k=4)
hotels.hierarchical = cbind(hotels.total, groupsD4)
hotels.clustered = hotels.hierarchical[-c(15:59)]
##### ---------- /(STANDARD) K-MEANS AND HIERARCHICAL CLUSTERING ---------- #####