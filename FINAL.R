##### ---------- LOAD AND CLEAN DATA ---------- #####
file = paste0(dirname(rstudioapi::getActiveDocumentContext()$path), "/tripadvisor-muenster.RData")
load(file)
library(stats)

# Remove useless columns
hotels.total = tripadvisor3$tripadvisor[, -which(names(tripadvisor3$tripadvisor) %in%
                                                   c("hotel_address", "hotel_city", "hotel_region", "hotel_country",
                                                     "rating_excellent", "rating_verygood", "rating_average", "rating_poor", "rating_terrible",
                                                     "rating_location", "rating_sleep", "rating_rooms", "rating_service", "rating_value", "rating_cleanliness",
                                                     "hotel_url"))]

# one hotel appears three times (32,1,11). so lets remove them
hotels.total = hotels.total[-c(1,11),]

# All numeric features
hotels.numeric <- hotels.total[ ,sapply(hotels.total, is.numeric)]

# Remove unusable features in f1_ .. f45_ which are all TRUE or all FALSE
hotels.features = hotels.total[15:59]
del = c()
for (i in 1:45)
  if(sum(hotels.features[i] * 1) == 0 || sum(hotels.features[i] * 1) == length(hotels.features))
    del = c(del, i)
hotels.features = hotels.features[-del]

# Remove NAs
hotels.noNA <- na.omit(hotels.total)
hotels.noNA.location <- na.omit(hotels.total[c(6,7,61:64)])

top3attractions = tripadvisor3$top3attractions
top3attractions = top3attractions[, -4]
##### ---------- /LOAD AND CLEAN DATA ---------- #####



##### ---------- DATA VISUALIZATION EXAMPLE ---------- #####
library(rgl)

x = c(FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, TRUE, TRUE)
y = c(FALSE, FALSE, TRUE, FALSE, TRUE, TRUE, FALSE, TRUE)
z = c(FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE)

rgl::plot3d(x = x, y = y, z = z, size = 10, col = "steelblue", expand=1.2)
##### ---------- /DATA VISUALIZATION EXAMPLE ---------- #####



##### ---------- CORRELATION, REGRESSION, BARPLOT, QQPLOT ANALYSIS ---------- #####

################## WORK IN PROGRESS

##### ---------- /CORRELATION, REGRESSION, BARPLOT, QQPLOT ANALYSIS ---------- #####



##### ---------- DIMENSIONALITY REDUCTION ---------- #####

################## WORK IN PROGRESS

##### ---------- /DIMENSIONALITY REDUCTION ---------- #####



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



##### ---------- MAP HOTELS ---------- #####
#install.packages("ggmap")
library(ggmap)

meanLat = mean(hotels.total$latitude, na.rm = TRUE)
meanLon = mean(hotels.total$longitude, na.rm = TRUE)

coordinateshotels.total = data.frame(lat = hotels.total[, "latitude"], lon = hotels.total[, "longitude"])
# Extract indices of hotels that have missing coordinates
missingCoordsInd = which(apply(coordinateshotels.total, 1, function(row){any(is.na(row))}) == TRUE)
coordinateshotels.total = na.omit(coordinateshotels.total)
coordinatesAttractions = data.frame(lat = top3attractions[, "latitude"], lon = top3attractions[, "longitude"])

gmapsObject <- get_map(location = c(lon = meanLon, lat = meanLat),
                       color = "color",
                       source = "google",
                       maptype = "roadmap",
                       zoom = 12)

gmap <- ggmap(gmapsObject, extent = "panel")
# gmap <- ggmap(gmapsObject, extent = "normal")

gmap + labs(x = 'Longitude', y = 'Latitude') + ggtitle("Tripadvisor Hotels & Attractions") +
  geom_point(data = coordinateshotels.total, color = "cornflowerblue", size = 1.5) +
  geom_point(data = coordinatesAttractions, color = "darkorange", size = 5, shape = 1)
##### ---------- /MAP HOTELS ---------- #####



##### ---------- KERNEL K-MEANS ---------- #####
library(kernlab)

# Save binary feature values from hotels as a numeric [0, 1] matrix
m = as.matrix(hotels.features) * 1
maxs = apply(m, 2, max)
mins = apply(m, 2, min)
m = scale(m, center = mins, scale = maxs - mins)

# Define the maximum number of clusters that kkmeans runs on
nrClusters = 15

# Calculate Kernel k-Means, using data [x], kernel function [kernel], for all clusters from 1 to [nrClusters].
# Save result in a list.
kkmeansWrapper = function(x, kernel, nrClusters) {
  result = 0
  for (i in seq_len(nrClusters)) {
    kkm = NA
    while(is.na(kkm)) {
      tryCatch({
        kkm = kkmeans(m, centers = i, kernel = kernel)
      }, error=function(e){})
    }
    result[i] = list(list(membership = kkm@.Data, centers = kkm@centers, withinss = kkm@withinss))
  }
  return(result)
}

# Extract the total within-sum-of-squares from kkmeans result-object, that contains results for all clusters from 1 to [nrClusters]
kkmeansTWSS = function(kkmeans.result) {
  sapply(kkmeans.result, function(kkm) {sum(kkm$withinss)})
}

# Save kkmeans results with different kernel functions
# (Commented out a few calls because those take a long time to compute)
kkmeans.result.rbfdot = kkmeansWrapper(m, "rbfdot", nrClusters)
kkmeans.result.polydot = kkmeansWrapper(m, "polydot", nrClusters)
kkmeans.result.vanilladot = kkmeansWrapper(m, "vanilladot", nrClusters)
# kkmeans.result.tanhdot = kkmeansWrapper(m, "tanhdot", nrClusters)
kkmeans.result.laplacedot = kkmeansWrapper(m, "laplacedot", nrClusters)
kkmeans.result.besseldot = kkmeansWrapper(m, "besseldot", nrClusters)
kkmeans.result.anovadot = kkmeansWrapper(m, "anovadot", nrClusters)
# kkmeans.result.splinedot = kkmeansWrapper(m, "splinedot", nrClusters)
# kkmeans.result.stringdot = kkmeansWrapper(m, "stringdot", nrClusters)

# Plot the total-within-sum-of-squares metric against the corresponding number of clusters
pdf(file='plots.pdf')
  plot(seq_len(nrClusters), kkmeansTWSS(kkmeans.result.rbfdot), type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")
  plot(seq_len(nrClusters), kkmeansTWSS(kkmeans.result.polydot), type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")
  plot(seq_len(nrClusters), kkmeansTWSS(kkmeans.result.vanilladot), type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")
  # plot(seq_len(nrClusters), kkmeansTWSS(kkmeans.result.tanhdot), type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")
  plot(seq_len(nrClusters), kkmeansTWSS(kkmeans.result.laplacedot), type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")
  plot(seq_len(nrClusters), kkmeansTWSS(kkmeans.result.besseldot), type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")
  plot(seq_len(nrClusters), kkmeansTWSS(kkmeans.result.anovadot), type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")
  # plot(seq_len(nrClusters), kkmeansTWSS(kkmeans.result.splinedot), type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")
  # plot(seq_len(nrClusters), kkmeansTWSS(kkmeans.result.stringdot), type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")
dev.off()
##### ---------- /KERNEL K-MEANS ---------- #####



##### ---------- COMPARE CLUSTERING APPROACHES ---------- #####
mappableHotels = hotels.total[-missingCoordsInd, ]

hierarchical.four.clusters = hotels.hierarchical$groupsD4[-missingCoordsInd]
kkmeans.rbfdot.four.clusters = kkmeans.result.rbfdot[[4]]$membership[-missingCoordsInd]
kkmeans.polydot.four.clusters = kkmeans.result.polydot[[4]]$membership[-missingCoordsInd]
kkmeans.vanilladot.four.clusters = kkmeans.result.vanilladot[[4]]$membership[-missingCoordsInd]
kkmeans.laplacedot.four.clusters = kkmeans.result.laplacedot[[4]]$membership[-missingCoordsInd]
kkmeans.besseldot.four.clusters = kkmeans.result.besseldot[[4]]$membership[-missingCoordsInd]
kkmeans.anovadot.four.clusters = kkmeans.result.anovadot[[4]]$membership[-missingCoordsInd]

getLargestCluster <- function(clusters) {
  t = as.matrix(table(clusters))
  row(t)[t == max(t)]
}

getHotelsInCluster <- function(hotels, clusters, cluster) {
  hotels[which(clusters == cluster), ]
}

matchHotelsInCluster <- function(hotels1, hotels2) {
  m = max(nrow(hotels1), nrow(hotels2))
  sum(is.element(row.names(hotels1), row.names(hotels2))) / m
}

getBestMatchCluster <- function(hotels, maxCluster, clusters1, clusters2) {
  currentMatch = 0
  bestClusterPair = NA
  for (i in seq_len(maxCluster)) {
    h1 = getHotelsInCluster(hotels, clusters1, i)
    for (j in seq_len(maxCluster)) {
      h2 = getHotelsInCluster(hotels, clusters2, j)
      match = matchHotelsInCluster(h1, h2)
      if(match > currentMatch) {
        currentMatch = match
        bestClusterPair = c(i, j)
      }
    }
  }
  return(bestClusterPair)
}

hier.rbfdot.best.match = getBestMatchCluster(mappableHotels,
                                             maxCluster = 4,
                                             clusters1 = hierarchical.four.clusters,
                                             clusters2 = kkmeans.rbfdot.four.clusters)

hier.polydot.best.match = getBestMatchCluster(mappableHotels,
                                              maxCluster = 4,
                                              clusters1 = hierarchical.four.clusters,
                                              clusters2 = kkmeans.polydot.four.clusters)

hier.vanilladot.best.match = getBestMatchCluster(mappableHotels,
                                                 maxCluster = 4,
                                                 clusters1 = hierarchical.four.clusters,
                                                 clusters2 = kkmeans.vanilladot.four.clusters)

hier.laplacedot.best.match = getBestMatchCluster(mappableHotels,
                                                 maxCluster = 4,
                                                 clusters1 = hierarchical.four.clusters,
                                                 clusters2 = kkmeans.laplacedot.four.clusters)

hier.besseldot.best.match = getBestMatchCluster(mappableHotels,
                                                maxCluster = 4,
                                                clusters1 = hierarchical.four.clusters,
                                                clusters2 = kkmeans.besseldot.four.clusters)
# -> [1] 4 2
# Check: matchHotelsInCluster(getHotelsInCluster(mappableHotels, hierarchical.four.clusters, cluster = 4),
#                             getHotelsInCluster(mappableHotels, kkmeans.besseldot.four.clusters, cluster = 2))
# -> 0.8571429, indeed high "match" value

hier.anovadot.best.match = getBestMatchCluster(mappableHotels,
                                               maxCluster = 4,
                                               clusters1 = hierarchical.four.clusters,
                                               clusters2 = kkmeans.anovadot.four.clusters)
##### ---------- /COMPARE CLUSTERING APPROACHES ---------- #####



