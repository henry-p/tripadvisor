mappableHotels = hotels.total[-missingCoordsInd, ]

hierarchical.four.clusters = hotels.hierarchical$groupsD4[-missingCoordsInd]
kkmeans.rbfdot.four.clusters = kkmeans.result.rbfdot[[4]]$membership[-missingCoordsInd]
kkmeans.polydot.four.clusters = kkmeans.result.polydot[[4]]$membership[-missingCoordsInd]
kkmeans.vanilladot.four.clusters = kkmeans.result.vanilladot[[4]]$membership[-missingCoordsInd]
kkmeans.laplacedot.four.clusters = kkmeans.result.laplacedot[[4]]$membership[-missingCoordsInd]
kkmeans.besseldot.four.clusters = kkmeans.result.besseldot[[4]]$membership[-missingCoordsInd]
kkmeans.anovadot.four.clusters = kkmeans.result.anovadot[[4]]$membership[-missingCoordsInd]

# getLargestCluster <- function(clusters) {
#   t = as.matrix(table(clusters))
#   row(t)[t == max(t)]
# }

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