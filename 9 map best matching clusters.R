##### ---------- MAP BEST MATCHING CLUSTERS ---------- #####
library(ggmap)

meanLat = mean(hotels.total$latitude, na.rm = TRUE)
meanLon = mean(hotels.total$longitude, na.rm = TRUE)
gmapsObject <- get_map(location = c(lon = meanLon, lat = meanLat), color = "color", source = "google", maptype = "roadmap", zoom = 12)

getCoordinatesForHotelCluster <- function(mappableHotels, clusters, cluster) {
  indices = which(clusters == cluster)
  hotels = mappableHotels[indices, ]
  data.frame(lat = hotels[, "latitude"], lon = hotels[, "longitude"])
}

plotClustersOnMap <- function(gmapsObject, coord.cl1, coord.cl2, coord.attrac) {
  # gmap <- ggmap(gmapsObject, extent = "panel")
  gmap <- ggmap(gmapsObject, extent = "normal")
  
  gmap + labs(x = 'Longitude', y = 'Latitude') + ggtitle("Tripadvisor Hotels & Attractions") +
    geom_point(data = coord.cl1, color = "red", size = 5, alpha = 0.5) +
    geom_point(data = coord.cl2, color = "blue", size = 5, alpha = 0.5) +
    geom_point(data = coord.attrac, color = "darkorange", size = 5, shape = 1)
}

coordinatesAttractions = data.frame(lat = top3attractions[, "latitude"], lon = top3attractions[, "longitude"])

coordinateshotels.hier.rbfdot.1 = getCoordinatesForHotelCluster(mappableHotels, hierarchical.four.clusters, hier.rbfdot.best.match[1])
coordinateshotels.hier.rbfdot.2 = getCoordinatesForHotelCluster(mappableHotels, kkmeans.rbfdot.four.clusters, hier.rbfdot.best.match[2])

coordinateshotels.hier.polydot.1 = getCoordinatesForHotelCluster(mappableHotels, hierarchical.four.clusters, hier.polydot.best.match[1])
coordinateshotels.hier.polydot.2 = getCoordinatesForHotelCluster(mappableHotels, kkmeans.polydot.four.clusters, hier.polydot.best.match[2])

coordinateshotels.hier.vanilladot.1 = getCoordinatesForHotelCluster(mappableHotels, hierarchical.four.clusters, hier.vanilladot.best.match[1])
coordinateshotels.hier.vanilladot.2 = getCoordinatesForHotelCluster(mappableHotels, kkmeans.vanilladot.four.clusters, hier.vanilladot.best.match[2])

coordinateshotels.hier.laplacedot.1 = getCoordinatesForHotelCluster(mappableHotels, hierarchical.four.clusters, hier.laplacedot.best.match[1])
coordinateshotels.hier.laplacedot.2 = getCoordinatesForHotelCluster(mappableHotels, kkmeans.laplacedot.four.clusters, hier.laplacedot.best.match[2])

coordinateshotels.hier.besseldot.1 = getCoordinatesForHotelCluster(mappableHotels, hierarchical.four.clusters, hier.besseldot.best.match[1])
coordinateshotels.hier.besseldot.2 = getCoordinatesForHotelCluster(mappableHotels, kkmeans.besseldot.four.clusters, hier.besseldot.best.match[2])

coordinateshotels.hier.anovadot.1 = getCoordinatesForHotelCluster(mappableHotels, hierarchical.four.clusters, hier.anovadot.best.match[1])
coordinateshotels.hier.anovadot.2 = getCoordinatesForHotelCluster(mappableHotels, kkmeans.anovadot.four.clusters, hier.anovadot.best.match[2])

pdf(file='cluster_movement.pdf')
  plotClustersOnMap(gmapsObject, coordinateshotels.hier.rbfdot.1, coordinateshotels.hier.rbfdot.2, coordinatesAttractions)
  plotClustersOnMap(gmapsObject, coordinateshotels.hier.polydot.1, coordinateshotels.hier.polydot.2, coordinatesAttractions)
  plotClustersOnMap(gmapsObject, coordinateshotels.hier.vanilladot.1, coordinateshotels.hier.vanilladot.2, coordinatesAttractions)
  plotClustersOnMap(gmapsObject, coordinateshotels.hier.laplacedot.1, coordinateshotels.hier.laplacedot.2, coordinatesAttractions)
  plotClustersOnMap(gmapsObject, coordinateshotels.hier.besseldot.1, coordinateshotels.hier.besseldot.2, coordinatesAttractions)
  plotClustersOnMap(gmapsObject, coordinateshotels.hier.anovadot.1, coordinateshotels.hier.anovadot.2, coordinatesAttractions)
dev.off()
##### ---------- /MAP BEST MATCHING CLUSTERS ---------- #####