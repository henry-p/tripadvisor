#install.packages("ggmap")
library(ggmap)

meanLat = mean(hotels.total$latitude, na.rm = TRUE)
meanLon = mean(hotels.total$longitude, na.rm = TRUE)

coordinateshotels.total = data.frame(lat = hotels.total[, "latitude"], lon = hotels.total[, "longitude"])
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
# geom_point(data = coordinateshotels.total, color = "cornflowerblue", size = 1.5) +
geom_point(data = coordinateshotels.total, color = kkmeans.result.besseldot[[4]]$membership[-missingCoordsInd], size = 1.5) +
geom_point(data = coordinatesAttractions, color = "darkorange", size = 5, shape = 1)
