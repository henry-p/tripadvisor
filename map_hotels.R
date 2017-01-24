#install.packages("ggmap")
library(ggmap)

meanLat = mean(hotels$latitude, na.rm = TRUE)
meanLon = mean(hotels$longitude, na.rm = TRUE)

coordinatesHotels = na.omit(data.frame(lat = hotels[, "latitude"], lon = hotels[, "longitude"]))
coordinatesAttractions = data.frame(lat = top3attractions[, "latitude"], lon = top3attractions[, "longitude"])

gmapsObject <- get_map(location = c(lon = meanLon, lat = meanLat),
                         color = "color",
                         source = "google",
                         maptype = "roadmap",
                         zoom = 12)

gmap <- ggmap(gmapsObject, extent = "normal")

gmap + labs(x = 'Longitude', y = 'Latitude') + ggtitle("Tripadvisor Hotels & Attractions") +
geom_point(data = coordinatesHotels, color = "cornflowerblue", size = 1.5) +
geom_point(data = coordinatesAttractions, color = "firebrick2", size = 2)
