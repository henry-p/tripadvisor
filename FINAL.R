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

##### ---------- HOTELS ON THE MAP VISUALIZATIONS ---------- #####

#install.packages("ggmap")
library(ggmap)

meanLat = mean(hotels.total$latitude, na.rm = TRUE)
meanLon = mean(hotels.total$longitude, na.rm = TRUE)

coordinatesHotels = data.frame(lat = hotels.total[, "latitude"], lon = hotels.total[, "longitude"])
coordinatesAttractions = data.frame(lat = tripadvisor3$top3attractions[, "latitude"], lon = tripadvisor3$top3attractions[, "longitude"])

gmapsObject <- get_map(location = c(lon = meanLon, lat = meanLat),
                       color = "color",
                       source = "google",
                       maptype = "roadmap",
                       zoom = 12)

gmap <- ggmap(gmapsObject, extent = "panel")

#initial presentation of position of hotels and attractions
gmap + labs(x = 'Longitude', y = 'Latitude') + ggtitle("Tripadvisor Hotels & Attractions") +
  geom_point(data = coordinatesHotels, color = "cornflowerblue", size = 2) + 
  labs(title = "M?nster", x = "Longitude", y = "Latitude") +
  geom_point(data = coordinatesAttractions, color = "firebrick2", size = 2)

#categorize the price
pricerange = ifelse(hotels.total$hotel_price <93, "1", ifelse(hotels.total$hotel_price >= 93 & hotels.total$hotel_price < 116, "2", ifelse(hotels.total$hotel_price > 140, "3", "4")))

#rating and price on the map
gmap + labs(x = 'Longitude', y = 'Latitude') + ggtitle("Tripadvisor Hotels & Attractions") +
  geom_point(data = coordinatesHotels, aes(color = factor(pricerange), size = hotels.total$rating_total)) + 
  labs(title = "M?nster", x = "Longitude", y = "Latitude", color = "Price", size = "Rating") +
  scale_color_discrete(labels = c("Budget <93 EUR", "Medium 93 - 116 EUR", "Luxury > 116 EUR")) +
  geom_point(data = coordinatesAttractions, color = "firebrick2", size = 2)


#distance to center categories
distcat = ifelse(hotels.total$distance_to_city_center < 1, "1", ifelse(hotels.total$distance_to_city_center >= 1 & hotels.total$distance_to_city_center < 3, "2", ifelse(hotels.total$distance_to_city_center > 7, "4", "3")))

#distance from center with City Ranking
gmap + labs(x = 'Longitude', y = 'Latitude') + ggtitle("Tripadvisor Hotels & Attractions") +
  geom_point(data = coordinatesHotels, aes(color = factor(distcat), size = hotels.total$city_ranking)) + 
  labs(title = "M?nster", x = "Longitude", y = "Latitude", color = "Location", size = "City Ranking") +
  scale_color_discrete(labels = c("Central", "Good Location", "Far from center","Outside of city")) +
  geom_point(data = coordinatesAttractions, color = "firebrick2", size = 2)

#stars and count ratings
gmap + labs(x = 'Longitude', y = 'Latitude') + ggtitle("Tripadvisor Hotels & Attractions") +
  geom_point(data = coordinatesHotels, aes(color = factor(hotels.total$hotel_stars), size = hotels.total$count_ratings_total)) + 
  labs(title = "M?nster", x = "Longitude", y = "Latitude", color = "Stars", size = "Count Rating") +
  geom_point(data = coordinatesAttractions, color = "firebrick2", size = 2)

#stars and number of rooms
gmap + labs(x = 'Longitude', y = 'Latitude') + ggtitle("Tripadvisor Hotels & Attractions") +
  geom_point(data = coordinatesHotels, aes(color = factor(hotels.total$hotel_stars), size = hotels.total$number_of_rooms)) + 
  labs(title = "M?nster", x = "Longitude", y = "Latitude", color = "Stars", size = "Number of Rooms") +
  geom_point(data = coordinatesAttractions, color = "firebrick2", size = 2)


##### ---------- /HOTELS ON THE MAP VISUALIZATIONS ---------- #####


##### ---------- DISTRIBUTIONS AND NORMALITY TEST ---------- #####

checkdistr = hotels.total[c(3,4,8:14,60, 64)]

layout(matrix(1:6, nrow = 2, ncol = 3))

# Kernel Density Plot for continuous data and Box plot for categorical data

d1 <- density(na.omit(checkdistr$hotel_price)) # returns the density data
hist(checkdistr$hotel_price, col="green", main = "Price",  freq = FALSE, xlab = "Price")
polygon(d1, border="blue")

d2 <- density(na.omit(checkdistr$number_of_rooms)) # returns the density data
hist(checkdistr$number_of_rooms, col="green", main = "Rooms", freq = FALSE, xlab = "Rooms")
polygon(d2, border = "blue")

d4 <- density(na.omit(checkdistr$city_ranking)) # returns the density data
hist(checkdistr$city_ranking, col="green", main = "City Rank", freq = FALSE, xlab = "City Ranking")
polygon(d4, border="blue")

d5 <- density(na.omit(checkdistr$distance_to_city_center)) # returns the density data
hist(checkdistr$distance_to_city_center, col="green", main = "Distance to Center", freq = FALSE, xlab = "Distance in km")
polygon(d5, border="blue")

boxplot(checkdistr$rating_total, col="green", main = "Rating", freq = FALSE, ylab = "Rating")

boxplot(checkdistr$hotel_stars, col="green", main = "Stars", freq = FALSE, ylab = "Stars")

#tests for normality

nb.data <- hotels.noNA[c(4:5, 9:13, 60:64)]
layout(matrix(1:12, ncol = 4, nrow = 3))

sapply(colnames(nb.data), function(x){
  qqnorm(nb.data[[x]], main = x, pch = 19, cex.lab = 1, cex.main = 1, ylab = "")
  qqline(nb.data[[x]], lwd = 0.6, col = "red")
})

#qqplot for log normal distribution
nb.data.log <- log(nb.data + 1)
layout(matrix(1:12, ncol = 4, nrow = 3))

sapply(colnames(nb.data.log), function(x){
  qqnorm(nb.data.log[[x]], main = x, pch = 19, cex.lab = 1, cex.main = 1, ylab = "")
  qqline(nb.data.log[[x]], lwd = 0.6, col = "red")
})

#qqplot for square distribution
nb.data.sqrt <- sqrt(nb.data)
layout(matrix(1:12, ncol = 4, nrow = 3))

sapply(colnames(nb.data.sqrt), function(x){
  qqnorm(nb.data.sqrt[[x]], main = x, pch = 19, cex.lab = 1, cex.main = 1, ylab = "")
  qqline(nb.data.sqrt[[x]], lwd = 0.6, col = "red")
})

#shapro-wilk test

shapiro.test(nb.data$count_ratings_total)
shapiro.test(nb.data$count_ratings_family)
shapiro.test(nb.data$count_ratings_couple)
shapiro.test(nb.data$count_ratings_single)
shapiro.test(nb.data$count_ratings_business)
shapiro.test(nb.data$foto_count)
shapiro.test(nb.data$number_of_rooms)
shapiro.test(nb.data$hotel_price)
shapiro.test(nb.data$distance_to_sight1)
shapiro.test(nb.data$distance_to_sight2)
shapiro.test(nb.data$distance_to_sight3)
shapiro.test(nb.data$distance_to_city_center)
sapply(colnames(nb.data), function(x){
  shapiro.test(as.numeric(nb.data[[x]]))
})
layout(matrix(1:12, ncol=4,nrow=3))
sapply(colnames(nb.data), function(x){
  hist(as.numeric(nb.data[[x]]),main=x,xlab=round(shapiro.test(nb.data[[x]])$p.value,5),
       col="green",cex.main=2)
})

##Anderson-Darling test

# install.packages("nortest")
library(nortest)
ad.test(nb.data$count_ratings_total)
ad.test(nb.data$count_ratings_family)
ad.test(nb.data$count_ratings_couple)
ad.test(nb.data$count_ratings_single)
ad.test(nb.data$count_ratings_business)
ad.test(nb.data$foto_count)
ad.test(nb.data$number_of_rooms)
ad.test(nb.data$hotel_price)
ad.test(nb.data$distance_to_sight1)
ad.test(nb.data$distance_to_sight2)
ad.test(nb.data$distance_to_sight3)
ad.test(nb.data$distance_to_city_center)
sapply(colnames(nb.data), function(x){
  ad.test(as.numeric(nb.data[[x]]))
})
layout(matrix(1:12, ncol=4,nrow=3))
sapply(colnames(nb.data), function(x){
  hist(as.numeric(nb.data[[x]]),main=x,xlab=round(ad.test(nb.data[[x]])$p.value,5),
       col="green",cex.main=2)
})


##### ---------- /DISTRIBUTIONS AND NORMALITY TEST ---------- #####

##### ---------- CORRELATION, REGRESSION, BARPLOT, QQPLOT ANALYSIS ---------- #####
## correlation
library(corrplot)

# correlation (non-binary data)
p.cor <- cor(hotels.noNA[c(60, 3:5, 8, 13, 64)])
par(mfrow = c(1,1))
corrplot(p.cor, method = "shade")

# correlation (non-binary + "count.features")
count.features <- rowSums( hotels.noNA[,15:59] )
hotels.noNA.f <- cbind(hotels.noNA, count.features)
f.cor <- cor(hotels.noNA.f[c(3:5, 8:14,60:65)])
corrplot(f.cor, method = "shade")

## Regression
#regression count.ratings.total & binary features
hotels.features.lm = hotels.total[,c(4,15:59)]
features.lm.noNA =na.omit(hotels.features.lm)

regression.countings <- lm(count_ratings_total ~.
                           , data = features.lm.noNA
)

summary(regression.countings)
coefficients(regression.countings) # model coefficients
anova(regression.countings)

#regression countings with selected features

regression.countings.sf <- lm(count_ratings_total ~
                                f10_Concierge
                              +f16_HotTub
                              + f17_Golfcourse
                              + f18_Suites
                              + f21_Kitchenette
                              + f27_ShuttleBusService
                              + f30_BanquetRoom
                              + f31_Self.ServeLaundry
                              + f33_ConferenceFacilities
                              + f36_Non.SmokingHotel
                              , data = features.lm.noNA
)
summary(regression.countings.sf)


## Bar chart

# stacked bar chart with the number of reviewers by type

reviewer.type <- hotels.noNA[,c(9:12)]
reviewer.type.T <- t(reviewer.type)
colnames(reviewer.type.T) <- hotels.noNA[,1]
png("~/Desktop/type.png", width = 1000, height = 600)
par(mar=c(5,12,4,2)+0.1,mgp=c(3,1,0))
bp <- barplot(as.matrix(reviewer.type.T), main="# of reviewers by type", 
              las=1, horiz = TRUE, col=c("grey70","darkred","grey78","rosybrown1"), 
              cex.names = 0.8,cex.axis=0.8)
legend("topright", legend = colnames(reviewer.type), 
       fill = c("grey70","darkred","grey78","rosybrown1"))
dev.off()

# bar chart for the number of features
count.features.a <- rowSums(hotels.total[,15:54])
cf.data <- data.frame(hotels.total$hotel_name, count.features.a)
cf.ordered <- cf.data[order(cf.data[,2],decreasing=TRUE),]
png("~/Desktop/count_features.png", width = 1000, height = 700)
par(mar=c(7,10,4,2)+3, mgp=c(4,0.8,0))
cf.bp <-barplot(cf.ordered[,2], names.arg = cf.ordered[,c(1)], horiz= T, las=1, xlab = "The number of features in hotel",  
                cex.names = 0.7,cex.axis=0.7)
dev.off()



## pairsplot

# pairsplot with non-binary variables (selected)

panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col=rgb(82,125,162, maxColorValue = 255))
}

panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r * 5)
}

pairs(hotels.noNA[c(3:5,8:13,60)], cex = 0.4, pch = 21, col = "dodgerblue3", bg = "azure", diag.panel = panel.hist, upper.panel = panel.cor)
##### ---------- /CORRELATION, REGRESSION, BARPLOT, QQPLOT ANALYSIS ---------- #####



##### ---------- DIMENSIONALITY REDUCTION ---------- #####
# install.packages("randomForest")
# install.packages("rgl")
# install.packages("gplots")
library(randomForest)
library(rgl)
library(gplots)
# reductionData <- get Features from CleanData
features.scaled <- as.data.frame(apply(hotels.features *1, 2, scale))
reductionResult = prcomp(features.scaled)
var <- reductionResult$sdev^2/sum(reductionResult$sdev^2)
barplot(var, xlab = "PCs", names.arg = c(1:40), ylab = "Variance")
plot(cumsum(var), xlab = "PCs", ylab = "cum. Variance", ylim = c(0,1))
abline(h=0.75, col="red", lty = 2)
abline(v=10, col="red", lty = 2)
abline(h=0.9, col="blue", lty = 2)
abline(v=17, col="blue", lty = 2)
abline(h=0.95, col="green", lty = 2)
abline(v=21, col="green", lty = 2)
abline(h=0.99, col="orange", lty = 2)
abline(v=28, col="orange", lty = 2)
summary(reductionResult)

# BiPlot
biplot(reductionResult)
# 3D Biplot
pcX <- apply(reductionResult$x[,1:3], 2, scale)
pcR <- apply(reductionResult$rotation[,1:3], 2, scale)
plot3d(pcX)
text3d(pcX, texts = paste0("H_", c(1:40)))
text3d(pcR, texts = paste0("F_", c(1:40)), col="cornflowerblue")
coords <- NULL
for (i in 1:nrow(pcR)) {
  coords <- rbind(coords, rbind(c(0,0,0),pcR[i,1:3]))
}
lines3d(coords, col="cornflowerblue", lwd=1)


# Linear Regression importance of features
priceS <- cbind(scale(hotels.total$hotel_price), features.scaled)
priceS <- na.omit(priceS)
regPrice <- lm(`scale(hotels.total$hotel_price)` ~., data = priceS)
summary(regPrice)

ratingS <- cbind(scale(hotels.total$rating_total), features.scaled)
ratingS <- na.omit(ratingS)
regRatings <- lm(`scale(hotels.total$rating_total)` ~., data = ratingS)
summary(regRatings)

# Random Forrest importance of features
impPriceT = c()
for (i in 1:100){
  rfPrice <- randomForest(`scale(hotels.total$hotel_price)` ~., data = priceS, importance=TRUE)
  impPriceT <- cbind(impPriceT, importance(rfPrice, type=1))
}
impPrice = apply(impPriceT, 1, mean)
barplot(impPrice[which(impPrice != 0)], names.arg=abbreviate(names(impPrice)[which(impPrice != 0)]), horiz= T, las=1, xlab = "Influence on Price")

impRatingT = c()
for (i in 1:100){
  rfRating <- randomForest(`scale(hotels.total$rating_total)` ~., data = ratingS, importance=TRUE)
  impRatingT <- cbind(impRatingT, importance(rfRating, type=1))
}
impRating = apply(impRatingT, 1, mean)
barplot(impRating[which(impRating != 0)], names.arg=abbreviate(names(impRating)[which(impRating != 0)]), horiz= T, las=1, xlab = "Influence on Rating")

# Feature selection based on PCA
rrMat <- reductionResult$rotation[,1:17]
sDev <- reductionResult$sdev[1:17]
var <- sDev^2/sum(sDev^2)
var = var / sum(var) # normalising var
fRotation <- abs(rrMat)
fRotation <- apply(fRotation, 2, function(col) { col / sum(col)})
fRotation <- t(t(fRotation) * var)
fPCA = apply(fRotation, 1, sum)
names(fPCA) = rownames(rrMat)[order(fPCA)]
fPCA = fPCA[order(fPCA)]
print(fPCA)

#dev.off()
rrHM <- reductionResult$rotation[,1:10]
rownames(rrHM) <- paste0("F_", c(1:40))
heatmap.2(rrHM, dendrogram = "none", Rowv = FALSE, Colv = FALSE, col=redgreen(100))

# Count Trues
cnt <- data.frame(1:45, apply(hotels.total[c(15:59)], 2, function(col) as.numeric(round(sum(1*col) / length(col) * 100))))
ordered = order(cnt[,2], decreasing = T)
cntOrdered = cnt[order(cnt[,2]),]
names = paste0("F_", cntOrdered[,1])
brplt <- barplot(cntOrdered[,2], names.arg = names, horiz= T, las=1, xlab = "Proportion of hotels in percentage", ylab = "Feature")
text(x= cntOrdered[,2]+1, y= brplt, labels=as.character(cntOrdered[,2]), xpd=TRUE)
##### ---------- /DIMENSIONALITY REDUCTION ---------- #####



##### ---------- (STANDARD) K-MEANS AND HIERARCHICAL CLUSTERING ---------- #####
## K-Means
PCs = reductionResult$x[, 1:17]
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