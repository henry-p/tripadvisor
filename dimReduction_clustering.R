library(randomForest)
library(rgl)
#reductionData <- get Features from CleanData
features.scaled <- as.data.frame(apply(hotels.features *1, 2, scale))
reductionResult = prcomp(features.scaled)
var <- reductionResult$sdev^2/sum(reductionResult$sdev^2)
barplot(var, xlab = "PCs", names.arg = c(1:40), ylab = "Variance")
plot(cumsum(var), xlab = "PCs", names.arg = c(1:40), ylab = "cum. Variance", ylim = c(0,1))
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
<<<<<<< HEAD
# 3D Biplot
plot3d(reductionResult$x[,1:3])
#text3d(reductionResult$sdev[1:3], texts = colnames(reductionResult)[1:3])
text3d(reductionResult$rotation[,1:3], texts=rownames(reductionResult$rotation), col="red")
coords <- NULL
for (i in 1:nrow(reductionResult$rotation)) {
  coords <- rbind(coords, rbind(c(0,0,0),reductionResult$rotation[i,1:3]))
}
lines3d(coords, col="red", lwd=1)

=======
#dimdesc(reductionResult)
>>>>>>> 800b8eb83fd38c1c9efa92d220aad5df2cddf9c1

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
<<<<<<< HEAD
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
rrMat <- reductionResult$rotation[,1:40]
sDev <- reductionResult$sdev[1:40]
var <- sDev^2/sum(sDev^2)
fRotation <- abs(rrMat)
fRotation <- apply(fRotation, 2, function(col) { (col - min(col)/(max(col)-min(col)))})
fRotation <- t(t(fRotation) * var)
fPCA = apply(fRotation, 1, sum)
names(fPCA) = rownames(rrMat)[order(fPCA)]
=======
rfPrice <- randomForest(`scale(hotels.total$hotel_price)` ~., data = priceS, importance=TRUE)
impPrice <- importance(rfPrice, type=1)
barplot(impPrice[which(impPrice != 0)], names.arg=abbreviate(rownames(impPrice)[which(impPrice != 0)]), horiz= T, las=1, xlab = "Influence on Price")

rfRating <- randomForest(`scale(hotels.total$rating_total)` ~., data = ratingS, importance=TRUE)
impRating <- importance(rfRating, type=1)
barplot(impRating[which(impRating != 0)], names.arg=abbreviate(rownames(impRating)[which(impRating != 0)]), horiz= T, las=1, xlab = "Influence on Rating")

# Feature selection based on PCA
var <- reductionResult$sdev^2/sum(reductionResult$sdev^2)
fRotation <- abs(reductionResult$rotation)
fRotation <- apply(fRotation, 2, function(col) {col/sum(col)})
fRotation <- t(t(fRotation) * var)
fPCA = apply(fRotation, 1, sum)
>>>>>>> 800b8eb83fd38c1c9efa92d220aad5df2cddf9c1
fPCA = fPCA[order(fPCA)]

# Count Trues
cnt <- data.frame(1:45, apply(hotels.total[c(15:59)], 2, function(col) as.numeric(round(sum(1*col) / length(col) * 100))))
ordered = order(cnt[,2], decreasing = T)
cntOrdered = cnt[order(cnt[,2]),]
brplt <- barplot(cntOrdered[,2], names.arg = cntOrdered[,1], horiz= T, las=1, xlab = "Proportion of hotels in percentage", ylab = "Feature")
text(x= cntOrdered[,2]+1, y= brplt, labels=as.character(cntOrdered[,2]), xpd=TRUE)