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