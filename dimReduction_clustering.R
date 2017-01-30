#reductionData <- get Features from CleanData
reductionResult = prcomp(hotels.features, scale = TRUE)
plot(reductionResult, type = "l")
summary(reductionResult)

biplot(reductionResult)
dimdesc(reductionResult)

# Count Trues
cnt <- data.frame(1:45, apply(hotels.total[c(15:59)], 2, function(col) as.numeric(round(sum(1*col) / length(col) * 100))))
ordered = order(cnt[,2], decreasing = T)
cntOrdered = cnt[order(cnt[,2]),]
brplt <- barplot(cntOrdered[,2], names.arg = cntOrdered[,1], horiz= T, las=1, xlab = "Proportion of hotels in percentage", ylab = "Feature")
text(x= cntOrdered[,2]+1, y= brplt, labels=as.character(cntOrdered[,2]), xpd=TRUE)

# Different way
regLinearR <- lm(hotels.noNA$rating_total ~ ., data = hotels.noNA)
summary(regLinearR)
pricing <- cbind(hotels.total$hotel_price, hotels.features * 1)
pricing <- na.omit(pricing)
regLinearP <- lm(pricing$`hotels.total$hotel_price` ~ ., data = pricing)
summary(regLinearP)
