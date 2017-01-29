#reductionData <- get Features from CleanData
reductionData = hotels.features
reductionResult = prcomp(reductionData, scale = TRUE)
plot(reductionResult, type = "l")
summary(reductionResult)

# Different way
ratings <- cbind(hotels.total$rating_total, hotels.features * 1)
ratings <- na.omit(ratings)
regLinearR <- lm(ratings$`hotels.total$rating_total` ~ ., data = ratings)
summary(regLinearR)

pricing <- cbind(hotels.total$hotel_price, hotels.features * 1)
pricing <- na.omit(pricing)
regLinearP <- lm(pricing$`hotels.total$hotel_price` ~ ., data = pricing)
summary(regLinearP)
