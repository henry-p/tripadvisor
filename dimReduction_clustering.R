#reductionData <- get Features from CleanData
reductionData = hotels.features
reductionResult = prcomp(reductionData, scale = TRUE)
plot(reductionResult, type = "l")
summary(reductionResult)

# Different way
regLinearR <- lm(hotels.noNA$rating_total ~ ., data = hotels.noNA)
summary(regLinearR)

pricing <- cbind(hotels.total$hotel_price, hotels.features * 1)
pricing <- na.omit(pricing)
regLinearP <- lm(pricing$`hotels.total$hotel_price` ~ ., data = pricing)
summary(regLinearP)
