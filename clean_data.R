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

# numberNAsInRows = apply(mydata, 1, function(row) sum(is.na(row)))
# mydata = mydata[-which(numberNAsInRows > 5), ]

