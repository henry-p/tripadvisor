file = paste0(dirname(rstudioapi::getActiveDocumentContext()$path), "/tripadvisor-muenster.RData")
load(file)
# Remove useless columns
hotels = tripadvisor3$tripadvisor[, -which(names(tripadvisor3$tripadvisor) %in%
                                        c("hotel_address", "hotel_city", "hotel_region", "hotel_country",
                                          "rating_excellent", "rating_verygood", "rating_average", "rating_poor", "rating_terrible",
                                          "rating_location", "rating_sleep", "rating_rooms", "rating_service", "rating_value", "rating_cleanliness",
                                          "hotel_url"))]

# Remove columns that are all FALSE out of the "f1_...f45_" columns
hotels = hotels[, -which(names(hotels) %in% names(which(apply(hotels[,15:59], 2, sum) == 0)))]

# one hotel appears three times (32,1,11). so lets remove them
hotels = hotels[-c(1,11),]

top3attractions = tripadvisor3$top3attractions
top3attractions = top3attractions[, -4]

# numberNAsInRows = apply(mydata, 1, function(row) sum(is.na(row)))
# mydata = mydata[-which(numberNAsInRows > 5), ]