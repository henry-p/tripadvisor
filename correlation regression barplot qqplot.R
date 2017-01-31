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
png("~/Desktop/type.png", width = 500, height = 300)
par(mar=c(5,12,4,2)+0.1,mgp=c(3,1,0))
bp <- barplot(as.matrix(reviewer.type.T), main="# of reviewers by type", 
              las=1, horiz = TRUE, col=c("grey70","darkred","grey78","rosybrown1"), 
              cex.names = 0.8,cex.axis=0.8)
dev.off()

# bar chart for the number of features
count.features.a <- rowSums(hotels[,15:54])
cf.data <- data.frame(hotels$hotel_name, count.features.a)
cf.ordered <- cf.data[order(cf.data[,2],decreasing=TRUE),]
png("~/Desktop/count_features.png", width = 1000, height = 500)
par(mar=c(5,10,4,2)+0.1,mgp=c(3,1,0))
cf.bp <-barplot(cf.ordered[,2], names.arg = cf.ordered[,c(1)], horiz= T, las=1, xlab = "The number of features in hotel",  
                cex.names = 0.8,cex.axis=0.8)
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

