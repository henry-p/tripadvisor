## simple explorary analysis
str(hotels)
summary(hotels)

## correlations 
p.cor <- cor(hotels.noNA[c(60, 3:5, 8, 13, 64)])
#s.cor <- cor(hotels.noNA[c(60, 3:5, 8, 13 64)], method = "spearman")
#k.cor <- cor(hotels.noNA[c(60, 3:5, 8, 13, 64)], method = "kendall")



## corrplot
library(corrplot)
par(mfrow = c(1,1))
corrplot(p.cor, method = "shade")
#corrplot(s.cor, method = "shade", main = 'Spearman')
#corrplot(k.cor, method = "shade", main = 'Kendall')

#REGRESSION (features -> how many customers stayed in hotel? Do features affect customer's choice of hotel?)
hotels.features.lm = hotels.total[,c(4,15:59)]
features.lm.noNA =na.omit(hotels.features.lm)

#regression countings
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

# regression: price
hotels.features.lm2 = hotels.total[,c(60, 15:59)]
features.lm2.noNA =na.omit(hotels.features.lm2)
regression.price <- lm(hotel_price ~.
                       , data = features.lm2.noNA
)

# stacked bar chart with the number of reviewers by type
reviewer.type <- hotels.noNA[,c(9:12)]
reviewer.type.T <- t(reviewer.type)
colnames(reviewer.type.T) <- hotels.noNA[,1]
bp <- barplot(as.matrix(reviewer.type.T), main="# of reviewers by type",
              xlab="Hotels", col=c("lightblue", "mistyrose", "lightcyan","lavender"), cex.names = 0.6, srt=45, 
              legend = rownames(counts))


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

pairs(hotels.noNA[c(3:5,8:13,60)], cex = 0.5, pch = 21, col = "dodgerblue3", bg = "azure", diag.panel = panel.hist, upper.panel = panel.cor)


# distribution for 4 features
layout(matrix(1:4, ncol = 2, nrow = 2))
d1 <- density(hotels.noNA$rating_total)
plot(d1, main="rating_total")
polygon(d1, col="azure", border="cyan")
d2 <- density(hotels.noNA$count_ratings_total)
plot(d2, main="count_rating_total")
polygon(d2, col="azure", border="cyan")
d3 <- density(hotels.noNA$foto_count)
plot(d3, main="foto_count")
polygon(d3, col="azure", border="cyan")
d4 <- density(hotels.noNA$hotel_stars)
plot(d4, main="hotel_stars")
polygon(d4, col="azure", border="cyan")


#box-cox
sapply(colnames(shift.cl.data), function(x){
  lambda <- boxcoxnc(as.numeric(shift.cl.data[[x]]), method = 'sw',
                     plotit = F, lam = seq(-4,4, 0.01))$result[[1]]
  tr <- (shift.cl.data[[x]]^{lambda}-1)/lambda
  hist(tr, main = x, xlab = round(shapiro.test(tr)$p.value,2),
       col = 'cyan', cex.main = 2, cex.lab = 2, ylab = "")
  #  print(lambda)
})

