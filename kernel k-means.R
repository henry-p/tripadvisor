library(kernlab)

m = as.matrix(hotels.features) * 1

totalwss = 0
maxNrOfClusters = 15
for (i in seq_len(maxNrOfClusters)) {
  km = NA
  while(is.na(km)) {
    tryCatch({
      km = kkmeans(m, centers = i, kernel = "polydot")
    }, error=function(e){})
  }
  totalwss[i] = sum(km@withinss)
}

plot(seq_len(maxNrOfClusters), totalwss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")
