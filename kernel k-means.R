library(kernlab)

m = as.matrix(hotels.features) * 1
maxNrOfClusters = 15

kkmeansTSSWrapper = function(x, kernel, maxNrOfClusters) {
  totalwss = 0
  for (i in seq_len(maxNrOfClusters)) {
    km = NA
    while(is.na(km)) {
      tryCatch({
        km = kkmeans(m, centers = i, kernel = "rbfdot")
      }, error=function(e){})
    }
    totalwss[i] = sum(km@withinss)
  }
  return(totalwss)
}

plot(seq_len(maxNrOfClusters), kkmeansTSSWrapper(m, "rbfdot", maxNrOfClusters), type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")
plot(seq_len(maxNrOfClusters), kkmeansTSSWrapper(m, "polydot", maxNrOfClusters), type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")
plot(seq_len(maxNrOfClusters), kkmeansTSSWrapper(m, "vanilladot", maxNrOfClusters), type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")
plot(seq_len(maxNrOfClusters), kkmeansTSSWrapper(m, "tanhdot", maxNrOfClusters), type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")
plot(seq_len(maxNrOfClusters), kkmeansTSSWrapper(m, "laplacedot", maxNrOfClusters), type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")
plot(seq_len(maxNrOfClusters), kkmeansTSSWrapper(m, "besseldot", maxNrOfClusters), type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")
plot(seq_len(maxNrOfClusters), kkmeansTSSWrapper(m, "anovadot", maxNrOfClusters), type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")
plot(seq_len(maxNrOfClusters), kkmeansTSSWrapper(m, "splinedot", maxNrOfClusters), type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")
plot(seq_len(maxNrOfClusters), kkmeansTSSWrapper(m, "stringdot", maxNrOfClusters), type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")
