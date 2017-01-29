library(kernlab)

# Save binary feature values from hotels as a numeric [0, 1] matrix
m = as.matrix(hotels.features) * 1

# Define the maximum number of clusters that kkmeans runs on
nrClusters = 15

# Calculate Kernel k-Means, using data [x], kernel function [kernel], for all clusters from 1 to [nrClusters].
# Save result in a list.
kkmeansWrapper = function(x, kernel, nrClusters) {
  result = 0
  for (i in seq_len(nrClusters)) {
    kkm = NA
    while(is.na(kkm)) {
      tryCatch({
        kkm = kkmeans(m, centers = i, kernel = kernel)
      }, error=function(e){})
    }
    result[i] = list(list(membership = kkm@.Data, centers = kkm@centers, withinss = kkm@withinss))
  }
  return(result)
}

# Extract the total within-sum-of-squares from kkmeans result-object, that contains results for all clusters from 1 to [nrClusters]
kkmeansTWSS = function(kkmeans.result) {
  sapply(kkmeans.result, function(kkm) {sum(kkm$withinss)})
}

# Save kkmeans results with different kernel functions
kkmeans.result.rbfdot = kkmeansWrapper(m, "rbfdot", nrClusters)
kkmeans.result.polydot = kkmeansWrapper(m, "polydot", nrClusters)
kkmeans.result.vanilladot = kkmeansWrapper(m, "vanilladot", nrClusters)
# kkmeans.result.tanhdot = kkmeansWrapper(m, "tanhdot", nrClusters)
kkmeans.result.laplacedot = kkmeansWrapper(m, "laplacedot", nrClusters)
kkmeans.result.besseldot = kkmeansWrapper(m, "besseldot", nrClusters)
kkmeans.result.anovadot = kkmeansWrapper(m, "anovadot", nrClusters)
# kkmeans.result.splinedot = kkmeansWrapper(m, "splinedot", nrClusters)
# kkmeans.result.stringdot = kkmeansWrapper(m, "stringdot", nrClusters)

# Plot the total-within-sum-of-squares metric against the corresponding number of clusters
pdf(file='plots.pdf')
  plot(seq_len(nrClusters), kkmeansTWSS(kkmeans.result.rbfdot), type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")
  plot(seq_len(nrClusters), kkmeansTWSS(kkmeans.result.polydot), type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")
  plot(seq_len(nrClusters), kkmeansTWSS(kkmeans.result.vanilladot), type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")
  # plot(seq_len(nrClusters), kkmeansTWSS(kkmeans.result.tanhdot), type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")
  plot(seq_len(nrClusters), kkmeansTWSS(kkmeans.result.laplacedot), type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")
  plot(seq_len(nrClusters), kkmeansTWSS(kkmeans.result.besseldot), type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")
  plot(seq_len(nrClusters), kkmeansTWSS(kkmeans.result.anovadot), type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")
  # plot(seq_len(nrClusters), kkmeansTWSS(kkmeans.result.splinedot), type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")
  # plot(seq_len(nrClusters), kkmeansTWSS(kkmeans.result.stringdot), type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")
dev.off()