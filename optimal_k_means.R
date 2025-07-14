setwd("C:\\Users\\anjal\\Downloads\\HDA")
library(sparcl)
library(ggplot2)
library(fpc)
tumor_data = read.csv("tumor_impute.csv")
normalized_df <- as.data.frame(scale(tumor_data, center = TRUE, scale = TRUE))
silwidth = NULL
opt_s = NULL

K_test = NULL
num_nonzero_weights = NULL
i=1
for(s in seq(1.5, 20, 0.5)){
  K_test[i] = KMeansSparseCluster(x=normalized_df, K=4, wbounds = s)
  num_nonzero_weights[i] = length(K_test[i][[1]]$ws[which(K_test[i][[1]]$ws > 0)])
  i = i+1
}
bounds_space = seq(2, 6, len=20)

for(k in 1:7){
  km.permute = KMeansSparseCluster.permute(x=normalized_df, K=k+1,
                                           wbounds=bounds_space, nperms=25)
  opt_s[k+1] = km.permute$bestw
  optimal_gap = max(km.permute$gaps)
  kmeans = KMeansSparseCluster(x=normalized_df, K=k+1, wbounds = opt_s[k+1])
  silwidth[k+1] = cluster.stats(dist(normalized_df), kmeans[[1]]$Cs,
                              silhouette = TRUE)$avg.silwidth
}
K_optimal = which.max(silwidth)
sparse_kmeans = KMeansSparseCluster(x=normalized_df, K = K_optimal, wbounds = opt_s[K_optimal])
