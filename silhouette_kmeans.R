setwd("C:\\Users\\anjal\\Downloads\\HDA")
library(sparcl)
library(ggplot2)
library(fpc)
tumor_data = read.csv("tumor_impute.csv")
normalized_df <- as.data.frame(scale(tumor_data, center = TRUE, scale = TRUE))
sil = 0
n_clust = 10

i_values = 2:n_clust

for(i in i_values){
  kmeans = KMeansSparseCluster(x=normalized_df, K=i)
  summary(kmeans)
  normalized_df$cluster = kmeans[[20]]$Cs
  sil[i] = cluster.stats(dist(normalized_df), kmeans[[20]]$Cs, silhouette = TRUE)$avg.silwidth
} 

result_matrix <- data.frame(i = i_values, silwidth=sil[2:n_clust])

ggplot(result_matrix, aes(x = i, y = silwidth)) +
  geom_line() +
  geom_point() +
  labs(title = "Silhouette Method for Optimal Number of Clusters", x = "Number of Clusters (k)", y = "Silhouette Width") +
  theme_minimal()