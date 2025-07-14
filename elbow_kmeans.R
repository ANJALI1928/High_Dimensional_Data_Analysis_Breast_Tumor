setwd("C:\\Users\\anjal\\Downloads\\HDA")
library(sparcl)
library(ggplot2)
library(fpc)
library(factoextra)
library(NbClust)
tumor_data = read.csv("tumor_impute.csv")
normalized_df <- as.data.frame(scale(tumor_data, center = TRUE, scale = TRUE))
wcss = 0
n_clust = 10

i_values = 2:n_clust

for(i in i_values){
kmeans = KMeansSparseCluster(x=normalized_df, K=i)
summary(kmeans)
#normalized_df$cluster = kmeans[[20]]$Cs
wcss[i] = kmeans[[20]]$wcss$wcss
} 
weights <- kmeans[[20]]$ws

# Feature names from colnames(tumor_data)
feature_names <- colnames(tumor_data)

# Create a data frame with feature names and weights
feature_df <- data.frame(Feature = feature_names, Weight = weights)

# Sort the data frame by weight in descending order
sorted_feature_df <- feature_df[order(-feature_df$Weight), ]

# Print the sorted data frame
print(sorted_feature_df)

result_matrix <- data.frame(i = i_values, wcss = wcss[2:n_clust])

ggplot(result_matrix, aes(x = i, y = wcss)) +
  geom_line() +
  geom_point() +
  labs(title = "Elbow Method for Optimal Number of Clusters", x = "Number of Clusters (k)", y = "Within-Cluster Sum of Squares") +
  theme_minimal()


# Perform min-max scaling on the weights
min_weight <- min(sorted_feature_df$Weight)
max_weight <- max(sorted_feature_df$Weight)
scaled_weights <- (sorted_feature_df$Weight - min_weight) / (max_weight - min_weight)
sorted_feature_df$scaled_weights <- scaled_weights
# Create a bar plot of scaled weights
library(ggplot2)
bar_plot_scaled <- ggplot(sorted_feature_df[1:100,], aes(x = reorder(Feature, -scaled_weights), y = scaled_weights)) +
  geom_bar(stat = "identity", fill = "dodgerblue") +
  xlab("Feature") +
  ylab("Scaled Weight") +  # Update the y-axis label
  ggtitle("Top 30 Min-Max Scaled Feature Weights") +  # Update the title
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Display the bar plot
print(bar_plot_scaled)
