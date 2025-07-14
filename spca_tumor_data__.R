setwd("C:\\Users\\Harshit\\Desktop\\data_science_practice\\HDA")
tumor_data = read.csv("tumor_impute.csv")
source("spca-master\\R\\spca.R")

spca_obj <- spca(tumor_data, k=6)
summary(spca_obj)
spca_obj$eigenvalues

options(max.print = 4000)
loadings = spca_obj$loadings
feature_names = colnames(tumor_data)

library(ggplot2)
library(gridExtra)

# Extract loadings and feature names
loadings <- spca_obj$loadings
feature_names <- colnames(tumor_data)

# Number of principal components to consider
num_components <- 6

# Define a color palette for the PCs
pc_colors <- rainbow(num_components)

# Create a list to store the plots for each PC
plots <- list()

# Iterate through principal components
for (i in 1:num_components) {
  component_loadings <- loadings[, i]
  # Filter out features with all-zero loadings
  non_zero_loadings_indices <- component_loadings != 0
  non_zero_feature_names <- feature_names[non_zero_loadings_indices]
  non_zero_component_loadings <- component_loadings[non_zero_loadings_indices]
  sorted_indices <- order(-abs(non_zero_component_loadings))
  sorted_feature_names <- non_zero_feature_names[sorted_indices]
  sorted_component_loadings <- non_zero_component_loadings[sorted_indices]
  
  # Determine the number of features to plot (up to 10 or the actual number of non-zero loadings)
  num_features_to_plot <- min(length(sorted_feature_names), 15)
  top_feature_names <- sorted_feature_names[1:num_features_to_plot]
  top_component_loadings <- sorted_component_loadings[1:num_features_to_plot]
  
  # Create a data frame for the current PC
  pc_data <- data.frame(
    Feature = top_feature_names,
    Importance = abs(top_component_loadings),
    PC = factor(paste("PC", i))
  )
  
  # Create a bar plot for the current PC with a unique color
  pc_plot <- ggplot(pc_data, aes(x = Importance, y = Feature, fill = PC)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = pc_colors[i])  # Assign a unique color to each PC
  pc_plot <- pc_plot + labs(
    title = paste("Top Features - PC", i),
    x = "Importance",
    y = "Feature Names"
  ) +
    theme_minimal()
  
  plots[[i]] <- pc_plot
}

# Arrange the bar plots in a grid
grid.arrange(grobs = plots, ncol = 2)

# Sum of absolute loadings for each feature across all PCs
feature_importance_overall <- rowSums(abs(loadings))

# Create a data frame for the overall feature importance
overall_feature_data <- data.frame(
  Feature = feature_names,
  Importance = feature_importance_overall
)

# Sort the data frame by importance in descending order
overall_feature_data <- overall_feature_data[order(-overall_feature_data$Importance), ]

# Select the top 10 overall most important features
top_10_overall_feature_data <- overall_feature_data[1:10, ]

# Create a bar plot for the top 10 overall most important features
top_10_overall_feature_plot <- ggplot(top_10_overall_feature_data, aes(x = Importance, y = Feature)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(
    title = "Top 10 Overall Most Important Features",
    x = "Importance",
    y = "Feature Names"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
        
# Display the bar plot
print(top_10_overall_feature_plot)
