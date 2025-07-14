setwd("C:\\Users\\anjal\\Downloads\\HDA")
tumor_data = read.csv("tumor_impute.csv")
source("spca-master\\R\\spca.R")

spca_obj <- spca(tumor_data, k=6, alpha=0)
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

# Create a list to store the plots for each PC
plots <- list()

# Define a color palette for the PCs
pc_colors <- rainbow(num_components)

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
  num_features_to_plot <- min(length(sorted_feature_names), 10)
  top_feature_names <- sorted_feature_names[1:num_features_to_plot]
  top_component_loadings <- sorted_component_loadings[1:num_features_to_plot]
  
  # Create a data frame for the current PC
  pc_data <- data.frame(
    Feature = top_feature_names,
    Importance = abs(top_component_loadings),
    PC = factor(paste("PC", i))
  )
  
  # Reverse the order of features to have the most important on top
  pc_data$Feature <- factor(pc_data$Feature, levels = rev(pc_data$Feature))
  
  # Create a bar plot for the current PC with a unique color
  pc_plot <- ggplot(pc_data, aes(x = Importance, y = Feature, fill = PC)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = pc_colors[i])  # Assign a unique color to each PC
  pc_plot <- pc_plot + labs(
    title = paste("Top Features - PC", i),
    x = "Importance",
    y = "Feature Names"
  ) +
    theme_minimal() +
    theme(legend.position = "none")
  
  plots[[i]] <- pc_plot
}

# Arrange the bar plots in a grid
grid.arrange(grobs = plots, ncol = 2)

# Calculate the overall feature importance
overall_importance <- rowSums(abs(loadings))

# Create a data frame for overall importance
overall_data <- data.frame(
  Feature = feature_names,
  Importance = overall_importance
)

# Sort the data frame by importance in descending order
overall_data <- overall_data[order(-overall_data$Importance), ]
overall_data <- overall_data[overall_data$Importance != 0, ]
# Select the top 15 overall important features
top_15_overall_data <- head(overall_data, 25)

# Create a bar plot for the top 15 overall important features in decreasing order
overall_plot <- ggplot(top_15_overall_data, aes(x = reorder(Feature, -Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "orange") +
  labs(title = "Top 25 Overall Important Features", x = "Feature Names", y = "Importance") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 40, hjust = 1, vjust = 0.5))

# Display the top 15 overall feature importance bar plot
print(overall_plot)

nrow(overall_data)
overall_data