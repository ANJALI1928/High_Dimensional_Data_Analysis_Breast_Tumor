setwd("C:\\Users\\anjal\\Downloads\\HDA")
tumor_data = read.csv("tumor_impute.csv")
source("spca-master\\R\\spca.R")

spca_obj <- spca(tumor_data, k=5)
summary(spca_obj)
spca_obj$eigenvalues

options(max.print = 4000)
loadings = spca_obj$loadings
feature_names = colnames(tumor_data)
# Assuming you have already performed PCA and obtained loadings in loadings_matrix
# and have feature names in feature_names

# Load the ggplot2 library
# Load necessary libraries
library(ggplot2)
library(gridExtra)
# Define the number of principal components to plot
num_pcs_to_plot <- 4

# Create a list to store individual barplots
barplot_list <- list()

# Define your threshold for non-zero loadings
non_zero_threshold <- 0.1

# Loop through each principal component to plot
for (i in 1:num_pcs_to_plot) {
  # Create a data frame for the current PC
  feature_importance <- data.frame(
    Feature = feature_names,
    Absolute_Importance = abs(loadings[, i]),
    Non_Zero_Importance = ifelse(abs(loadings[, i]) > non_zero_threshold, abs(loadings[, i]), 0)
  )
  
  # Sort the data frame by absolute importance in descending order
  feature_importance <- feature_importance[order(-feature_importance$Absolute_Importance), ]
  
  # Print the feature_importance for debugging
  print(head(feature_importance))
  
  # Create the barplot for the current PC showing both absolute and non-zero loadings
  barplot_absolute <- ggplot(data = feature_importance, aes(x = reorder(Feature, Absolute_Importance), y = Absolute_Importance)) +
    geom_bar(stat = "identity", fill = "skyblue") +
    labs(x = "Features", y = "Importance") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
    ggtitle(paste("Feature Importance for PC", i, "(Absolute Loadings)"))
  
  # Store the barplot in the list
  barplot_list[[i]] <- barplot_absolute
  
  # Create the barplot for the current PC showing non-zero loadings
  barplot_non_zero <- ggplot(data = feature_importance, aes(x = reorder(Feature, Non_Zero_Importance), y = Non_Zero_Importance)) +
    geom_bar(stat = "identity", fill = "lightcoral") +
    labs(x = "Features", y = "Importance") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
    ggtitle(paste("Feature Importance for PC", i, "(Non-Zero Loadings)"))
  
  # Store the barplot in the list
  barplot_list[[i + num_pcs_to_plot]] <- barplot_non_zero
}

# Arrange the plots in a 2x2 grid
grid.arrange(grobs = barplot_list, ncol = 2, top = "Feature Importance for First 4 Principal Components")