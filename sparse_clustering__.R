setwd("C:\\Users\\anjal\\Downloads\\HDA")
library(sparcl)
library(ggplot2)
tumor_data = read.csv("tumor_impute.csv")
kmeans = KMeansSparseCluster(x=tumor_data, K=3)
summary(kmeans)
tumor_data$cluster = kmeans[[2]]$Cs
ggplot(data = tumor_data$cluster, aes(x = ACO2, y = PLIN4, color = factor(cluster))) +
  geom_point() +
  ggtitle("Scatter Plot of Tumor Data by Cluster") +
  xlab("ACO2") +
  ylab("PLIN4")

# Load the necessary libraries
library(plotly)

# Assuming your "tumor_data" includes columns "ACO2," "PLIN4," and "AnotherColumn" for the 3D plot
# You should adjust these column names accordingly

# Create a 3D scatter plot with cluster colors
plot_ly(data = tumor_data, x = ~ACO2, y = ~PLIN4, z = ~RPS10, color = ~cluster, colors = "Set1") %>%
  add_markers() %>%
  layout(scene = list(
    xaxis = list(title = "ACO2"),
    yaxis = list(title = "PLIN4"),
    zaxis = list(title = "RPS10")
  )) %>%
  colorbar(title = "Cluster")
