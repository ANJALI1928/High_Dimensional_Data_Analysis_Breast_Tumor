setwd("C:\\Users\\anjal\\Downloads\\HDA")
library(sparcl)
library(ggplot2)
tumor_data = read.csv("tumor_impute.csv")
hc <- hclust(dist(t(tumor_data), method = "euclidean"), method = "complete")

# Access the linkage matrix
linkage_matrix <- hc$merge
csv_file_path = "C:\\Users\\anjal\\Downloads\\HDA\\linkage_matrix.csv"
write.csv(linkage_matrix,  file = csv_file_path,  row.names = FALSE)
