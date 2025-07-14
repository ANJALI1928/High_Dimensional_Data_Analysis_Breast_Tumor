setwd("C:\\Users\\anjal\\Downloads\\HDA")
tumor_data = read.csv("tumor_impute.csv")
source("spca-master\\R\\spca.R")


spca_obj <- spca(tumor_data, k=5, alpha=0)
summary(spca_obj)
spca_obj$eigenvalues

options(max.print = 4000)
loadings = spca_obj$loadings
#idx = which(loadings[, 1] != 0)
#tumor_data[,idx]
#colnames(tumor_data)[idx]
# Get the loadings from the SPCA object

# Calculate the number of non-zero loadings for each feature
num_non_zero_loadings <- rowSums(loadings != 0)

# Find the indices of features with the highest number of non-zero loadings
top_feature_indices <- order(num_non_zero_loadings, decreasing = TRUE)

non_zero_loadings_indices <- which(num_non_zero_loadings > 0)
num_top_features <- 631
top_feature_indices <- intersect(top_feature_indices, non_zero_loadings_indices)

# Extract the top features from tumor_data
top_features <- tumor_data[, top_feature_indices]

# Get the column names of the top features
top_feature_names <- colnames(tumor_data)[top_feature_indices[1:num_top_features]]

non_na_features <- colnames(top_features)[!apply(is.na(top_features), 2, all)]

# Get the column names of features with non-NA values
top_feature_names2 <- intersect(top_feature_names, non_na_features)
VarImp(spca_obj)