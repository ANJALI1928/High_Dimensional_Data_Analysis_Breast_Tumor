This project presents a pipeline for identifying key protein biomarkers from high-dimensional tumor proteomics data using robust preprocessing, dimensionality reduction, and clustering techniques.

📁 Dataset
Samples: 42 tumor patients

Features: 8,071 protein expression levels

Missing Data: ~40% of the dataset

🧹 Data Preprocessing
Applied variance-normalized KNN (var-norm KNN) to impute missing values.

The method considers the variance of each feature during KNN-based restoration, improving imputation quality and downstream analysis reliability.

📉 Dimensionality Reduction
Used Sparse Principal Component Analysis (Sparse PCA) to reduce dimensionality while preserving interpretability.

Identified 50 key features (biomarkers) that captured 97% of the total variance in the dataset.

🔍 Clustering & Feature Selection
Implemented Robust Sparse K-means Clustering (RSKC) to perform simultaneous clustering and feature selection.

Isolated 24 discriminative proteins that contributed most to sample separation.

Achieved a 63% classification accuracy, indicating meaningful biological subgroup differentiation.

💡 Objective
To enable exploratory biomarker discovery in small-sample, high-dimensional omics data by integrating robust imputation, interpretable feature reduction, and sparse clustering.

