setwd("C:\\Users\\anjal\\Downloads\\HDA")
library(sparcl)
library(ggplot2)
tumor_data = read.csv("tumor_impute.csv")

hcl = HierarchicalSparseCluster(x=as.matrix(tumor_data),
                          method='average',
                          dissimilarity="squared.distance", niter = 10)

ColorDendrogram(hc=hcl, y=hcl$hc$order, main="", branchlength=0.7, labels=NULL, xlab=NULL,
                sub="NULL", ylab="", cex.main=NULL)
library(HierarchicalSparseCluster)
dend <- as.dendrogram(hcl)
linkage_matrix <- as.hclust(dend)$merge

# Heatmap for dissimilarity, Assuming hcl$u is your 16x16 matrix
heatmap_data <- as.data.frame(as.table(hcl$u))
colnames(heatmap_data) <- c("X", "Y", "Value")
ggplot(heatmap_data, aes(x = X, y = Y, fill = Value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "Heatmap") +
  theme_minimal()



set.seed(1)
x<-matrix(rnorm(100*20),ncol=20)
y<-c(rep(1,50),rep(2,50))
x[y==1,]<-x[y==1,]+2
#Performhierarchicalclustering
hc<-hclust(dist(x),method="complete")
