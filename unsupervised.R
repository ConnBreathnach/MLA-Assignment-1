#Dataset preparation
data <- read.csv("data/hfd.csv")
unlabelled_data <- data[, 1:4]
summary(unlabelled_data)

# Principal component analysis
pcs <- prcomp(unlabelled_data)
print(pcs)
summary(pcs)
round(pcs$rotation, 2)


# Hierarchical clustering
bloods_dist <- dist(unlabelled_data)
bloods_hier_cluster <- hclust(bloods_dist, method = "complete")
plot(bloods_hier_cluster)
