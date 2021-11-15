data <- read.csv("data/hfd.csv")
unlabelled_data <- data[, 1:4]
summary(unlabelled_data)


pcs <- prcomp(unlabelled_data)
print(pcs)
summary(pcs)
round(pcs$rotation, 2)

pc_mat <- as.matrix(pcs[["x"]])
pc_dist <- dist(pc_mat)
pc_hier_cluster <- hclust(pc_dist, method="single")
plot(pc_hier_cluster)

bloods_dist <- dist(unlabelled_data)
bloods__hier_cluster <- hclust(bloods_dist, method = "average")

plot(bloods__hier_cluster)
