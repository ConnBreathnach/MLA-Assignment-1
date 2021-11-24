#Dataset preparation
data <- read.csv("data/hfd.csv")
head(data)
unlabelled_data <- data[, 1:4]
summary(unlabelled_data)
hist(unlabelled_data[,1])

# Principal component analysis
pcs <- prcomp(unlabelled_data)
print(pcs)
summary(pcs)
sround(pcs$rotation, 2)
first_3_pcs <- pcs$x[,1:3]
first_3_pcs

# PCA component percentage graph
pc_var <- pcs$sdev^2 / sum(pcs$sdev^2)
plot(pc_var, type="b", main="Proportion of variance explained by components", xlab="No. of components",ylab="Proportion of variance explained", xaxt="n")
axis(1, at= 1:4)


predicted <- predict(pcs)
pairs(unlabelled_data)
pairs(predicted)

# Hierarchical clustering
bloods_scale <- scale(unlabelled_data, center = TRUE, scale = TRUE)
bloods_dist <- dist(bloods_scale)
bloods_hier_cluster <- hclust(bloods_dist, method = "complete")
plot(bloods_hier_cluster)
height_mean <- mean(bloods_hier_cluster$height)
height_std <- sd(bloods_hier_cluster$height)
cluster_line = height_mean + (3*height_std)
abline(h = cluster_line, col="blue")


#K means clustering
install.packages("factoextra")
install.packages("NbClust")
library(factoextra)
library(NbClust)
nb_blood <- NbClust(unlabelled_data, method="kmeans")
pc_mat <- as.matrix(pcs)
scaled_data <- scale(unlabelled_data)
fviz_nbclust(first_3_pcs, kmeans, "wss")

k <- 4
k_clusters <- kmeans(unlabelled_data, centers = k)
table(k_clusters$cluster)
k_clusters$centers
k_clusters$withinss
dist(k_clusters$centers)
plot(unlabelled_data, col = k_clusters$cluster)
points(k_clusters$centers, col=1:k, pch=8, cex=5)


k <- 4
k_clusters_scaled <- kmeans(scaled_data, centers = k)
table(k_clusters_scaled$cluster)
k_clusters_scaled$centers
k_clusters_scaled$withinss
dist(k_clusters_scaled$centers)
plot(scaled_data, col = k_clusters_scaled$cluster)
points(k_clusters_scaled$centers, col=1:k, pch=8, cex=5)

k <- 4
k_clusters_pc <- kmeans(first_3_pcs, centers = k)
table(k_clusters_pc$cluster)
k_clusters_pc$withinss
fig <- plot_ly(df, x=~PC1, y = ~PC2, z = ~PC3, color = k_clusters_pc$cluster)
fig <- fig %>% add_markers()
fig
k_clusters_pc$centers
dist(k_clusters_pc$centers)
plot(first_3_pcs, col = k_clusters_pc$cluster)
points(k_clusters_pc$centers, col=1:k, pch=8, cex=5)
summary(first_3_pcs)

plot(first_3_pcs[,2], first_3_pcs[,3], col = k_clusters$cluster)

install.packages("plotly")
library("plotly")
df <- as.data.frame(first_3_pcs)
fig <- plot_ly(df, x=~PC1, y = ~PC2, z = ~PC3, color = k_clusters_pc$cluster)
fig <- fig %>% add_markers()
fig

df2 <- as.data.frame(scaled_data)
fig_2 <- plot_ly(df2, color = k_clusters_scaled$cluster)
fig_2 <- fig_2 %>% add_markers()

k_clusters_pc$withinss / k_clusters_pc$size
