# lab6_theory.R

# fix(iris)
labels_iris <- iris[,5] # extract the labels
iris_C <- iris[,-5] # remove the labels


maxs <- apply(iris_C, 2, max)
mins <- apply(iris_C, 2, min)

# ---
# example of scaling (normalizing) the data
# for example, having a data frame df
df <- data.frame(
  feature1 = c(1, 2, 3, 4, 5),
  feature2 = c(6, 7, 8, 9, 10)
)
# using z-score normalization
df_centered <- scale(df, center = TRUE, scale = apply(df, 2, max) - apply(df, 2, min))
# ---

# using min-max normalizationv
iris_C <- scale(iris_C, center= mins, scale = maxs - mins)

dist.iris <- dist(iris_C, method = "euclidean")

clust.iris <- hclust(dist.iris, method = "ward.D")


# install.packages("factoextra")
library(factoextra)
library(cluster)
fviz_nbclust(iris_C, kmeans, method="wss") # elbow method


fviz_nbclust(iris_C, kmeans, method = "silhouette") + # silhouette
  labs(subtitle = "Silhouette method")


fviz_nbclust(iris_C, kmeans, method = "wss")
# count gap statistics basing on the number of clusters K.max = 5
gap_stat <- clusGap(iris_C, FUN = kmeans, nstart = 5, K.max = 5, B = 5)

# plot number of clusters vs gap statistic
fviz_gap_stat(gap_stat)


# Consensus-based clustering
# install.packages("parameters")
library(parameters)

n_clust <- n_clusters(iris[,-5], package=c("easystats", "NbClust", "mclust"),
                      standardize = FALSE)
n_clust
plot(n_clust)



plot(clust.iris)

plot(clust.iris, labels_iris, cex=0.5)
rect.hclust(clust.iris, k=4, border="red")

hcd <- as.dendrogram(clust.iris)
hcd # hdc is a not builded dendrogram object
# after this will be graphical representation in 3 rows and 1 column
par(mfrow=c(3,1))
# top side while cutting
plot(cut(hcd, h=4)$upper, main="top side of dendrogram")
# first branch of bottom side (Branch 1)
plot(cut(hcd, h=4)$lower[[1]], main="Branch 1")

# third branch of bottom side (Branch 3)
plot(cut(hcd, h=4)$lower[[3]], main="Branch 3")
# dev.off()


par(mfrow=c(1,1))
# selection of 3 clusters
groups <- cutree(clust.iris, k=3)

# 1 cluster
g1 <- colMeans(iris[groups == 1, 1:4])
# 2 cluster
g2 <- colMeans(iris[groups == 2, 1:4])
# 3 cluster
g3 <- colMeans(iris[groups == 3, 1:4])

df <- data.frame(g1, g2, g3)
df1 <- t(df)
df <- t(df1)

barplot(df, col=c("red", "green", "blue", "yellow"))

barplot(df, ylim=c(0, 12),
        main = "Groups of iris", axes=FALSE,
        col = c("red", "green", "blue", "yellow"), beside=TRUE)
axis(2, at=0:5, labels = 0:5)
legend("top", legend = rownames(df),
       col=c("red","green","blue","yellow"), lwd= 10, bty = "n")


# iris k-means clustering
km.res <- kmeans(iris_C, 3, nstart = 10)
fviz_cluster(km.res, iris[, -5], ellipse.type = "norm")

# change the color palett and theme
fviz_cluster(km.res, iris[, -5],
             palette = "Set2", ggtheme = theme_minimal())


library(lattice)
my_data <- iris
head(my_data)

xyplot(Sepal.Length ~ Petal.Length, group = Species, data = my_data, auto.key = TRUE)


boxplot(Sepal.Length ~ Species, data = iris, ylab = "Sepal.Length",
        frame = FALSE, col = "lightgray")


pairs(iris[, -5])
pairs(iris[, -5], main = "Irises", col = c("red", "green", "blue"))

my_cols <- c("#00AFBB", "#E7B800", "#FC4E07")
pairs(iris[,-5],main= "Irises by sort",pch = 19,  cex = 0.8,
      col = my_cols[iris$Species],
      lower.panel=NULL)


# lets try to determine affilation of the iris according to the sepal length and width
xyplot(Sepal.Length ~ Petal.Length | Species,
       layout = c(3, 1),
       group = Species, data = iris,
       type = c("p", "smooth"),
       scales = "free")


# lets build 3d plot of our classes
# install.packages("scatterplot3d")
library("scatterplot3d")
colors <- c("#999999", "#E69F00", "#56B4E9")
colors <- colors[as.numeric(iris$Species)]
s3d <- scatterplot3d(iris[,1:3], main= "Irises by sort", pch = 16, color=colors)
legend(s3d$xyz.convert(7.5, 3, 4.5), legend = levels(iris$Species),
       col =  c("#999999", "#E69F00", "#56B4E9"), pch = 16)


# k-means clustering (ggplot2)

packages <- c("ggplot2", "dplyr", "tidyr", "tibble")
# install.packages(packages)
library(ggplot2)
library(dplyr)
library(tidyr)
library(tibble)
iris %>%
  ggplot(aes(Petal.Length, Petal.Width, color = Species))+geom_point()


library(broom)
set.seed(42)
iris %>%
  select(Petal.Length, Petal.Width) %>%
  kmeans(centers = 3) ->  km

km %>%
  augment(iris) %>% count(Species, .cluster)

km %>%
  augment(iris) %>%
  mutate(.cluster = recode_factor(.cluster,
                                  `1` = "setosa",
                                  `2` = "versicolor",
                                  `3` = "virginica"),
         correct = Species == .cluster) %>%
  ggplot(aes(Petal.Length, Petal.Width))+
  geom_point(aes(color = correct, shape = Species))+
  geom_point(data = data.frame(km$centers)) # centroids