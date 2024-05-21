# lab6.R

# libraries needed for the script
# install.packages("GGally")
library(ggplot2)
library(GGally)
library(dplyr)
library(tidyr)
library(corrplot)
library(psych)

# Load the data from the CSV file
data <- read.csv("lab6/dataset/Job_Placement_Data.csv")

# descriptive analysis block for the data ------------------------------
summary(data)
describe(data)

# visualizing the distribution of the data
ggplot(data, aes(x = ssc_percentage)) + geom_histogram(binwidth = 5, fill = "blue", color = "black") + theme_minimal()
ggplot(data, aes(x = hsc_percentage)) + geom_histogram(binwidth = 5, fill = "green", color = "black") + theme_minimal()

# correlation matrix
cor_matrix <- cor(data[, sapply(data, is.numeric)])
corrplot(cor_matrix, method = "circle")

# boxplot for identifying outliers
boxplot(data$ssc_percentage, main = "SSC Percentage", col = "orange")
boxplot(data$hsc_percentage, main = "HSC Percentage", col = "purple")

# pairwise scatter plots
pairs(data[, sapply(data, is.numeric)], main = "Scatterplot Matrix")

# categorical data analysis
table(data$gender)
ggplot(data, aes(x = gender)) + geom_bar(fill = "lightblue") + theme_minimal()

# check for normal distribution
shapiro.test(data$ssc_percentage)
qqnorm(data$ssc_percentage)
qqline(data$ssc_percentage, col = "red")


data <- data %>% drop_na()

# ------------------------------ end of descriptive analysis block

# data clustering block ------------------------------

# libraries needed for the script
library(factoextra)
library(cluster)
library(NbClust)
library(parameters)

numeric_columns <- data[, sapply(data, is.numeric)]
# stundardize the data
data_clustering <- scale(numeric_columns)
str(data_clustering)

# lets use different methods to determine the number of clusters
data_clustering_df <- as.data.frame(data_clustering)
fviz_nbclust(data_clustering_df, kmeans, method = "wss") # elbow method
fviz_nbclust(data_clustering_df, kmeans, method = "silhouette") # silhouette method
gap_stat <- clusGap(data_clustering, FUN = kmeans, nstart = 25, K.max = 10, B = 50)
fviz_gap_stat(gap_stat)

# Consensus-based clustering
n_clust <- n_clusters(data_clustering_df, package = c("easystats", "NbClust", "mclust"))
plot(n_clust)

# got 3 clusters as the optimal number of clusters

# K-means clustering with 3 clusters
set.seed(123)
km_res <- kmeans(data_clustering, centers = 3, nstart = 25)

# cluster visualization
fviz_cluster(km_res, data = data_clustering)
fviz_cluster(km_res, data = data_clustering, ellipse.type = "norm")

# hierarchical clustering
dist_data <- dist(data_clustering)
clust_data <- hclust(dist_data, method = "ward.D2")
plot(clust_data)
rect.hclust(clust_data, k = 3, border = "red")

hcd <- as.dendrogram(clust_data)

par(mfrow = c(3, 1))
plot(cut(hcd, h = 4)$upper, main = "Верхняя часть дендрограммы")

plot(cut(hcd, h = 4)$lower[[1]], main = "Первая ветка нижней части")

# Третья ветка нижней части
plot(cut(hcd, h = 4)$lower[[3]], main = "Третья ветка нижней части")
# dev.off()

# ------------------------------ end of data clustering block

# data visualization block ------------------------------

# for more data visualization go to lab6_visualization.R

# bar plot for the 'gender' variable
ggplot(data, aes(x = gender)) +
  geom_bar(fill = "lightblue") +
  theme_minimal() +
  labs(title = "Распределение по полу", x = "Пол", y = "Количество")

# bar plot for the 'ssc_board' variable
ggplot(data, aes(x = ssc_board)) +
  geom_bar(fill = "lightgreen") +
  theme_minimal() +
  labs(title = "Распределение по совету образования (SSC)", x = "Совет образования", y = "Количество")

# bar plot for the 'ssc_percentage' variable
ggplot(data, aes(x = status, y = ssc_percentage, fill = status)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Распределение процентов SSC по статусу", x = "Статус", y = "Процент SSC")

# bar plot for the 'hsc_percentage' variable
ggplot(data, aes(x = status, y = hsc_percentage, fill = status)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Распределение процентов HSC по статусу", x = "Статус", y = "Процент HSC")


# building scatter plot
# Добавление результатов кластеризации в исходные данные
data$cluster <- as.factor(km_res$cluster)

# Построение scatterplot с окраской точек по кластерам
ggplot(data, aes(x = ssc_percentage, y = hsc_percentage, color = cluster)) +
  geom_point(size = 3) +
  theme_minimal() +
  labs(title = "Scatterplot с окраской по кластерам", x = "Процент SSC", y = "Процент HSC")

# Построение парных scatterplot с использованием ggplot2 и окраской по кластерам
ggpairs(data, columns = 2:6, aes(color = cluster), title = "Парные scatterplot с окраской по кластерам")


# 3d plotting
# Установка пакета scatterplot3d, если он еще не установлен
if (!require("scatterplot3d")) install.packages("scatterplot3d")

# Загрузка библиотеки
library(scatterplot3d)

# Построение трехмерного scatterplot
colors <- c("#999999", "#E69F00", "#56B4E9")
colors <- colors[as.numeric(data$cluster)]

par(mfrow = c(1, 1))
s3d <- scatterplot3d(data$ssc_percentage, data$hsc_percentage, data$degree_percentage,
                     color = colors, pch = 16, main = "Трехмерная кластеризация кандидатов",
                     xlab = "Процент SSC", ylab = "Процент HSC", zlab = "Процент степени бакалавра")

# Добавление легенды
legend("topright", legend = levels(data$cluster), col = c("#999999", "#E69F00", "#56B4E9"), pch = 16)

# ------------------------------ end of data visualization block



