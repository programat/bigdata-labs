# lab6.R

# libraries needed for the script
required_packages <- c("ggplot2", "GGally", "dplyr", "tidyr", "corrplot", "psych", "factoextra", "cluster", "NbClust", "parameters", "scatterplot3d")

# Function to install and load required packages
install_and_load <- function(packages) {
  for (pkg in packages) {
    if (!require(pkg, character.only = TRUE)) {
      install.packages(pkg, dependencies = TRUE)
      library(pkg, character.only = TRUE)
    }
  }
}

# Install and load all required packages
install_and_load(required_packages)


# Load the data from the CSV file
data <- read.csv("lab6/dataset/Job_Placement_Data.csv")

# Descriptive Analysis Block ------------------------------
# 1. Perform descriptive analysis of the data (additional research is welcome).
summary(data)
describe(data)

# Visualizing the distribution of the data
ggplot(data, aes(x = ssc_percentage)) + geom_histogram(binwidth = 5, fill = "blue", color = "black") + theme_minimal()
ggplot(data, aes(x = hsc_percentage)) + geom_histogram(binwidth = 5, fill = "green", color = "black") + theme_minimal()

# Correlation matrix
cor_matrix <- cor(data[, sapply(data, is.numeric)])
corrplot(cor_matrix, method = "circle")

# Boxplot for identifying outliers
boxplot(data$ssc_percentage, main = "SSC Percentage", col = "orange")
boxplot(data$hsc_percentage, main = "HSC Percentage", col = "purple")

# Pairwise scatter plots
pairs(data[, sapply(data, is.numeric)], main = "Scatterplot Matrix")

# Categorical data analysis
table(data$gender)
ggplot(data, aes(x = gender)) + geom_bar(fill = "lightblue") + theme_minimal()

# Check for normal distribution
shapiro.test(data$ssc_percentage)
qqnorm(data$ssc_percentage)
qqline(data$ssc_percentage, col = "red")

data <- data %>% drop_na()

# ------------------------------ end of Descriptive Analysis Block

# Data Clustering Block ------------------------------
# libraries needed for clustering
library(factoextra)
library(cluster)
library(NbClust)
library(parameters)

# 2. Determine the optimal number of clusters
numeric_columns <- data[, sapply(data, is.numeric)]
# Standardize the data
data_clustering <- scale(numeric_columns)
data_clustering_df <- as.data.frame(data_clustering)

# Elbow method
fviz_nbclust(data_clustering_df, kmeans, method = "wss")

# Silhouette method
fviz_nbclust(data_clustering_df, kmeans, method = "silhouette")

# Gap statistic
gap_stat <- clusGap(data_clustering, FUN = kmeans, nstart = 25, K.max = 10, B = 50)
fviz_gap_stat(gap_stat)

# Consensus-based clustering
n_clust <- n_clusters(data_clustering_df, package = c("easystats", "NbClust", "mclust"))
plot(n_clust)

# 3. Perform hierarchical clustering of your dataset
# Hierarchical clustering
dist_data <- dist(data_clustering)
clust_data <- hclust(dist_data, method = "ward.D2")
plot(clust_data)
rect.hclust(clust_data, k = 3, border = "red")

hcd <- as.dendrogram(clust_data)
par(mfrow = c(3, 1))
plot(cut(hcd, h = 4)$upper, main = "Upper part of the dendrogram")
plot(cut(hcd, h = 4)$lower[[1]], main = "First branch of the lower part")
plot(cut(hcd, h = 4)$lower[[3]], main = "Third branch of the lower part")

# 5. Perform k-means clustering of your dataset
# K-means clustering with 3 clusters
set.seed(123)
km_res <- kmeans(data_clustering, centers = 3, nstart = 25)

# Cluster visualization
fviz_cluster(km_res, data = data_clustering)
fviz_cluster(km_res, data = data_clustering, ellipse.type = "norm")

# ------------------------------ end of Data Clustering Block

# Data Visualization Block ------------------------------
# 4. Create bar plots and boxplots for the groups
# Bar plot for the 'gender' variable
ggplot(data, aes(x = gender)) +
  geom_bar(fill = "lightblue") +
  theme_minimal() +
  labs(title = "Gender Distribution", x = "Gender", y = "Count")

# Bar plot for the 'ssc_board' variable
ggplot(data, aes(x = ssc_board)) +
  geom_bar(fill = "lightgreen") +
  theme_minimal() +
  labs(title = "SSC Board Distribution", x = "SSC Board", y = "Count")

# Boxplot for the 'ssc_percentage' variable
ggplot(data, aes(x = status, y = ssc_percentage, fill = status)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "SSC Percentage Distribution by Status", x = "Status", y = "SSC Percentage")

# Boxplot for the 'hsc_percentage' variable
ggplot(data, aes(x = status, y = hsc_percentage, fill = status)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "HSC Percentage Distribution by Status", x = "Status", y = "HSC Percentage")

# 6. Create scatter plots
# Adding clustering results to the original data
data$cluster <- as.factor(km_res$cluster)

# Scatterplot with cluster coloring
ggplot(data, aes(x = ssc_percentage, y = hsc_percentage, color = cluster)) +
  geom_point(size = 3) +
  theme_minimal() +
  labs(title = "Scatterplot with Cluster Coloring", x = "SSC Percentage", y = "HSC Percentage")

# Pairwise scatter plots with cluster coloring
ggpairs(data, columns = 2:6, aes(color = cluster), title = "Pairwise Scatterplot with Cluster Coloring")

# 7. Create 3D scatter plot
# 3D scatter plot
colors <- c("#999999", "#E69F00", "#56B4E9")
colors <- colors[as.numeric(data$cluster)]

par(mfrow = c(1, 1))
s3d <- scatterplot3d(data$ssc_percentage, data$hsc_percentage, data$degree_percentage,
                     color = colors, pch = 16, main = "3D Clustering of Candidates",
                     xlab = "SSC Percentage", ylab = "HSC Percentage", zlab = "Degree Percentage")

# Adding legend
legend("topright", legend = levels(data$cluster), col = c("#999999", "#E69F00", "#56B4E9"), pch = 16)

# ------------------------------ end of Data Visualization Block
