# lab6_2.R

# libraries needed for the script
required_packages <- c("factoextra", "cluster", "dplyr", "party", "e1071", "randomForest")

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

# Data Preprocessing Block ------------------------------
# Convert categorical variables to factors
data$gender <- as.factor(data$gender)
data$ssc_board <- as.factor(data$ssc_board)
data$hsc_board <- as.factor(data$hsc_board)
data$hsc_subject <- as.factor(data$hsc_subject)
data$undergrad_degree <- as.factor(data$undergrad_degree)
data$work_experience <- as.factor(data$work_experience)
data$specialisation <- as.factor(data$specialisation)
data$status <- as.factor(data$status)

# Select only numeric columns for clustering
numeric_columns <- data[, sapply(data, is.numeric)]

# Standardize the data
data_clustering <- scale(numeric_columns)

# ------------------------------ end of Data Preprocessing Block

# Clustering Block ------------------------------
# K-means clustering with 3 clusters
set.seed(123)
km_res <- kmeans(data_clustering, centers = 3, nstart = 25)

# Add clustering results to the original data
data$cluster <- as.factor(km_res$cluster)

# ------------------------------ end of Clustering Block

# Data Splitting Block ------------------------------
# Split the data into training and testing sets
set.seed(1234)
ind <- sample(2, nrow(data), replace = TRUE, prob = c(0.7, 0.3))
trainData <- data[ind == 1, ]
testData <- data[ind == 2, ]

# ------------------------------ end of Data Splitting Block

# Model Training and Evaluation Block ------------------------------
# Naive Bayes Classifier
naive_bayes_model <- naiveBayes(cluster ~ ., data = trainData)
predictions_nb <- predict(naive_bayes_model, testData)
accuracy_nb <- mean(predictions_nb == testData$cluster)
print(paste("Naive Bayes Classifier Accuracy:", round(accuracy_nb * 100, 2), "%"))

# Decision Trees
myFormula <- cluster ~ .
iris_ctree <- ctree(myFormula, data = trainData)
predictions_ctree <- predict(iris_ctree, newdata = testData)
accuracy_ctree <- mean(predictions_ctree == testData$cluster)
print(paste("Decision Tree Accuracy:", round(accuracy_ctree * 100, 2), "%"))
plot(iris_ctree)

# Random Forest
rf_model <- randomForest(cluster ~ ., data = trainData, ntree = 100, proximity = TRUE)
predictions_rf <- predict(rf_model, newdata = testData)
accuracy_rf <- mean(predictions_rf == testData$cluster)
print(paste("Random Forest Accuracy:", round(accuracy_rf * 100, 2), "%"))
print(rf_model)

# ------------------------------ end of Model Training and Evaluation Block
