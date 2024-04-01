# Load the data from a CSV file
data <- read.csv(file='lab3/countries_invest.csv', header=TRUE)

# Display summary statistics of the data
summary(data)

# Set the number of plots per page
par(mfrow = c(3, 5)) # This can be changed depending on the number of countries

# Loop through each country in the data
for (i in 2:ncol(data)) {
  country <- names(data)[i]

  # Plot a histogram for the current country
  hist(data[[country]], main=paste("Distribution for", country), xlab="Rating")
}

# Reset the number of plots per page
par(mfrow = c(1, 1))

# Boxplots for identifying outliers
boxplot(data$Russia, main="Boxplot for Russia", ylab="Rating")
boxplot(data$USA, main="Boxplot for USA", ylab="Rating")
# Continue for the rest of the countries

# Scatter plots
plot(data$Russia, data$USA, xlab="Russia", ylab="USA", main="Russia vs USA")
plot(data$Japan, data$Norway, xlab="Russia", ylab="USA", main="Russia vs USA")
plot(data$China, data$Taiwan, xlab="Russia", ylab="USA", main="Russia vs USA")
# Continue for the rest of the countries

# Scatterplot matrix
pairs(~ Russia + USA + China + Germany + UK,
      data = data,
      main = "Scatterplot Matrix",
      pch = 19)

# Compute the correlation matrix
cor_matrix <- cor(data[,2:ncol(data)], use = "complete.obs")

# Print the correlation matrix
print(cor_matrix)

# Wilcoxon test
print(pairwise.wilcox.test(stack(data)$value, stack(data)$variable,
                     p.adjust.method = "holm"))

# Kruskal-Wallis test
kruskal.test(value ~ variable, data = reshape2::melt(data[,2:ncol(data)]))

# Pairwise comparisons using Wilcoxon test
pairwise.wilcox.test(reshape2::melt(data[,2:ncol(data)])$value,
                     reshape2::melt(data[,2:ncol(data)])$variable,
                     p.adjust.method = "BH",
                     exact = FALSE)

# Initialize an empty data frame
results <- data.frame()

# Get all unique values from the data
all_values <- sort(unique(unlist(data[,-1])))

# Loop through each country in the data
for (i in 2:ncol(data)) {
  country <- names(data)[i]
  # Convert the data to a factor and set the levels
  data_factor <- factor(data[[country]], levels = all_values)
  # Use the table function to count unique values
  temp <- as.vector(table(data_factor))
  # Initialize or append the results to the data frame
  if (i == 2) {
    results <- data.frame(temp)
  } else {
    results <- cbind(results, temp)
  }
  # Set the column name according to the country name
  colnames(results)[ncol(results)] <- country
}

# Print the results
print(results)

# Sort the data by Russia in descending order
data_sorted <- data[order(data$Russia, decreasing = TRUE),]

# Sort the data by multiple countries
data_sorted <- data[order(data$Russia, data$USA, data$Germany, data$UK, data$China),]

# Print the top 10 rows of the sorted data
head(data_sorted, 10)

# Create separate data sets for each country (rating > 7)
data_russia <- data_sorted[data_sorted$Russia > 7, ]
data_usa <- data_sorted[data_sorted$USA > 7, ]
data_germany <- data_sorted[data_sorted$Germany > 7, ]
data_uk <- data_sorted[data_sorted$UK > 7, ]
data_china <- data_sorted[data_sorted$China > 7, ]

# Print the number of rows in each data set
nrow(data_russia)
nrow(data_usa)
nrow(data_germany)
nrow(data_uk)
nrow(data_china)

# Analyze the new data sets
# Histograms
par(mfrow = c(2, 5))
hist(data_russia$Russia, main = "Russia (rating > 7)", xlab = "Rating")
hist(data_usa$USA, main = "USA (rating > 7)", xlab = "Rating")
hist(data_germany$Germany, main = "Germany (rating > 7)", xlab = "Rating")
hist(data_uk$UK, main = "UK (rating > 7)", xlab = "Rating")
hist(data_china$China, main = "China (rating > 7)", xlab = "Rating")

# Boxplots
boxplot(data_russia$Russia, main = "Russia (rating > 7)", ylab = "Rating")
boxplot(data_usa$USA, main = "USA (rating > 7)", ylab = "Rating")
boxplot(data_germany$Germany, main = "Germany (rating > 7)", ylab = "Rating")
boxplot(data_uk$UK, main = "UK (rating > 7)", ylab = "Rating")
boxplot(data_china$China, main = "China (rating > 7)", ylab = "Rating")

# Summary statistics
summary(data_russia$Russia)
summary(data_usa$USA)
summary(data_germany$Germany)
summary(data_uk$UK)
summary(data_china$China)