# Create a data frame with specific values
data <- data.frame(matrix(c(
  1, 7, 5, 9, 5, 10, 8, 5, 3, 7, 7, 9, 6, 9, 7,
  10, 1, 10, 1, 8, 1, 10, 10, 9, 10, 9, 1, 2, 9, 1,
  7, 3, NA, 7, 2, 8, 10, 2, 8, 9, 7, 7, 4, 6, 5,
  5, 3, 9, 9, 4, 6, 10, 8, 2, 10, 10, 8, 9, 10, 9,
  2, 7, 9, 9, 4, 5, 10, 10, 6, 5, 7, 5, 4, 8, 8,
  4, 4, 8, 7, 2, 5, 7, 3, 7, 8, NA, 6, 4, 3, 6,
  6, 7, 10, 4, 7, 7, 3, 9, 9, 3, 9, 5, NA, 4, 5,
  10, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
  10, 5, 10, 1, 1, 3, 10, 1, 1, 6, 5, 8, 1, 7, 10,
  NA, 10, 1, 10, 1, 6, 5, 1, NA, 4, 10, 10, 3, 10, 10,
  10, 1, 8, 1, 1, 2, 3, 1, 1, 4, 1, 1, 1, 1, 1,
  3, 8, 8, 8, 3, 6, 10, NA, 2, 1, NA, NA, NA, 5, 5,
  2, 7, 4, 5, 5, 8, 7, 2, 4, 6, 7, 9, 6, 9, 8,
  NA, NA, 10, NA, NA, 8, 9, 1, NA, 9, 10, NA, NA, 8, NA,
  3, 6, 7, 10, 6, 9, 7, 3, 5, 5, 5, NA, 9, 9, 8,
  NA, 10, 1, 10, 3, 10, 10, 1, 1, 1, 6, 10, 10, 10, 10,
  3, 4, 7, 9, 3, 7, 7, 4, 5, 9, 9, 7, 6, 7, 7,
  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
  10, NA, NA, 6, NA, NA, NA, NA, NA, NA, 5, NA, NA, NA, NA
), ncol = 15, byrow = TRUE))

# Check if the file "personal_data.R" exists
if (file.exists("personal_data.R")) {
  # If it exists, source it
  source("personal_data.R")
} else {
  # If it doesn't exist, create a vector of respondent names
  respondents <- NULL
  for (i in 1:19) {
    respondents <- c(respondents, paste("Respondent", i))
  }
}
# Assign the respondent names as row names of the data frame
rownames(data) <- respondents

# Convert the data frame to a table with specified column names
colnames(data) <- c("Россия", "США", "Китай", "Германия", "Мексика",
                    "Великобритания", "Япония", "Индия", "ЮАР", "ОАЭ",
                    "Сингапур", "Канада", "Тайвань", "Норвегия", "Нидерланды")
# Convert the data to integer type
data <- apply(data, 2, as.integer)
# Sort the data by column names
data <- data[, order(colnames(data))]

# Write the data to a CSV file
write.csv(data, "../lab3/countries_invest.csv", fileEncoding = "UTF-8", row.names = TRUE)

# Calculate the number of survey participants
num_participants <- nrow(data)

# Calculate the maximum, minimum, and mean for each column
max_values <- apply(data, 2, max, na.rm = TRUE)
min_values <- apply(data, 2, min, na.rm = TRUE)
mean_values <- apply(data, 2, mean, na.rm = TRUE)

# Count the number of people with a preference >0.7 and <0.3
preference_high <- colSums(data > 7, na.rm = TRUE)
preference_low <- colSums(data < 3, na.rm = TRUE)

# Print the results
cat("Максимальные значения по столбцам:", max_values, "\n")
cat("Минимальные значения по столбцам:", min_values, "\n")
cat("Средние значения по столбцам:", mean_values, "\n")
cat("Количество людей с предпочтением >0.7:", preference_high, "\n")
cat("Количество людей с предпочтением <0.3:", preference_low, "\n")

# Calculate the country rating in descending order
rating <- sort(rowMeans(data, na.rm = TRUE), decreasing = TRUE)
cat("Рейтинг предпочтений (по убыванию):\n")
for (i in seq_along(rating)) {
  cat(i, ": ", rating[i], "\n")
}

# Create a bar plot of the average ratings by country
barplot(colMeans(data, na.rm = TRUE), main = "Распределение оценок по странам",
        xlab = "Столбцы", ylab = "Средняя оценка", col = "skyblue")

# Create a table with the described data

# Create names for the new rows
row_names <- c(seq_len(nrow(data)), "Max", "Min", "Mean", ">0.7", "<0.3")

# Convert the old rows (1-30) to integer, new rows (Max, Min, Mean, >0.7, <0.3) to float
data_summary <- as.data.frame(rbind(data, max_values, min_values, mean_values, preference_high, preference_low))

# Label the new rows
rownames(data_summary) <- row_names

# Print the results
print(data_summary)