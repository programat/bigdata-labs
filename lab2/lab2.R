# Создание данных
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


if (file.exists("personal_data.R")) {
  source("personal_data.R")
} else {
  respondents <- NULL
  for (i in 1:19) {
    respondents <- c(respondents, paste("Respondent", i))
  }
}
rownames(data) <- respondents

# Преобразование в таблицу с заданными именами столбцов
colnames(data) <- c("Россия", "США", "Китай", "Германия", "Мексика",
                    "Великобритания", "Япония", "Индия", "ЮАР", "ОАЭ",
                    "Сингапур", "Канада", "Тайвань", "Норвегия", "Нидерланды")
data <- apply(data, 2, as.integer)
data <- data[, order(colnames(data))]

write.csv(data, "../lab3/countries_invest.csv", fileEncoding = "UTF-8", row.names = TRUE)

# Количество участников опроса
num_participants <- nrow(data)

# Вычисление максимума, минимума и среднего по каждому столбцу
max_values <- apply(data, 2, max, na.rm = TRUE)
min_values <- apply(data, 2, min, na.rm = TRUE)
mean_values <- apply(data, 2, mean, na.rm = TRUE)


# Подсчет количества людей, отдавших предпочтение >0.7 и <0.3
preference_high <- colSums(data > 7, na.rm = TRUE)
preference_low <- colSums(data < 3, na.rm = TRUE)

# Вывод результатов
cat("Максимальные значения по столбцам:", max_values, "\n")
cat("Минимальные значения по столбцам:", min_values, "\n")
cat("Средние значения по столбцам:", mean_values, "\n")
cat("Количество людей с предпочтением >0.7:", preference_high, "\n")
cat("Количество людей с предпочтением <0.3:", preference_low, "\n")

# Рейтинг страны по убыванию
rating <- sort(rowMeans(data, na.rm = TRUE), decreasing = TRUE)
cat("Рейтинг предпочтений (по убыванию):\n")
for (i in seq_along(rating)) {
  cat(i, ": ", rating[i], "\n")
}

# Построение столбчатой диаграммы
barplot(colMeans(data, na.rm = TRUE), main = "Распределение оценок по странам",
        xlab = "Столбцы", ylab = "Средняя оценка", col = "skyblue")


# Создание таблицы с описанными данными

# Создание названий для новых строк
row_names <- c(seq_len(nrow(data)), "Max", "Min", "Mean", ">0.7", "<0.3")

# Конвертация старых строк (1-30) в integer, новых строк (Max, Min, Mean, >0.7, <0.3) в float
data_summary <- as.data.frame(rbind(data, max_values, min_values, mean_values, preference_high, preference_low))
# data_summary <- type.convert(data_summary, as.is = TRUE)

# Подпись новых строк
rownames(data_summary) <- row_names

# Вывод результатов
print(data_summary)