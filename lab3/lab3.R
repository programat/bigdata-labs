data <- read.csv(file='lab3/countries_invest.csv', header=TRUE)

# Описательные статистики
summary(data)

# Устанавливаем количество графиков на странице
par(mfrow = c(3, 5)) # Можно изменить в зависимости от количества стран

# Проходим по каждой стране в данных
for (i in 2:ncol(data)) {
  country <- names(data)[i]

  # Строим гистограмму для текущей страны
  hist(data[[country]], main=paste("Распределение для", country), xlab="Оценка")
}

par(mfrow = c(1, 1))
# Боксплоты для выявления выбросов
boxplot(data$Россия, main="Боксплот для России", ylab="Оценка")
boxplot(data$США, main="Боксплот для США", ylab="Оценка")
# и так далее по остальным странам

boxplot(data[,2:ncol(data)], las = 2, main="Боксплоты по странам", xlab = "", ylab = "Оценка")

# Диаграмма рассеяния
plot(data$Россия, data$США, xlab="Россия", ylab="США", main="Россия vs США")
plot(data$Япония, data$Норвегия, xlab="Россия", ylab="США", main="Россия vs США")
plot(data$Китай, data$Тайвань, xlab="Россия", ylab="США", main="Россия vs США")
# и так далее по остальным странам

pairs(~ Россия + США + Китай + Германия + Великобритания,
      data = data,
      main = "Матрица диаграмм рассеяния",
      pch = 19)

# Вычисляем корреляционную матрицу
cor_matrix <- cor(data[,2:ncol(data)], use = "complete.obs")

# Выводим результат
print(cor_matrix)

print(pairwise.wilcox.test(stack(data)$value, stack(data)$variable,
                     p.adjust.method = "holm"))



# Тест Краскела-Уоллиса
kruskal.test(value ~ variable, data = reshape2::melt(data[,2:ncol(data)]))

# Попарные сравнения с помощью теста Вилкоксона
pairwise.wilcox.test(reshape2::melt(data[,2:ncol(data)])$value,
                     reshape2::melt(data[,2:ncol(data)])$variable,
                     p.adjust.method = "BH",
                     exact = FALSE)

results <- data.frame()

# Получаем все уникальные значения из данных
all_values <- sort(unique(unlist(data[,-1])))
results <- data.frame()
# Проходим по каждой стране в данных
for (i in 2:ncol(data)) {
  country <- names(data)[i]
  # Преобразуем данные в фактор и устанавливаем уровни
  data_factor <- factor(data[[country]], levels = all_values)
  # Используем функцию table для подсчета уникальных значений
  temp <- as.vector(table(data_factor))
  # Инициализируем или добавляем результаты в data.frame
  if (i == 2) {
    results <- data.frame(temp)
  } else {
    results <- cbind(results, temp)
  }
  # Устанавливаем имя столбца в соответствии с именем страны
  colnames(results)[ncol(results)] <- country
}

print(results)



data_sorted <- data[order(data$Россия, decreasing = TRUE),]
data_sorted <- data[order(data$Россия, data$США, data$Германия, data$Великобритания, data$Китай),]
head(data_sorted, 10)

# Формирование отдельных наборов данных по странам (оценка > 7)
data_russia <- data_sorted[data_sorted$Россия > 7, ]
data_usa <- data_sorted[data_sorted$США > 7, ]
data_germany <- data_sorted[data_sorted$Германия > 7, ]
data_uk <- data_sorted[data_sorted$Великобритания > 7, ]
data_china <- data_sorted[data_sorted$Китай > 7, ]

# Размерности полученных наборов данных
nrow(data_russia)
nrow(data_usa)
nrow(data_germany)
nrow(data_uk)
nrow(data_china)

# Анализ новых наборов данных
# Гистограммы
par(mfrow = c(2, 5))
hist(data_russia$Россия, main = "Россия (оценка > 7)", xlab = "Оценка")
hist(data_usa$США, main = "США (оценка > 7)", xlab = "Оценка")
hist(data_germany$Германия, main = "Германия (оценка > 7)", xlab = "Оценка")
hist(data_uk$Великобритания, main = "Великобритания (оценка > 7)", xlab = "Оценка")
hist(data_china$Китай, main = "Китай (оценка > 7)", xlab = "Оценка")

# Боксплоты
boxplot(data_russia$Россия, main = "Россия (оценка > 7)", ylab = "Оценка")
boxplot(data_usa$США, main = "США (оценка > 7)", ylab = "Оценка")
boxplot(data_germany$Германия, main = "Германия (оценка > 7)", ylab = "Оценка")
boxplot(data_uk$Великобритания, main = "Великобритания (оценка > 7)", ylab = "Оценка")
boxplot(data_china$Китай, main = "Китай (оценка > 7)", ylab = "Оценка")

# Описательные статистики
summary(data_russia$Россия)
summary(data_usa$США)
summary(data_germany$Германия)
summary(data_uk$Великобритания)
summary(data_china$Китай)