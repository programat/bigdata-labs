# 1.8.1
p <- 7:4
q <- 0:3

print(paste('p + q = [', paste(p + q, collapse = ','), ']'))
print(paste('p - q = [', paste(p - q, collapse = ','), ']'))
print(paste('p * q = [', paste(p * q, collapse = ','), ']'))
print(paste('p / q = [', paste(p / q, collapse = ','), ']'))
print(paste('p ^ q = [', paste(p ^ q, collapse = ','), ']'))

# 1.8.2
p <- ifelse(seq_along(0:19) %% 2 == 0, seq(1, 20), 0)
p <- 1:20 * 0:1
print(p)
p <- 2^(1:20)
print(p)
p <- 10^(0:4)
print(p)

# 1.8.3
seq <- 1 / (1:50 * (1:50 + 1))
print(seq)
seq <- 1/ (2^(0:30))
print(seq)
seq <- 1 + (3 * (0:9)) / (3^(0:9))
print(seq)
ans_sum <- sum(seq)
print(ans_sum)

count_greater_than_half <- sum(seq > 0.5)
print(count_greater_than_half)


# 1.8.4
vec3 <- seq(3,27, len=27/3)
vec3[c(2,5,7)]
vec3[length(p)-1]
vec3[-(length(p)-1)]
vec3[-6]
vec3[100]
vec3[-c(1, length(vec3))]
vec3[vec3 > 4 & vec3 < 10]
vec3[vec3 < 4 | vec3 > 10]


# 1.9.6

# Установим начальное значение для воспроизводимости результатов
set.seed(1)

N <- 20
data <- data.frame(Nrow = 1:N,
                   Name = sample(c("Иван", "Мария", "Александр", "Екатерина", "Сергей", "Анна"), N, replace = TRUE),
                   BirthYear = round(runif(N, 1960, 1985)),
                   EmployYear = round(runif(N, 1960, 2006)),
                   Salary = numeric(N))

# Заполним данные для зарплаты в соответствии с условиями
data$Salary <- ifelse(data$BirthYear < 1975,
                      (log(2007 - data$EmployYear) + 1) * 8000,
                      (log2(2007 - data$EmployYear) + 1) * 8000)

# Подсчитаем число сотрудников с зарплатой больше 15000
num_high_salary <- sum(data$Salary > 15000)

# Функция для расчета суммарного подоходного налога
calculate_income_tax <- function(birth_year, employ_year) {
  years_worked <- (employ_year:2006)
  annual_salary <- ifelse(birth_year < 1975,
                          (log(2007 - years_worked) + 1) * 8000,
                          (log2(2007 - years_worked) + 1) * 8000)
  income_tax <- sum(annual_salary * 0.13)
  return(income_tax)
}

# Рассчитаем суммарный подоходный налог для каждого сотрудника
data$IncomeTax <- mapply(calculate_income_tax, data$BirthYear, data$EmployYear)

# Выведем результаты
print(data)
print(paste("Число сотрудников с зарплатой больше 15000: ", num_high_salary))

# 1.9.21

n <- as.integer(readline("Введите число векторов (n): "))
data_types <- c("numeric", "character", "logical", "factor")

# Создание n векторов с случайными значениями и типами данных
vectors <- list()
for (i in 1:n) {
  random_type <- sample(data_types, 1)
  if (random_type == "numeric") {
    vectors[[i]] <- as.numeric(sample(1:100, 5, replace = TRUE))
  } else if (random_type == "character") {
    vectors[[i]] <- as.character(sample(letters, 5, replace = TRUE))
  } else if (random_type == "logical") {
    vectors[[i]] <- as.logical(sample(c(TRUE, FALSE), 5, replace = TRUE))
  } else if (random_type == "factor") {
    vectors[[i]] <- as.factor(sample(c("A", "B", "C"), 5, replace = TRUE))
  }
}

vector_classes <- sapply(vectors, class)
df <- data.frame(vectors)
colnames(df) <- paste("Column", 1:n)
num_rows <- nrow(df)
num_cols <- ncol(df)

print("Классы векторов:")
print(vector_classes)
print("Data.frame:")
print(df)
print(paste("Число строк:", num_rows))
print(paste("Число столбцов:", num_cols))