# Функция для проверки, находится ли клетка в пределах доски
valid_move <- function(x, y, N) {
  return(x >= 1 && x <= N && y >= 1 && y <= N)
}

# Функция для поиска пути для коня
find_path_for_konya <- function(N, start_x, start_y, goal_x, goal_y) {
  # Возможные ходы коня
  horse_jumps <- matrix(c(2, 1, 2, -1, -2, 1, -2, -1, 1, 2, 1, -2, -1, 2, -1, -2), nrow = 8, ncol = 2, byrow = TRUE)

  # Инициализация начальной позиции коня
  queue <- list(c(start_x, start_y))
  visited <- list()

  # Инициализация массива родителей
  parents <- matrix(list(c(NA, NA)), nrow = N, ncol = N)

  # Пока очередь не пуста
  while (length(queue) > 0) {
    # Извлекаем текущую позицию
    current <- queue[[1]]
    queue <- queue[-1]
    x <- current[1]
    y <- current[2]

    # Добавляем текущую позицию в список посещенных
    visited <- append(visited, list(current))

    # Если достигли цели
    if (x == goal_x && y == goal_y) {
      # Восстанавливаем путь
      path <- list(c(goal_x, goal_y))
      while (!(x == start_x && y == start_y)) {
        parent <- parents[[x, y]]
        path <- append(list(parent), path, after = 0)
        x <- parent[1]
        y <- parent[2]
      }
      return(path)
    }

    # Проверяем все возможные ходы
    for (i in 1:8) {
      new_x <- x + horse_jumps[i, 1]
      new_y <- y + horse_jumps[i, 2]

      # Проверяем, не выходит ли конь за пределы доски и не посещал ли он эту клетку ранее
      if (valid_move(new_x, new_y, N) && !(list(c(new_x, new_y)) %in% visited)) {
        # Запоминаем откуда конь пришел
        parents[[new_x, new_y]] <- c(x, y)
        # Добавляем клетку в очередь
        queue <- append(queue, list(c(new_x, new_y)))
      }
    }
  }

  # Если путь не найден
  cat("Путь не найден\n")
  return(NULL)
}

# Пример использования:
N <- 5
start_x <- 1
start_y <- 1
goal_x <- 3
goal_y <- 1
path <- find_path_for_konya(N, start_x, start_y, goal_x, goal_y)

# Выводим результат:
if (is.null(path)) {
  cat("Путь не найден\n")
} else {
  cat("Конь нашел путь за", length(path) - 1, "хода\n")
  cat("Путь:\n")
  for (i in 1:length(path)) {
    cat("(", path[[i]][1], ",", path[[i]][2], ") ", sep = "")
  }
  cat("\n")
}