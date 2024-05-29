# Подключаем библиотеку igraph
library(igraph)

# Задаем значение N (номер варианта)
N <- 7

# Генерируем случайное число вершин G_size
G_size <- sample(17:61, 1)

# Создаем кольцевой граф g
g <- graph.ring(G_size)

# Выводим количество вершин и ребер
cat("Количество вершин:", vcount(g), "\n")
cat("Количество ребер:", ecount(g), "\n")

# Строим граф
plot(g)

# Выводим матрицу смежности
print(g[])



# Создаем пустой граф g1 с G_size вершинами
g1 <- graph.empty(n = G_size)

# Устанавливаем желтый цвет вершинам
V(g1)$color <- "yellow"

# Добавляем N*8 (56) случайных красных ребер
random_edges <- sample(V(g1), size = 2 * N * 8, replace = TRUE)
random_edges <- matrix(random_edges, ncol = 2, byrow = TRUE)
g1 <- add_edges(g1, random_edges)
E(g1)[(ecount(g1) - N * 8 + 1):ecount(g1)]$color <- "red"

# Визуализируем граф g1 и выводим его матрицу смежности
plot(g1)
print(g1[])

# Добавляем N*10 (70) случайных синих ребер
random_edges <- sample(V(g1), size = 2 * N * 10, replace = TRUE)
random_edges <- matrix(random_edges, ncol = 2, byrow = TRUE)
g1 <- add_edges(g1, random_edges)
E(g1)[(ecount(g1) - N * 10 + 1):ecount(g1)]$color <- "blue"

# Визуализируем граф g1 и выводим его матрицу смежности
plot(g1)
print(g1[])


# --- Шаг 3 ---
# Проверяем существование вершин
vertices_to_check <- c(2*N+23, 2*N+20, 2*N+12, N+15, 2*N-1, N+8, 2*N, 2*N+1, N+7, N+13)
existing_vertices <- vertices_to_check[vertices_to_check %in% V(g1)]

# Добавляем ребра между существующими вершинами
edges_to_add <- matrix(c(
  2*N+23, 2*N+20,
  2*N+12, N+15,
  2*N-1, N+8,
  2*N, 2*N+1,
  N+7, N+13
), ncol = 2, byrow = TRUE)
edges_to_add <- edges_to_add[apply(edges_to_add, 1, function(x) all(x %in% existing_vertices)), ]
g1 <- add_edges(g1, edges_to_add)
E(g1)[(ecount(g1) - nrow(edges_to_add) + 1):ecount(g1)]$color <- "black"

# Визуализируем граф
plot(g1)

# Анализ графа
cat("Соседи вершины", N, ":", neighbors(g1, N), "\n")
cat("Ребра, инцидентные вершине", N, ":", incident(g1, N), "\n")
cat("Вершины", N+10, "и", N+12, "связаны:", are_adjacent(g1, N+10, N+12), "\n")
print(g1[])


# --- Шаг 4 ---
# Находим вершину с максимальной степенью
max_degree_vertex <- which.max(degree(g1))

# Добавляем новую вершину и соединяем с вершиной максимальной степени
g1 <- add_vertices(g1, 1)
g1 <- add_edges(g1, c(vcount(g1), max_degree_vertex))

# Присваиваем имена вершинам
vertex_names <- c(LETTERS, letters)[1:vcount(g1)]
V(g1)$name <- vertex_names

# Выводим матрицу смежности
print(g1[])

# Находим вершины со степенью от 2 до 5
vertices_with_degree <- which(degree(g1) >= 2 & degree(g1) <= 5)
cat("Вершины со степенью от 2 до 5:", vertices_with_degree, "\n")


# --- Шаг 5 ---

# 1. Алгоритм "в круге" (in_circle)
layout_circle <- layout_in_circle(g1)
plot(g1, layout=layout_circle, main="Алгоритм 'в круге'")

# 2. Алгоритм "дерево" (in_tree)
layout_tree <- layout_as_tree(g1)
plot(g1, layout=layout_tree, main="Алгоритм 'дерево'")

# 3. Алгоритм "решетка" (lattice)
layout_grid <- layout_on_grid(g1)
plot(g1, layout=layout_grid, main="Алгоритм 'решетка'")

# --- Шаг 6 ---
# Вычисляем диаметр графа g1
diameter <- diameter(g1)
cat("Диаметр графа g1:", diameter, "\n")

# Визуализируем граф с размером вершин, пропорциональным их степени
vertex_degrees <- degree(g1)
V(g1)$size <- vertex_degrees * 3
plot(g1, layout=layout_with_fr(g1)) # Можешь выбрать любой понравившийся алгоритм размещения