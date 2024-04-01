x <- c(174, 162, 188, 192, 165, 168, 172) # данные о росте семи сотрудников небольшой компании
str(x)

pol <- c("male", "female", "male", "male", "female", "male", "male") # формируем вектор "пол" для сотрудников фирмы

is.character(pol)
is.factor(pol)
is.vector(pol)
str(pol)
table(pol)
pol.f <- factor(pol)
is.factor(pol.f)
pol.f
ls.str()

#Вектор веса
w <- c(69, 68, 93, 87, 59, 82, 72)
#Построение графика
plot(x, w, pch = as.numeric(pol.f), col = as.numeric(pol.f)) # x–возраст, см. выше
legend("topleft", pch = 1:2, col = 1:2, legend = levels(pol.f))

plot(x, w, pch = (7:8), col = c("magenta", "green"))
legend("topleft", pch = 7:8, col = c("magenta", "green"), legend = levels(pol.f))

m <- c("L", "S", "XL", "XXL", "S", "M", "L")
m.f <- factor(m)
m.f

m.o <- ordered(m.f, levels = c("S", "M", "L", "XL", "XXL"))
m.o

names(w) <- c("Коля", "Женя", "Петя", "Саша", "Катя", "Вася", "Жора")
d <- data.frame(weight = w, height = x, size = m.o, pol = pol.f)

d[, order(colnames(d))]
