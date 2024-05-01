num <- c(1:10)
square <- num * num
plot(num, square, type = "b")

opar <- par(no.readonly = TRUE)
par(lty=2, pch=17)
plot(num, square, type = "b")
par(opar)

plot(num, square, type = "b", lty = 2, pch = 17)

n <- 10
mycolors <- rainbow(n)
pie(rep(1, n), labels =mycolors, col = mycolors)
mygrays <- gray(0:n/n)
pie(rep(1, n), labels = mygrays, col = mygrays)

par(mfrow = c(1,1))
# Training with function arguments
dune <- c(20, 7, 70)
labels <- c("Dune", "Shadow side of the dune", "Sky")
colors <- c(rgb(0.79, 0.6, 0.32),
            rgb(0.3, 0.2, 0.14),
            rgb(0.23, 0.32, 0.51))
pie(dune, labels = labels, main = "Dune Composition", init.angle = 220, col=colors, border=NA)
