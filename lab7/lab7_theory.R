# lab7_theory.R

# set names for the data frame
names<-c("Коля","Женя","Петя","Саша","Катя","Вася","Жора")
# set the other values for df
weight <- c(69, 68, 93, 87, 59, 87, 72)
height <- c(180, 165, 175, 178, 177, 176, 170)
sex <- c("Male", "Female", "Male", "Male", "Female", "Male", "Male")
salary <- c(21, 19, 27, 11, 102, 25, 21)

# create a data frame
df <- data.frame(names, weight, height, sex, salary)
print(df)


# lets start from the one dimensiaoanal tests, that are used to check the statement that the data is normally distributed

# H0 hypothesis - the data is normally distributed
t.test(salary, mu=32) # that's the Student's t-test (Whilliam Gosset, 1908)


numb <- rnorm(100)
plot(1:100, numb, col="red", pch=19, xlab="Index", ylab="Value", main="Random numbers")
hist(numb, breaks = 10, col="lightblue", main="Histogram of distribution", xlab="Value", ylab="Frequency", border="black")
lines(density(numb), col="blue", lwd=2)
density(numb)

# lets check the normality of the data by the Student's t-test
t.test(numb, mu=mean(numb), conf.int=TRUE)

# lets check the normality of the data by the Wilcoxon signed rank test
wilcox.test(numb, mu=mean(numb), conf.int=TRUE)


# sheck on the normality of the data ---

shapiro.test(salary)
shapiro.test(numb)

set.seed(0)
shapiro.test(rnorm(100, mean=2, sd=5))
set.seed(0)
shapiro.test(runif(100, min=-10, max=10))

qqnorm(numb, col=4)
qqline(numb, col=2)

if (!require("car")) install.packages("car")
library(car)
qqPlot(rnorm(100, mean=2, sd=5))
qqPlot(runif(100, min=-10, max=10))


# lets check the example of the women's day energy consumption from the book "Introductory Statistics with R" by Peter Dalgaard
if (!require("ISwR")) install.packages("ISwR")
library(ISwR)
data(energy)
attach(energy)
energy
tapply(energy$expend, energy$stature, mean)

# Shapiro-Wilk test to check the normality of the data
shapiro.test(energy$expend)

# Barlett's test to check the homogeneity of the variances
bartlett.test(expend ~ stature, data=energy)

# t-test to check the difference between the means
t.test(expend ~ stature, data=energy, var.equal=FALSE)

