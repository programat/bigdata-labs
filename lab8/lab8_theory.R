# lab8_theory.R

# correalation

cor(5:15, 7:17)
cor(5:15, c(7:16, 23))
cor(5:15, c(7:16, 23), method = "spearman")

# task 8.2.1
library("psych")
library("corrplot")
library("ggplot2")
# lets check the longley dataset

# amount of variables in the dataset
length(longley)
# amout of rows in the dataset
nrow(longley)
describe(longley)

cor_matrix <- cor(longley)
corrplot(cor_matrix, method = "circle", addCoef.col = "white", tl.col = "black", tl.srt = 45)

pairs(longley[, sapply(longley, is.numeric)], main = "Scatterplot Matrix", col="4")

pairs.panels(longley[, sapply(longley, is.numeric)],
             method = "pearson", # correlation method
             hist.col = "#00AFBB", # color of the histogram
             density = TRUE,  # show density plots
             ellipses = TRUE, # show ellipses of bivariate normality
             stars = TRUE,    # show significance stars
             main = "Scatterplot Matrix with Correlation Coefficients")

# lets test the normality of the data
shapiro_results <- lapply(longley, shapiro.test)
print(shapiro_results)

par(mfrow = c(2, 1))
for(column in colnames(longley)) {
  hist(longley[[column]], main = paste("Histogram for", column), xlab = column, col = "lightblue")
  qqnorm(longley[[column]], main = paste("QQ Plot for", column))
  qqline(longley[[column]])
}


# task 8.2.2
# random numbers in the exponential distribution
x <- rexp(50)
cor(x, log(x), method = "pearson") # this method is not suitable for the exponential distribution
cor(x, log(x), method = "spearman")


# check the hipotesis about the equality of 0 to the correlation coefficient
x <- rnorm(50)
y <- rnorm(50, mean = 2, sd = 1)
cor.test(x, y)

cor.test(x, 2 * x)


# task 8.3 some methods of the correlation visualization

par(mfrow = c(1, 1))
symnum(cor(longley))

if (!require("ellipse")) install.packages("ellipse")
library("ellipse")
plotcorr(cor(longley), col=2, type="full", mar=c(0, 0, 3, 0), main="Correlation plot")


# task 8.4 regression analysis

fit <- lm(weight ~ height, data = women)
fit
summary(fit)
fitted(fit)
residuals(fit)

plot(women$height,women$weight, xlab="Рост (дюймы)",
     ylab="Вес(фунты)",col="blue"); abline(fit, col="red")



fit2 <- lm(weight ~ height + I(height^2), data=women)
fit2
summary(fit2)
plot(women$height,women$weight,xlab="Рост (дюймы)", ylab="Вес(фунты)",col="10");
lines(women$height,fitted(fit2), col="20")


states <- as.data.frame(state.x77[,c("Murder", "Population",
                                     "Illiteracy", "Income", "Frost")])
states
cor(states)

if (!require('car')) install.packages('car')
library('car')

scatterplotMatrix(states, spread=FALSE, lty.smooth=2, main="Матрица диаграмм рассеяния")

fit <- lm(Murder ~ Population + Illiteracy + Income + Frost, data=states)

summary(fit)



