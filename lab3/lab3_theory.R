
patientID<-c(1, 2, 3, 4)
age<-c(25, 34, 28, 52)
diabetes<-c("Type1", "Type2", "Type1", "Type1")
status<-c("Poor", "Improved", "Excellent", "Poor")
patientdata<-data.frame(patientID, age, diabetes, status)
patientdata

new.1000 <-round(sample((median(age) - IQR(age)):(median (age)+IQR(age)) , 1000, replace=TRUE))
age<-c(age, new.1000)
patientdata<-data.frame(patientID, age, diabetes, status)
View(patientdata)
plot(patientdata$age ~ factor(patientdata$status))
plot(factor(patientdata$status), patientdata$age)

fix(patientdata)

boxplot(patientdata$age)

summary(patientdata$age)

IQR(patientdata$age)

boxplot(chickwts$weight ~ chickwts$feed)
library(help = "datasets")#справка по датасетам


head(chickwts)

res <- boxplot(chickwts$weight ~ chickwts$feed)
res$stats

bxp(res)

chickwts$feed

manager <- c(1, 2, 3, 4, 5)
date <- c("10/24/08", "10/28/08", "10/1/08", "10/12/08", "5/1/09")
country <- c("US", "US", "UK", "UK", "UK")
gender <- c("M", "F", "F", "M", "F")
age <- c(32, 45, 25, 39, 99)
q1 <- c(5, 3, 3, 3, 2)
q2 <- c(4, 5, 5, 3, 2)
q3 <- c(5, 2, 5, 4, 1)
q4 <- c(5, 5, 5, NA, 2)
q5 <- c(5, 5, 2, NA, 1)
leadership <- data.frame(manager, date, country, gender, age, q1, q2, q3, q4, q5, stringsAsFactors=FALSE)
leadership
remove(leadership)

leadership[1:6,]

myvars <- c("q1","q2", "q3", "q4", "q5")
newdata <- leadership[myvars]

newdata <- subset(leadership, age >= 35 | age < 24, select=c(q1,q2,q3,q4))

patientdata = edit(patientdata)

help(read.table)