# lab7.R

# libraries needed for the script
required_packages <- c("psych", "ggplot2", "corrplot", "dplyr", "car")

# Function to install and load required packages
install_and_load <- function(packages) {
  for (pkg in packages) {
    if (!require(pkg, character.only = TRUE)) {
      install.packages(pkg, dependencies = TRUE)
      library(pkg, character.only = TRUE)
    }
  }
}

# Install and load all required packages
install_and_load(required_packages)

# Load the data from the CSV file
data <- read.csv("lab7/dataset/athlete_events.csv")

# Descriptive Analysis Block ------------------------------
# 1. Perform descriptive analysis of the data (additional research is welcome).

summary(data)
describe(data)

# Visualizing the distribution of the data
ggplot(data, aes(x = Age)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  theme_minimal() +
  labs(title = "Age Distribution", x = "Age", y = "Frequency")

ggplot(data, aes(x = Height)) +
  geom_histogram(binwidth = 5, fill = "green", color = "black") +
  theme_minimal() +
  labs(title = "Height Distribution", x = "Height (cm)", y = "Frequency")

ggplot(data, aes(x = Weight)) +
  geom_histogram(binwidth = 5, fill = "red", color = "black") +
  theme_minimal() +
  labs(title = "Weight Distribution", x = "Weight (kg)", y = "Frequency")

boxplot(data$Age, main = "Age", col = "orange")
boxplot(data$Height, main = "Height", col = "purple")
boxplot(data$Weight, main = "Weight", col = "cyan")

pairs(numeric_data, main = "Scatterplot Matrix", col="4")

table(data$Sex)
ggplot(data, aes(x = Sex)) +
  geom_bar(fill = "lightblue") +
  theme_minimal() +
  labs(title = "Gender Distribution", x = "Gender", y = "Frequency")

# ------------------------------ end of Descriptive Analysis Block

# Normality Check Block ------------------------------
# 2. Check normal distribution and dispersion homogeneity.

shapiro.test(data$Weight[sample(1:nrow(data), 5000)])

qqnorm(data$Weight, col = "lightblue")
qqline(data$Weight, col = "red")

leveneTest(Weight ~ Sport, data = data)

# ------------------------------ end of Normality Check Block

# Hypothesis Testing Block ------------------------------
# 3. Check the hypothesis about the average weight of athletes in skiing

# Filter data for skiing
skiing_data <- subset(data, grepl("Skiing", Sport))

# Test hypothesis about the average weight
t.test(skiing_data$Weight, mu = mean(data$Weight, na.rm = TRUE))

# 4. Check the hypothesis about the equality of the average women (men) weight in skiing and hockey

# Filter data for skiing and hockey
skiing_data_female <- subset(data, grepl("Skiing", Sport) & Sex == "F")
hockey_data_female <- subset(data, Sport == "Hockey" & Sex == "F")

# Test hypothesis about the equality of average weight
t.test(skiing_data_female$Weight, hockey_data_female$Weight)

# ------------------------------ end of Hypothesis Testing Block

# Additional Research Block ------------------------------
# Check normality distribution for skiing and hockey
shapiro.test(skiing_data_female$Weight[sample(1:nrow(data), 5000)])
shapiro.test(hockey_data_female$Weight[sample(1:nrow(data), 5000)])

# Check equality of variances for skiing and hockey
bartlett.test(Weight ~ Sport, data = subset(data, grepl("Skiing", Sport) | (Sport == "Hockey" & Sex == "F")))

# ANOVA for skiing and hockey
anova_result <- aov(Weight ~ Sport, data = subset(data, grepl("Skiing", Sport) | (Sport == "Hockey" & Sex == "F")))
summary(anova_result)

# ------------------------------ end of Additional Research Block
