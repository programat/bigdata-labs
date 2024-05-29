# lab8.R

# libraries needed for the script
required_packages <- c("psych", "ggplot2", "corrplot", "dplyr", "car", "tidyr")

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

# Load the data for Serbia
serbia_data <- read.csv("lab8/dataset/World-Development-Indicators-database_Data.csv", header = TRUE, sep = ",", dec = ".")
serbia_data <- serbia_data %>% filter(`Country.Name` == "Serbia")

# Descriptive Analysis Block ------------------------------
# 1. Perform descriptive analysis of the data (additional research is welcome).

numeric_data <- serbia_data %>% select(-`Country.Name`, -`Country.Code`, -`Series.Name`, -`Series.Code`)

# Remove unnecessary columns
serbia_data <- serbia_data[, -c(1, 2, 4)]

# Transpose the table for convenience
serbia_data <- as.data.frame(t(serbia_data))
colnames(serbia_data) <- c("GDP_current_USD", "GDP_growth_annual", "Births_attended_skilled_health_staff",
                           "Birth_rate_crude", "Adjusted_net_national_income_growth", "Unemployment_advanced_ed",
                           "Unemployment_basic_ed", "Trained_teachers_secondary_female", "Trained_teachers_upper_secondary",
                           "Imports_goods_services_GDP", "Industry_value_added_growth", "Health_expenditure_per_capita",
                           "Life_expectancy_birth", "Population_growth_annual", "Gov_expenditure_education_GDP",
                           "Goods_imports_BoP", "Exports_goods_services_growth", "Death_rate_crude",
                           "Educational_attainment_Bachelors_total", "Educational_attainment_Bachelors_female",
                           "High_tech_exports", "Medium_high_tech_Industry", "Scientific_articles")
serbia_data <- serbia_data[-1,]
rownames(serbia_data) <- 1989:2018

# Convert data to numeric format
serbia_data <- sapply(serbia_data, as.numeric)
serbia_data <- as.data.frame(serbia_data)

summary(numeric_data)
describe(numeric_data)

# Convert data to "long" format
serbia_data_long <- serbia_data %>%
  pivot_longer(cols = everything(), names_to = "Indicator", values_to = "Value")

# Histograms
ggplot(serbia_data_long, aes(x = Value)) +
  geom_histogram(fill = "blue", color = "black") +
  facet_wrap(~Indicator, scales = "free") +
  theme_minimal() +
  labs(title = "Histograms of Indicators for Serbia")

# Boxplots
ggplot(serbia_data_long, aes(x = Indicator, y = Value)) +
  geom_boxplot(fill = "lightblue") +
  theme_minimal() +
  labs(title = "Boxplots of Indicators for Serbia")

# ------------------------------ end of Descriptive Analysis Block

# Correlation Analysis Block ------------------------------
# 2a. GDP growth and population growth
cor(serbia_data$`GDP_growth_annual`, serbia_data$`Population_growth_annual`, use = "complete.obs")
ggplot(serbia_data, aes(x = `Population_growth_annual`, y = `GDP_growth_annual`)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "red") +
  theme_minimal() +
  labs(title = "Correlation between GDP Growth and Population Growth in Serbia",
       x = "Population Growth (annual %)",
       y = "GDP Growth (annual %)")

# 2b. Population growth and unemployment dynamics
cor(serbia_data$`Population_growth_annual`, serbia_data$`Unemployment_advanced_ed`, use = "complete.obs")
cor(serbia_data$`Population_growth_annual`, serbia_data$`Unemployment_basic_ed`, use = "complete.obs")

ggplot(serbia_data, aes(x = `Population_growth_annual`, y = `Unemployment_advanced_ed`)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "red") +
  theme_minimal() +
  labs(title = "Correlation between Population Growth and Unemployment (Advanced Education)",
       x = "Population Growth (annual %)",
       y = "Unemployment (Advanced Education)")

ggplot(serbia_data, aes(x = `Population_growth_annual`, y = `Unemployment_basic_ed`)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "red") +
  theme_minimal() +
  labs(title = "Correlation between Population Growth and Unemployment (Basic Education)",
       x = "Population Growth (annual %)",
       y = "Unemployment (Basic Education)")

# 2c. Health expenditure, life expectancy, and mortality
cor(serbia_data$`Health_expenditure_per_capita`, serbia_data$`Life_expectancy_birth`, use = "complete.obs")
cor(serbia_data$`Health_expenditure_per_capita`, serbia_data$`Death_rate_crude`, use = "complete.obs")

ggplot(serbia_data, aes(x = `Health_expenditure_per_capita`, y = `Life_expectancy_birth`)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "red") +
  theme_minimal() +
  labs(title = "Correlation between Health Expenditure and Life Expectancy",
       x = "Health Expenditure per Capita (current US$)",
       y = "Life Expectancy at Birth (years)")

ggplot(serbia_data, aes(x = `Health_expenditure_per_capita`, y = `Death_rate_crude`)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "red") +
  theme_minimal() +
  labs(title = "Correlation between Health Expenditure and Death Rate",
       x = "Health Expenditure per Capita (current US$)",
       y = "Death Rate (per 1,000 people)")

# 2d. Growth in higher education, export growth, and high-tech industry growth
serbia_data$`Higher_Education_Growth` <- c(NA, diff(serbia_data$`Educational_attainment_Bachelors_total`))
cor(serbia_data$`Higher_Education_Growth`, serbia_data$`Exports_goods_services_growth`, use = "complete.obs")
cor(serbia_data$`Higher_Education_Growth`, serbia_data$`Medium_high_tech_Industry`, use = "complete.obs")

ggplot(serbia_data, aes(x = `Higher_Education_Growth`, y = `Exports_goods_services_growth`)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "red") +
  theme_minimal() +
  labs(title = "Correlation between Higher Education Growth and Exports Growth",
       x = "Higher Education Growth (annual %)",
       y = "Exports of goods and services (annual % growth)")

ggplot(serbia_data, aes(x = `Higher_Education_Growth`, y = `Medium_high_tech_Industry`)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "red") +
  theme_minimal() +
  labs(title = "Correlation between Higher Education Growth and High-Tech Industry Growth",
       x = "Higher Education Growth (annual %)",
       y = "Medium and high-tech Industry Growth (annual %)")

# 2e. Education expenditure and cumulative growth of female bachelor's degree holders
cor(serbia_data$`Gov_expenditure_education_GDP`, serbia_data$`Educational_attainment_Bachelors_female`, use = "complete.obs")

ggplot(serbia_data, aes(x = `Gov_expenditure_education_GDP`, y = `Educational_attainment_Bachelors_female`)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "red") +
  theme_minimal() +
  labs(title = "Correlation between Education Expenditure and Female Bachelor's Attainment",
       x = "Government expenditure on education, total (% of GDP)",
       y = "Educational attainment, at least Bachelor's or equivalent, population 25+, female (%) (cumulative)")

# 2f. Growth in higher education and development of high technology (growth in scientific journal articles)
cor(serbia_data$`Higher_Education_Growth`, serbia_data$`Scientific_articles`, use = "complete.obs")

ggplot(serbia_data, aes(x = `Higher_Education_Growth`, y = `Scientific_articles`)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "red") +
  theme_minimal() +
  labs(title = "Correlation between Higher Education Growth and Scientific Articles",
       x = "Higher Education Growth (annual %)",
       y = "Scientific and technical journal articles")

# ------------------------------ end of Correlation Analysis Block

# Regression Analysis Block ------------------------------
# 2g. Regression analysis
# Choose dependent and independent variables for analysis
# Example:
fit <- lm(GDP_growth_annual ~ Population_growth_annual + Gov_expenditure_education_GDP, data = serbia_data)
summary(fit)

# ------------------------------ end of Regression Analysis Block

# Prediction Block ------------------------------
# 2h. Prediction
# Use the predict() function for the chosen model
# Example:
new_data <- data.frame(Population_growth_annual = c(0.5, 1, 1.5),
                       Gov_expenditure_education_GDP = c(4, 4.5, 5))
predict(fit, newdata = new_data)

# ------------------------------ end of Prediction Block
