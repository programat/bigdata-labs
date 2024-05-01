library(dplyr)

# Load data
winter_data <- read.csv(file="lab4/winter.csv", stringsAsFactors = FALSE)

countries <- c("NOR", "SWE", "FIN", "DEN", "ISL", "USA", "CAN", "URS", "RUS")

# Filter data
skiing_data <- winter_data %>%
  filter(Sport == "Skiing", Year >= 1984, Year <= 2014, Country %in% countries)

medals_by_year <- skiing_data %>%
  group_by(Year, Medal) %>%
  summarise(Count = n())

medal_counts <- table(skiing_data$Year, skiing_data$Medal)

# Convert table to data frame
medal_df <- as.data.frame.matrix(medal_counts)

colors <- c("Gold" = "gold", "Silver" = "gray", "Bronze" = "brown")

par(mfrow = c(1, 1))
# Build bar chart
barplot(t(medal_df),
        main = "Finland's medals in skiing by year",
        xlab = "Year", ylab = "Number of medals",
        col = colors[colnames(medal_df)],
        legend.text = c("Gold", "Silver", "Bronze"),
        args.legend = list(x = "topright", inset = c(0.05, 0.05)),
        beside = TRUE)

fin_gold <- skiing_data[skiing_data$Medal == "Gold", ]
gold_by_year <- table(fin_gold$Year)

pie(gold_by_year, main = "Finland's first places in skiing by year",
    col = rainbow(length(gold_by_year)), labels = names(gold_by_year))

fin_data_by_gender <- table(skiing_data$Year, skiing_data$Gender)

matplot(as.numeric(rownames(fin_data_by_gender)), fin_data_by_gender, type = "b",
        main = "Trends in Finland's prize-winning places in skiing by gender (1984-2014)",
        xlab = "Year", ylab = "Number of prize-winning places", col = c("blue", "red"), lty = 1:2, pch=1)
legend("topleft", legend = c("Men", "Women"), col = c("blue", "red"), lty = 1:2, bty = "n")


# 4.9.3

# Filter data for the last 6 Olympics
last_6_olympics <- winter_data %>%
  filter(Year >= 1992 & Year <= 2014 & Country %in% countries)

# Count the number of gold medals by country and year
gold_medals <- last_6_olympics %>%
  filter(Medal == "Gold") %>%
  group_by(Year, Country) %>%
  summarise(Gold = n())

# Plot the number of gold medals
plot(NULL, xlim = range(gold_medals$Year), ylim = c(0, max(gold_medals$Gold)),
     xlab = "Year", ylab = "Number of gold medals", main = "Gold medals by country")
colors <- rainbow(length(unique(gold_medals$Country)))
for (i in 1:length(unique(gold_medals$Country))) {
  country <- unique(gold_medals$Country)[i]
  country_data <- gold_medals[gold_medals$Country == country,]
  lines(country_data$Year, country_data$Gold, col = colors[i], type = "o", pch = i)
}
legend("topleft", legend = unique(gold_medals$Country), col = colors, pch = 1:length(unique(gold_medals$Country)), lty = 1)


# Count the number of prize-winning places by country and year
medals <- last_6_olympics %>%
  group_by(Year, Country) %>%
  summarise(Medals = n())

# Plot the number of prize-winning places
plot(NULL, xlim = range(medals$Year), ylim = c(0, max(medals$Medals)),
     xlab = "Year", ylab = "Number of prize-winning places", main = "Prize-winning places by country")
colors <- rainbow(length(unique(medals$Country)))
for (i in 1:length(unique(medals$Country))) {
  country <- unique(medals$Country)[i]
  country_data <- medals[medals$Country == country,]
  lines(country_data$Year, country_data$Medals, col = colors[i], type = "o", pch = i)
}
legend("topleft", legend = unique(medals$Country), col = colors, pch = 1:length(unique(medals$Country)), lty = 1)


# 4.9.4

# Count the number of medals by country, year, and gender
medals_by_gender <- last_6_olympics %>%
  group_by(Year, Gender) %>%
  summarise(Medals = n())

# Plot the functional graph
plot(NULL, xlim = range(medals_by_gender$Year), ylim = c(0, max(medals_by_gender$Medals)),
     xlab = "Year", ylab = "Number of medals", main = "Dynamics of medals in skiing")
colors <- c("blue", "red")
for (i in 1:length(unique(medals_by_gender$Gender))) {
  gender <- unique(medals_by_gender$Gender)[i]
  gender_data <- medals_by_gender[medals_by_gender$Gender == gender,]
  lines(gender_data$Year, gender_data$Medals, col = colors[i], type = "o", pch = i)
}
legend("topleft", legend = c("Men", "Women"), col = colors, pch = 1:2, lty = 1)

# Count the number of medals by country and gender
medals_by_country_gender <- last_6_olympics %>%
  group_by(Country, Gender) %>%
  summarise(Medals = n())

# Plot the bar chart
barplot(t(spread(medals_by_country_gender, Gender, Medals)[,-1]),
        beside = TRUE, col = c("lightblue", "pink"),
        main = "Medals in skiing by country and gender",
        xlab = "Country", ylab = "Number of medals",
        names.arg = unique(medals_by_country_gender$Country),
        legend.text = c("Men", "Women"))

# Count the number of medals by country
medals_by_country <- last_6_olympics %>%
  group_by(Country) %>%
  summarise(Medals = n())

# Plot the pie chart
pie(medals_by_country$Medals, labels = medals_by_country$Country,
    main = "Distribution of medals in skiing by country",
    col = rainbow(length(medals_by_country$Country)))


par(mfrow = c(2, 2))
# Functional graph
plot(NULL, xlim = range(medals_by_gender$Year), ylim = c(0, max(medals_by_gender$Medals)),
     xlab = "Year", ylab = "Number of medals", main = "Dynamics of medals in skiing")
for (i in 1:length(unique(medals_by_gender$Gender))) {
  gender <- unique(medals_by_gender$Gender)[i]
  gender_data <- medals_by_gender[medals_by_gender$Gender == gender,]
  lines(gender_data$Year, gender_data$Medals, col = colors[i], type = "o", pch = i)
}
legend("topleft", legend = c("Men", "Women"), col = colors, pch = 1:2, lty = 1)

# Bar chart
barplot(t(spread(medals_by_country_gender, Gender, Medals)[,-1]),
        beside = TRUE, col = colors,
        main = "Medals in skiing by country and gender",
        xlab = "Country", ylab = "Number of medals",
        names.arg = unique(medals_by_country_gender$Country),
        legend.text = c("Men", "Women"))

# Pie chart
pie(medals_by_country$Medals, labels = medals_by_country$Country,
    main = "Distribution of medals in skiing by country",
    col = rainbow(length(medals_by_country$Country)))


# Count the number of medals by country and gender
medals_by_gender <- aggregate(Medal ~ Country + Year + Gender, data = skiing_data, FUN = length)

# Plot the dynamics of medals by country and gender
plot(Medal ~ Year, data = medals_by_gender, type = "n",
     main = "Dynamics of medals in skiing by country and gender",
     xlab = "Year", ylab = "Number of medals")
colors <- rainbow(length(unique(medals_by_gender$Country)))
legend_labels <- character(length(unique(medals_by_gender$Country)))
for (i in seq_along(unique(medals_by_gender$Country))) {
  country <- unique(medals_by_gender$Country)[i]
  gender_data <- subset(medals_by_gender, Country == country)
  lines(Medal ~ Year, data = subset(gender_data, Gender == "Men"), col = colors[i], lty = 1)
  lines(Medal ~ Year, data = subset(gender_data, Gender == "Women"), col = colors[i], lty = 2)
  legend_labels[i] <- paste(country, "(M/W)")
}
legend("topleft", legend = legend_labels, col = colors, lty = 1:2, ncol = 2)

par(mfrow = c(1, 1))
# Function to count the number of places by country and year
count_medals <- function(data, place) {
  table(data$Year[data$Medal == place], data$Country[data$Medal == place])
}

# Pie chart for the number of first places in each Olympics
gold_counts <- count_medals(skiing_data, "Gold")
pie_data <- colSums(gold_counts)
pie(pie_data, labels = names(pie_data), col = rainbow(length(pie_data)),
    main = "Number of gold medals by country")