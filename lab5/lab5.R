library(rvest)
library(dplyr)
library(ggplot2)

# Функция для сбора данных за определенный год
get_data <- function(year) {
  url <- paste0("https://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title=", year)
  page <- read_html(url)
  table <- page %>% html_nodes("table") %>% .[[2]] %>% html_table()
  df <- data.frame(table[-1,], row.names = NULL)
  colnames(df) <- c("Rank", "Country", "Quality of Life Index", "Purchasing Power Index",
                    "Safety Index", "Health Care Index", "Cost of Living Index",
                    "Property Price to Income Ratio", "Traffic Commute Time Index",
                    "Pollution Index", "Climate Index")
  cols <- c("Rank", "Quality of Life Index", "Purchasing Power Index", "Safety Index",
            "Health Care Index", "Cost of Living Index", "Property Price to Income Ratio",
            "Traffic Commute Time Index", "Pollution Index", "Climate Index")
  df[cols][df[cols] == "-" | df[cols] == "N/A"] <- NA
  df[cols] <- sapply(df[cols], as.numeric)
  df$Year <- year
  return(df)
}

# Сбор данных за несколько лет
years <- 2014:2021
df_list <- lapply(years, get_data)
df <- do.call(rbind, df_list)

# Фильтрация данных для выбранных стран
countries <- c("Colombia", "China", "Russia", "Mexico", "Brazil")
df_filtered <- df[df$Country %in% countries,]

# Построение графика
ggplot(df_filtered, aes(x=Year, y=`Quality of Life Index`, color=Country, group=Country)) +
  geom_line(linewidth=1) +
  scale_color_manual(values=c("Colombia"="blue", "China"="red", "Russia"="purple",
                              "Mexico"="green", "Brazil"="orange")) +
  labs(title="Quality of Life Index 2014-2021",
       x="Year", y="Quality of Life Index") +
  theme_minimal() +
  theme(plot.title = element_text(size=16, face="bold"),
        legend.title = element_blank(),
        legend.position = "bottom",
        axis.title = element_text(size=12))




scrape_museums <- function(url) {
  # Чтение HTML-кода страницы
  page <- read_html(url)

  # Извлечение блоков с информацией о музеях
  museum_nodes <- html_nodes(page, "a.place")

  # Инициализация векторов для хранения данных
  museum_names <- character()
  museum_urls <- character()
  museum_addresses <- character()

  # Цикл по каждому блоку с информацией о музее
  for (i in 1:length(museum_nodes)) {
    print('scraping in progress...')
    # Извлечение названия музея
    museum_name <- html_node(museum_nodes[i], "h2.name") %>% html_text()
    museum_names <- c(museum_names, museum_name)

    # Извлечение ссылки на страницу музея
    museum_url <- html_attr(museum_nodes[i], "href")
    museum_urls <- c(museum_urls, museum_url)
    print(paste('fetching', museum_url, sep = " "))

    # Переход на страницу музея для получения адреса
    museum_page <- read_html(museum_url)
    address_node <- html_node(museum_page, ".infobox .content p[itemprop='address']")

    if (!is.na(address_node)) {
      museum_address <- html_text(address_node) %>% trimws()
      print(paste('got an adress:', museum_address, sep=" "))
    } else {
      museum_address <- NA
    }

    museum_addresses <- c(museum_addresses, museum_address)
  }

  result <- list(museum_names, museum_urls, museum_addresses)
  return(result)
}

museums_info <- function(result) {
  museum_names <- result[[1]]
  museum_urls <- result[[2]]
  museum_addresses <- result[[3]]

  # Проверка на совпадение длин векторов и обрезка до минимальной длины
  min_length <- min(length(museum_names), length(museum_urls), length(museum_addresses))
  museum_names <- museum_names[1:min_length]
  museum_urls <- museum_urls[1:min_length]
  museum_addresses <- museum_addresses[1:min_length]

  # Создание итогового data.frame
  df <- data.frame(
    name = museum_names,
    url = museum_urls,
    address = museum_addresses,
    stringsAsFactors = FALSE
  )

  return(df)
}

urlx <- "https://kudamoscow.ru/place/museum/"
result <- scrape_museums(urlx)
museums_df <- museums_info(result)
head(museums_df)

