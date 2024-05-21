# install.packages("rvest")

library(rvest)
library(dplyr)

# URL for the Visit Ithaca website, wineries page

url <- read_html("https://www.kp.ru/russia/tury-po-rossii/gastronomicheskie/")
selector_name <- ".full-card-content>h3>a"

tnames <- html_nodes(url, selector_name) %>%
  html_text() %>%
  as.array();

tnames
selector_a_name <- ".full-card-button"
fnames_addr <- html_nodes(url, selector_a_name) %>% html_attr("href");
fnames_addr
fnames_addr1 <- paste0(fnames_addr, "dostoprimechatelnosti/")
fnames_addr1[2]<-paste0(fnames_addr[2], "#places")