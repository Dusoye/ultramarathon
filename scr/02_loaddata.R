# load dataset
raw_data <- fread("./data/TWO_CENTURIES_OF_UM_RACES.csv")

#load country mappings
library(rvest)

url <- "https://en.wikipedia.org/wiki/List_of_IOC_country_codes"

wiki_page <- read_html(url)

ioc_codes <- wiki_page %>%
  html_nodes(xpath='//*[@id="mw-content-text"]/div[1]/table[1]') %>%
  html_table(header = TRUE) %>%
  .[[1]] %>%
  select(code = Code, country = `National Olympic Committee`)
