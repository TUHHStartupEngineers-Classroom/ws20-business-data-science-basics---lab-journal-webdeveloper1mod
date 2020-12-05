#Scrape one of the competitor websites of canyon (either https://www.rosebikes.de/ or
#https://www.radon-bikes.de) and create a small database. The database should contain the model
#names and prices for at least one category. Use the selectorgadget to get a good understanding of 
#the website structure.
#challange2.2 

# Challange 3.2
library(rvest)
library(httr)
library(tidyverse) # Main Package - Loads dplyr, purrr, etc.
library(rvest)     # HTML Hacking & Web Scraping
library(xopen)     # Quickly opening URLs
library(jsonlite)  # converts JSON files to R objects
library(glue)      # concatenate strings
library(stringi)

url <- "https://www.radon-bikes.de/"
html <- url %>% 
  read_html()

model_tbl <- html %>%
  html_nodes(css = ".m-bikegrid__grid .m-bikegrid__item .a-heading--small") %>%
  html_text("h4") %>%
  enframe(name="model.id",value = "Model Name")

price_tbl <- html %>%
  html_nodes(css = ".m-bikegrid__grid .m-bikegrid__item .m-bikegrid__price--active ") %>%
  html_text('span') %>%
  discard(.p = ~stringr::str_detect(.x,"₤")) %>%
  enframe(name = "price.id", value = "Prices")


new_bike_models_tbl <- left_join(price_tbl, model_tbl, by = c( "price.id" = "model.id" ))