#Challange3.1

#Chapter4 
library(tidyverse) # Main Package - Loads dplyr, purrr, etc.
library(rvest)     # HTML Hacking & Web Scraping
library(xopen)     # Quickly opening URLs
library(jsonlite)  # converts JSON files to R objects
library(glue)      # concatenate strings
library(stringi)
library(data.table)
library(vroom)


col_types <- list(
  id = col_character(),
  type = col_character(),
  number = col_character(),
  country = col_character(),
  date = col_date("%Y-%m-%d"),
  abstract = col_character(),
  title = col_character(),
  kind = col_character(),
  num_claims = col_double(),
  filename = col_character(),
  withdrawn = col_double()
)


patent_assigne_tbl <- vroom(
  file       = "patent_assignee.tsv", 
  delim      = "\t", 
  col_types  = col_types,
  na         = c("", "NA", "NULL")
)

patent_tbl <- vroom(
  file       = "patento.tsv", 
  delim      = "\t", 
  col_types  = col_types,
  na         = c("", "NA", "NULL")
)


assignee_tbl <- vroom(
  file       = "assignee.tsv", 
  delim      = "\t", 
  col_types  = col_types,
  na         = c("", "NA", "NULL")
)


uspc_tbl <- vroom(
  file       = "uspc.tsv", 
  delim      = "\t", 
  col_types  = col_types,
  na         = c("", "NA", "NULL")
)
library(data.table)

assigneeDT <- setDT(assignee_tbl)
patent_assigneDT <- setDT(patent_assigne_tbl)

main_tbl <- assigneeDT %>%
  left_join( y=patent_assigneDT, by = c("id" = "assignee_id")) 
   

 big_tbl <- main_tbl %>%
 group_by(organization) %>%
   pull(patent_id) %>%
  summarise(count = n()) %>%
   head(10)
 
 
