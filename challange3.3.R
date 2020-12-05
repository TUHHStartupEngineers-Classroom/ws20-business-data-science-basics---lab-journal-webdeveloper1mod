#Challange3.3
#Innovation in Tech: What is the most innovative tech sector?
#For the top 10 companies (worldwide) with the most patents, 
#what are the top 5 USPTO tech main classes?
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

assignee_tbl <- vroom(
  file       = "assignee.tsv", 
  delim      = "\t", 
  col_types  = col_types,
  na         = c("", "NA", "NULL")
)

col_types <- list(
  patent_id = col_character(),
  mainclass_id = col_character(),
  sequence = col_double()
)

uspc_tbl <- vroom(
  file       = "uspc.tsv", 
  delim      = "\t", 
  col_types  = col_types,
  na         = c("", "NA", "NULL")
)


uspc_DT <- setDT(uspc_tbl)

assigneeDT <- setDT(assignee_tbl) %>%
    filter(type==2 | type ==3)

patent_assigneDT <- setDT(patent_assigne_tbl)

main_tbl <- assigneeDT %>%
  left_join( y=patent_assigneDT, by = c("id" = "assignee_id")) #join two tables
   
 #First task
 first10_companies <- main_tbl %>% 
  select(organization,patent_id,id) %>%
  group_by(organization,id) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
   head(10)

 
 #second Task
 
 #connection the two tables in order to get access to patent_id
  preparational_tbl <- patent_assigne_tbl %>% 
   left_join(y=first10_companies , by = c("assignee_id" = "id"))

  
  #final selection and counting
  uspc_tbl_final <- uspc_DT %>% 
    select(patent_id,sequence,mainclass_id) %>%
    left_join(preparational_tbl, by="patent_id") %>%
    group_by(mainclass_id) %>%
    summarise(count = n()) %>%
    arrange(desc(count)) %>%
    head(5)
    
  
     
  