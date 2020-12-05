#Goal: Map the time course of the cumulative Covid-19 cases!
#Challange4.1


library(tidyverse)
covid_data_tbl <- read_csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv")

glimpse(covid_data_tbl)

filter_list <- c("Spain","United_States_of_America","France","Germany","United_Kingdom")

country_data <- subset(covid_data_tbl, countriesAndTerritories %in% filter_list) 

glimpse(country_data)

country_data_1 <- country_data%>%
  select(countriesAndTerritories,cases,month,year,dateRep) %>%
  mutate(date = lubridate::dmy(dateRep)) %>%
  arrange(date) %>%
  
  #filter(year == 2020)
  group_by(countriesAndTerritories) %>%
  mutate(case_sum = cumsum(cases)) %>%
  ungroup() 


library(tidyverse)
covid_data_tbl <- read_csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv")

glimpse(covid_data_tbl)

filter_list <- c("Spain","United_States_of_America","France","Germany","United_Kingdom")

country_data <- subset(covid_data_tbl, countriesAndTerritories %in% filter_list) 

glimpse(country_data)

country_data_1 <- country_data%>%
  select(countriesAndTerritories,cases,month,year,dateRep) %>%
  mutate(date = lubridate::dmy(dateRep)) %>%
  arrange(date) %>%
  
  #filter(year == 2020)
  group_by(countriesAndTerritories) %>%
  mutate(case_sum = cumsum(cases)) %>%
  ungroup() 
  
  #Data
 country_data_1 %>%
   ggplot() +
   geom_line(aes(x     = date,
                 y     = case_sum,
                 color = countriesAndTerritories)) +
   labs(
     title = "Revenue",
     subtitle = "Sales are trending up and to the right!",
     x = "Year-2020",
     y = "Cumulative Cases",
     color = "Rev (M)")