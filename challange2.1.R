#Get some data via an API. There are millions of providers, that offer API access for free
#and have good documentation about how to query their service. You just have to google them. 
#You can use whatever service you want. For example, you can get data about your listening history
#(spotify), get data about flights (skyscanner) or just check the weather forecast.


#Get data via API
library(rlist)
library(httr)
library(jsonlite)
library(dplyr)
res <- GET("http://api.citybik.es/v2/networks")
cont <- content(res)
cont[["networks"]][[2]][["location"]][["city"]] # I got one specific data
result_lst <- cont$networks
View(result_lst)
purrr::map(result_lst, purrr::pluck, "company",1) %>% unlist()
