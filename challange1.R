#
#Analysis with R
library(tidyverse)
library(readxl)
bikes_tbl <- read_excel("00_data/01_bike_sales/01_raw_data/bikes.xlsx")
orderlines_tbl <- read_excel("00_data/01_bike_sales/01_raw_data/orderlines.xlsx")
bikeshops_tbl <-  read_excel("00_data/01_bike_sales/01_raw_data/bikeshops.xlsx")


bike_orderlines_joined_tbl <- orderlines_tbl %>%
  left_join(bikes_tbl, by = c("product.id" = "bike.id")) %>%
  left_join(bikeshops_tbl, by = c("customer.id" = "bikeshop.id"))



 bike_orderlines_wrangled_tbl <- bike_orderlines_joined_tbl %>%
 separate(col    = category,
           into   = c("category.1", "category.2", "category.3"),
           sep    = " - ") %>% 
   mutate(total.price = price * quantity) %>% 
  select(-...1, -gender) %>%
  select(-ends_with(".id")) %>%
  bind_cols(bike_orderlines_joined_tbl %>% select(order.id)) %>% 
  select(order.id, contains("order"), contains("model"),contains("category"), price, quantity, total.price,everything())  %>% 
   rename(bikeshops = name) %>%
     set_names(names(.) %>% str_replace_all("\\.", "_"))


#bike_orderlines_wrangled_tbl 
#glimpse(bike_orderlines_joined_tbl)

#library(lubridate)

#Step 1
#sales_by_year_tbl  <- bike_orderlines_wrangled_tbl %>%
#  select(order_date,total_price) %>%
 # mutate(year = year(order_date)) %>%
 # group_by(year) %>% 
#  summarize(sales = sum(total_price)) %>%
#  mutate(sales_text = scales::dollar(sales, big.mark = ".", 
#                                     decimal.mark = ",", prefix = "", suffix = " €"))
#sales_by_year_tbl

#step 2  
#sales_by_year_tbl %>% 
 # ggplot(aes(x = year, y = sales)) +
 # geom_col(fill = "#2DC6D6") + 
 # geom_label(aes(label = sales_text)) +
 # geom_smooth(method = "lm", se = FALSE) +
 # scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
#                                                    decimal.mark = ",", 
##                                                    prefix = "", 
 #                                                   suffix = " €")) +
 # labs(
   # title    = "Revenue by year",
  #  subtitle = "Upward Trend",
  #  x = "", # Override defaults for x and y
  #  y = "Revenue"
 # )

#Sales by category: Step 1
#sales_by_year_cat_1_tbl <- bike_orderlines_wrangled_tbl %>%
#  select(order_date, total_price, category_1) %>%
 # mutate(year = year(order_date)) %>% 
 # group_by(year, category_1)  %>%
 # summarise(sales = sum(total_price)) %>%
 # ungroup() %>% 
 # mutate(sales_text = scales::dollar(sales, big.mark = ".", 
   #                                  decimal.mark = ",", 
    #                                 prefix = "", 
     #                                suffix = " €"))
#sales_by_year_cat_1_tbl 


#step 2

#sales_by_year_cat_1_tbl %>% 
 # ggplot(aes(x = year, y = sales, fill = category_1)) +
  #geom_col() + 
 # geom_smooth(method = "lm", se = FALSE) +
#  facet_wrap(~ category_1) +
#  scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
 #                                                   decimal.mark = ",", 
  #                                                  prefix = "", 
 #                                                  suffix = " €")) 

#labs(
#  title = "Revenue by year and main category",
#  subtitle = "Each product category has an upward trend",
#  fill = "Main category" # Changes the legend name
#)




#Chanllange1 
library(tidyverse) 
library(readxl)
library(lubridate)
#first step -> state and city are multiple features (variables), they should be split
bike_loc_wrangled_tbl <- bike_orderlines_joined_tbl %>%  
  separate(col = location, 
           into = c("state", "city"),
           sep  = ",")  %>% 
  mutate(total.price = price * quantity) %>%
  rename(bikeshop = name) %>%
  set_names(names(.) %>% str_replace_all("\\.", "_"))


 

 #second step
sales_by_locYear_tbl  <- bike_loc_wrangled_tbl  %>% 
  select(state, city, order_date, total_price) %>% 
  mutate(year = year(order_date)) %>%
  group_by(year,state,city) %>%
  summarise(sales = sum(total_price)) %>%
  ungroup() %>%
  mutate(sales_text = scales::dollar(sales, big.mark = ".", 
                                     decimal.mark = ",", 
                                     prefix = "", 
                                     suffix = " €")) 

# Ploting the data
#{r plot, fig.width=10, fig.height=7}
sales_by_locYear_tbl %>% 
  ggplot(aes(x = city, y = year)) + 
  geom_col(fill = "#2DC6D6") +
  geom_label(aes(label = sales_text)) +
  geom_smooth(method = "lm", se = FALSE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 