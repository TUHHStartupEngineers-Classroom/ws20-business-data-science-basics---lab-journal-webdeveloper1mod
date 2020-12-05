#Challange4.2
#Goal: Visualize the distribution of the mortality rate (deaths / population) with geom_map(). 
#The necessary longitudinal and lateral data can be accessed with this function:
#world <- map_data("world")

library(ggplot2)
library(ggrepel)
library(tidyverse)
library(lubridate)
require(scales)
install.packages("ggrepel")

country_data_3 <- covid_data_tbl %>%
  select(countriesAndTerritories,deaths,popData2019) %>%
  mutate(across(countriesAndTerritories, str_replace_all, "_", " ")) %>%
  mutate(countriesAndTerritories = case_when(
    countriesAndTerritories == "United Kingdom" ~ "UK",
    countriesAndTerritories == "United States of America" ~ "USA",
    countriesAndTerritories == "Czechia" ~ "Czech Republic",
    TRUE ~ countriesAndTerritories
  )) %>%
  group_by(countriesAndTerritories,popData2019)  %>%
  summarize(overall_deaths = sum(deaths)) %>%
  mutate(mortality = overall_deaths / popData2019) %>%
  rename(region = countriesAndTerritories)


  
world <- map_data("world")

combined_covid_data <- merge(x = world, y = country_data_3, 
                             by    = "region", 
                             all.x = TRUE, 
                             all.y = FALSE)
 world <- map_data("world")
maximum <- max(combined_covid_data$mortality, na.rm = TRUE)
combined_covid_data %>% 
  ggplot() +
  geom_map(aes(x=long,y=lat,map_id=region, fill = mortality),map = world)+
  scale_fill_gradient(low="#ffcccb", high="#940008",labels = percent,limits = c(0,maximum),breaks=c(0, 0.0005, 0.001,maximum)) +
  theme(axis.ticks = element_blank(),axis.title = element_blank(),axis.text = element_blank())+
  labs(
    title = "Confirmed COVID-19 deaths relative to the size of the population",
    subtitle = "More than 1.2 Million confirmed COVID-19 deaths worldwide",
    fill = "Mortality Rate",
    caption = str_glue("Date: 12/02/2020"))





  
  