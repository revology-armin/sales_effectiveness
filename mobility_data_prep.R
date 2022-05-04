# revologyanalytics.com -------------------------------------------------


library(tidyverse)
library(janitor)
library(anytime)
library(lubridate)
library(zoo)
library(data.table)
library(corrr)
library(broom)

data = read_csv('C:/Users/armin/OneDrive/Documents/GitHub/sales_effectiveness/apple-mobility-trends-report.csv',
             col_names = T)



data = data %>% clean_names() %>% mutate_if(is.logical, as.double)


data_2 = data %>% pivot_longer(
            cols = starts_with("x2"),
            names_prefix = "x",
            names_to = "date",
            values_to = "direction_requests") %>%
             mutate(date = anydate(date),
                    year = year(date),
                    month = month(date))



data_3 = data_2 %>% filter(country == "United States" & geo_type == "sub-region") %>%
     mutate(date = as.Date(paste(year, month, 1, sep = "-"))) %>%
      group_by(transportation_type, country, date) %>%
  summarise(direction_requests = mean(direction_requests, na.rm = T)) %>% ungroup() 





# Pull in government data -------------------------------------------------

## Auto parts sales ----

auto = read_csv("C:/Users/armin/OneDrive/Documents/GitHub/sales_effectiveness/retail_sales_automotive_parts_tires_MM_dollars.csv",
                col_names = T) 

setnames(auto, names(auto), c('date','auto_parts_tires_sales'))

## + Alcohol sales ----
alcoholic_beverage = read_csv("C:/Users/armin/OneDrive/Documents/GitHub/sales_effectiveness/retail_sales_beer_wine_liquor_MM_dollars.csv",
                col_names = T) 

setnames(alcoholic_beverage, names(alcoholic_beverage), c('date','alcoholic_beverage_sales'))

## + Electronics sales ----
electronic_retail = read_csv("C:/Users/armin/OneDrive/Documents/GitHub/sales_effectiveness/retail_sales_electronic_stores_MM_dollars.csv",
                col_names = T) 

setnames(electronic_retail, names(electronic_retail), c('date','electronics_sales'))

## + Grocery sales ----
grocery = read_csv("C:/Users/armin/OneDrive/Documents/GitHub/sales_effectiveness/retail_sales_grocery_stores_MM_dollars.csv",
                col_names = T) 

setnames(grocery, names(grocery), c('date','grocery_sales'))

## + Toy store sales ----
toy = read_csv("C:/Users/armin/OneDrive/Documents/GitHub/sales_effectiveness/retail_sales_hobby_toy_game_stores_MM_dollars.csv",
                col_names = T) 

setnames(toy, names(toy), c('date','toy_store_sales'))

## + Restaurant sales ----
restaurant = read_csv("C:/Users/armin/OneDrive/Documents/GitHub/sales_effectiveness/retail_sales_restaurants_MM_dollars.csv",
                col_names = T) 

setnames(restaurant, names(restaurant), c('date','restaurant_spend'))

## + Sporting Good retail sales ----
sport_retailer = read_csv("C:/Users/armin/OneDrive/Documents/GitHub/sales_effectiveness/retail_sales_sporting_goods_stores_MM_dollars.csv",
                col_names = T) 

setnames(sport_retailer, names(sport_retailer), c('date','sporting_good_sales'))



# Merge mobility and public spending data -------------------------------------------------

tableau_data_set = list(data_3,
                        auto, alcoholic_beverage, electronic_retail, grocery, toy, restaurant, sport_retailer) %>%
                          reduce(left_join, by = 'date') %>% select(-country) %>% na.omit()





# Run simple correlations and regression tests -------------------------------------------------

## + Correlation of mobility and consumer spending ----
cor_data = tableau_data_set %>% 
            group_by(transportation_type) %>%
                      summarise(auto_parts_cor = cor(direction_requests, auto_parts_tires_sales),
                                alcohol_cor = cor(direction_requests, alcoholic_beverage_sales),
                                electronics_cor = cor(direction_requests, electronics_sales),
                                grocery_cor = cor(direction_requests, grocery_sales),
                                toys_cor = cor(direction_requests, toy_store_sales),
                                restaurant_cor = cor(direction_requests, restaurant_spend),
                                sporting_goods_cor = cor(direction_requests, sporting_good_sales))
  
cor_data                              
  
## + Regression of mobility and consumer spending ----  

### **Remember: we are not doing any sort of modeling here. Just understanding high level relationship strength.** ----


## + Simple regression of mobility trends and car part sales ----
auto_parts_sales_reg <- tableau_data_set %>%
  nest(data = -transportation_type) %>% 
  mutate(
    fit = map(data, ~ lm(auto_parts_tires_sales ~ direction_requests, data = .x)),
    tidied = map(fit, tidy),
    glanced = map(fit, glance),
    augmented = map(fit, augment)
  )

auto_parts_sales_reg %>% 
  unnest(tidied) %>% filter(!grepl("intercept", term, ignore.case = T))



## + ~ alcoholic beverage sales ----
alcohol_reg <- tableau_data_set %>%
  nest(data = -transportation_type) %>% 
  mutate(
    fit = map(data, ~ lm(alcoholic_beverage_sales ~ direction_requests, data = .x)),
    tidied = map(fit, tidy),
    glanced = map(fit, glance),
    augmented = map(fit, augment)
  )

alcohol_reg %>% 
  unnest(tidied) %>% filter(!grepl("intercept", term, ignore.case = T))




## + ~ electronics sales ----
electronics_reg <- tableau_data_set %>%
  nest(data = -transportation_type) %>% 
  mutate(
    fit = map(data, ~ lm(electronics_sales ~ direction_requests, data = .x)),
    tidied = map(fit, tidy),
    glanced = map(fit, glance),
    augmented = map(fit, augment)
  )

electronics_reg %>% 
  unnest(tidied) %>% filter(!grepl("intercept", term, ignore.case = T))



## + ~ grocery sales ----
grocery_reg <- tableau_data_set %>%
  nest(data = -transportation_type) %>% 
  mutate(
    fit = map(data, ~ lm(grocery_sales ~ direction_requests, data = .x)),
    tidied = map(fit, tidy),
    glanced = map(fit, glance),
    augmented = map(fit, augment)
  )

grocery_reg %>% 
  unnest(tidied) %>% filter(!grepl("intercept", term, ignore.case = T))




## + ~ toy store sales ----
toys_reg <- tableau_data_set %>%
  nest(data = -transportation_type) %>% 
  mutate(
    fit = map(data, ~ lm(toy_store_sales ~ direction_requests, data = .x)),
    tidied = map(fit, tidy),
    glanced = map(fit, glance),
    augmented = map(fit, augment)
  )

toys_reg %>% 
  unnest(tidied) %>% filter(!grepl("intercept", term, ignore.case = T))



## + ~ restaurant sales ----
restaurant_reg <- tableau_data_set %>%
  nest(data = -transportation_type) %>% 
  mutate(
    fit = map(data, ~ lm(restaurant_spend ~ direction_requests, data = .x)),
    tidied = map(fit, tidy),
    glanced = map(fit, glance),
    augmented = map(fit, augment)
  )

restaurant_reg %>% 
  unnest(tidied) %>% filter(!grepl("intercept", term, ignore.case = T))

                  
## + ~ sporting goods sales ----
sporting_goods_reg <- tableau_data_set %>%
  nest(data = -transportation_type) %>% 
  mutate(
    fit = map(data, ~ lm(sporting_good_sales ~ direction_requests, data = .x)),
    tidied = map(fit, tidy),
    glanced = map(fit, glance),
    augmented = map(fit, augment)
  )

sporting_goods_reg %>% 
  unnest(tidied) %>% filter(!grepl("intercept", term, ignore.case = T))                  
                  
                             
                              
                  
                  
# Write out for Talbeau visualization -------------------------------------------------   


viz_data = tableau_data_set %>% pivot_longer(
  cols = auto_parts_tires_sales:sporting_good_sales,
  names_to = "consumption",
  values_to = "sales_MM_dollars")

write.csv(viz_data, file = 'C:/Users/armin/OneDrive/Documents/GitHub/sales_effectiveness/mobility_spending_tableau.csv', row.names = F)


