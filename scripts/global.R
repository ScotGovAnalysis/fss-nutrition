library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(shinyWidgets)
library(purrr)
library(shinyBS)
library(readxl)
library(plotly)
library(tidyverse)
library(janitor)
library(htmlwidgets)
library(lubridate)
library(scales)
library(viridis)
library(sgplot)


# read in theme and modules

source("scripts/dashboard_theme.R")
modules <- list.files("modules/", full.names = T)
map(modules, source)

# read in data


# overall data of nutritional volume, spend, kcal and nutritional info for each year and each SIMD category
overall <- read_excel("data/Kantar data 2019-22.xlsx", sheet = "Overall - Purchase") %>% 
  mutate(across(where(is.character), trimws))

# promotype data: nutritional volume, spend, kcal for each year and each SIMD category and promotion type

promotype <- read_excel("data/Kantar data 2019-22.xlsx", sheet = "Overall - PromotypeSIMD") %>% 
  mutate(across(where(is.character), trimws))

# online data: nutritional volume, spend, kcal for each year purchased online by retailer type
online <-  read_excel("data/Kantar data 2019-22.xlsx", sheet = "Overall - OnlineChannels") %>% 
  mutate(across(where(is.character), trimws))

# population data 
pop <- read_excel("data/Kantar data 2019-22.xlsx", sheet = "Population")

# calculate the volume and spend per person per day 
totals_pppd <- overall %>% 
  filter(SIMD == "Total Household") %>% select(-c(Channels, SIMD, `Promotion type`)) %>%pivot_longer(Spend:`Sodium g`) %>%
  left_join(pop, by = c("Year" = "year")) %>%
  mutate(value_pppd = value/population/365)%>%
  select(-c(value, population)) %>%
  pivot_wider(names_from = name, values_from = value_pppd) %>%
  select(-c(ends_with("%"), Trips))

# designate F&D categories that are classified as discretionary

discretionary <- c("Confectionery",
                   "Cakes",
                   "Sweet Biscuits",
                   "Ice Cream & Edible Ices",
                   "Puddings & Desserts",
                   "Crisps & Savoury Snacks",
                   "Regular Soft Drinks (exc. Water)")

# designate F&D categories that are additional categories of interest

additional <- c("Breakfast Cereals",
                "Yoghurt & Fromage Frais",
                "Pizza",
                "Ready Meals",
                "Roast & Processed Potatoes")

alcohol <- "Alcoholic Drinks"
veg <- "Vegetables"
meat <- "Total Meat"

# category data: annual spend, nutritional vol, kcal and nutritional components for select F&D categories and SIMD categories
category <- read_excel("data/Kantar data 2019-22.xlsx", sheet = "F&D - Purchase") %>% 
  mutate(across(where(is.character), trimws)) %>%
  mutate(food_groups = case_when(
    `F&D Category` %in% discretionary ~ "Discretionary categories",
    `F&D Category` %in% additional ~ "Additional categories", 
    `F&D Category` %in% meat ~ "Total Meat",   
    `F&D Category` %in% veg ~ "Vegetables",   
    `F&D Category` %in% alcohol ~ "Alcoholic drinks",   
    TRUE ~ "Other"
  )) 

# category data: annual spend, nutritional vol, kcal and nutritional components for select F&D categories and SIMD categories

category_promo <- read_excel("data/Kantar data 2019-22.xlsx", sheet = "F&D - Promotype") %>% 
  mutate(across(where(is.character), trimws)) %>%
  mutate(food_groups = case_when(
    `F&D Category` %in% discretionary ~ "Discretionary categories",
    `F&D Category` %in% additional ~ "Additional categories", 
    `F&D Category` %in% meat ~ "Total Meat",   
    `F&D Category` %in% veg ~ "Vegetables",   
    `F&D Category` %in% alcohol ~ "Alcoholic drinks",   
    TRUE ~ "Other"
  ))


# data manipulation for category chart builder 1 to sum up all variables for 2019-2022

# calculate totals for nutrional vol, spend for period 2019-2022
yearly_totals <- category_promo %>% 
  select(Year, `F&D Category`, Promotype, `Nutritional Volume`, Spend) %>%
  filter(Promotype %in% c("TOTAL MARKET")) %>%
  select(Year, `Nutritional Volume`, `F&D Category`, Spend) %>%
  group_by(`F&D Category`) %>%
  bind_rows(summarise(., across(where(is.numeric), sum), 
                      across(where(is.character), ~'Annual'))) %>%
  mutate(Year = as.character(Year), 
         Year = str_replace_all(Year, "8082", "2019-2022")) %>% ungroup()

# calculate totals for nutrional vol, spend for period 2019-2022 on promotion
on_promo <- category_promo %>%
  select(Year, `F&D Category`, Promotype, `Nutritional Volume`) %>%
  filter(Promotype %in% c("On Promotion")) %>%
  select(Year, `Nutritional Volume`, `F&D Category`) %>%
  group_by(`F&D Category`) %>%
  bind_rows(summarise(., across(where(is.numeric), sum), 
                      across(where(is.character), ~'Annual'))) %>%
  mutate(Year = as.character(Year), 
         Year = str_replace_all(Year, "8082", "2019-2022")) %>%
  rename("Nutritional volume on promotion" = `Nutritional Volume`)

# calculate % for nutritional vol, spend for period 2019-2022 on promotion

category_promo_totals <- yearly_totals %>%
  left_join(on_promo, by = c("Year", "F&D Category")) %>%
  mutate(pctg_price_promo = round(`Nutritional volume on promotion`/`Nutritional Volume` * 100, 1)) %>%
  left_join((yearly_totals %>% filter(`F&D Category` == "Total Food & Drink") %>% select(-`F&D Category`)), by = "Year") %>%
  mutate(pctg_total_fd = `Nutritional Volume.x`/`Nutritional Volume.y` *100,
         pctg_spend = Spend.x/Spend.y * 100) %>%
  select(-c(`Nutritional Volume.y`, `Nutritional volume on promotion`)) %>%
  rename(NutritionalVolume = `Nutritional Volume.x`)

# simd per year
x <- category %>%
  group_by(SIMD) %>%
  summarise(simd_nutritional_vol = sum(`Nutritional Volume`))


category_simd <- category %>%
  group_by(`F&D Category`, SIMD, food_groups) %>%
  summarise(cat_nutritional_vol = sum(`Nutritional Volume`)) %>% 
  left_join(x, by = "SIMD") %>%
  mutate(pctg_nut_vol = cat_nutritional_vol/simd_nutritional_vol * 100) %>%
  mutate(Year = "2019-2022") %>%
  rename(`Nutritional Volume %` = pctg_nut_vol) %>%
  bind_rows(category %>% mutate(Year = as.character(Year))) 


  
